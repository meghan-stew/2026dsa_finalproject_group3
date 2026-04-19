source("Code/utils_pipeline.R")

check_required_packages()
ensure_output_dirs()

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(parsnip)
  library(recipes)
  library(rsample)
  library(tune)
  library(workflows)
})

train_data <- readr::read_rds(project_path("Output", "data", "train_model_data.rds"))

split_data <- make_time_split(train_data)
analysis_data <- split_data$analysis
assessment_data <- split_data$assessment

cv_folds <- rsample::group_vfold_cv(
  analysis_data,
  group = site_year,
  v = 5
)

model_recipe <- recipes::recipe(
  yield_mg_ha ~ hybrid + site + previous_crop + longitude + latitude +
    soilph + om_pct + soilk_ppm + soilp_ppm +
    annual_prcp + annual_gdd + annual_tmax + annual_tmin +
    annual_srad + annual_vp + hot_days + cold_days +
    spring_prcp + spring_tmax + summer_prcp + summer_tmax +
    growing_prcp + growing_gdd + growing_tmax + growing_tmin,
  data = analysis_data
) %>%
  recipes::step_unknown(recipes::all_nominal_predictors()) %>%
  recipes::step_novel(recipes::all_nominal_predictors()) %>%
  recipes::step_impute_median(recipes::all_numeric_predictors()) %>%
  recipes::step_dummy(recipes::all_nominal_predictors()) %>%
  recipes::step_zv(recipes::all_predictors())

max_mtry <- min(25L, ncol(analysis_data) - 2L)

xgb_spec <- parsnip::boost_tree(
  trees = 1000,
  tree_depth = tune::tune(),
  min_n = tune::tune(),
  loss_reduction = tune::tune(),
  sample_size = tune::tune(),
  mtry = tune::tune(),
  learn_rate = tune::tune(),
  stop_iter = 25
) %>%
  parsnip::set_engine("xgboost") %>%
  parsnip::set_mode("regression")

ranger_spec <- parsnip::rand_forest(
  trees = 1000,
  mtry = tune::tune(),
  min_n = tune::tune()
) %>%
  parsnip::set_engine("ranger", importance = "permutation") %>%
  parsnip::set_mode("regression")

xgb_wf <- workflows::workflow() %>%
  workflows::add_recipe(model_recipe) %>%
  workflows::add_model(xgb_spec)

ranger_wf <- workflows::workflow() %>%
  workflows::add_recipe(model_recipe) %>%
  workflows::add_model(ranger_spec)

xgb_params <- tune::parameters(xgb_wf) %>%
  dials::update(mtry = dials::mtry(c(2L, max_mtry)))

ranger_params <- tune::parameters(ranger_wf) %>%
  dials::update(mtry = dials::mtry(c(2L, max_mtry)))

xgb_grid <- dials::grid_latin_hypercube(xgb_params, size = 15)
ranger_grid <- dials::grid_latin_hypercube(ranger_params, size = 15)

metric_set_reg <- yardstick::metric_set(yardstick::rmse, yardstick::rsq, yardstick::mae)
ctrl <- tune::control_grid(save_pred = TRUE, verbose = TRUE)

xgb_tuned <- tune::tune_grid(
  xgb_wf,
  resamples = cv_folds,
  grid = xgb_grid,
  metrics = metric_set_reg,
  control = ctrl
)

ranger_tuned <- tune::tune_grid(
  ranger_wf,
  resamples = cv_folds,
  grid = ranger_grid,
  metrics = metric_set_reg,
  control = ctrl
)

xgb_best <- tune::select_best(xgb_tuned, metric = "rmse")
ranger_best <- tune::select_best(ranger_tuned, metric = "rmse")

xgb_fit <- parsnip::fit(tune::finalize_workflow(xgb_wf, xgb_best), data = analysis_data)
ranger_fit <- parsnip::fit(tune::finalize_workflow(ranger_wf, ranger_best), data = analysis_data)

xgb_assessment <- assessment_data %>%
  dplyr::select(yield_mg_ha, year, site, hybrid) %>%
  dplyr::bind_cols(predict(xgb_fit, new_data = assessment_data)) %>%
  dplyr::mutate(model = "xgboost")

ranger_assessment <- assessment_data %>%
  dplyr::select(yield_mg_ha, year, site, hybrid) %>%
  dplyr::bind_cols(predict(ranger_fit, new_data = assessment_data)) %>%
  dplyr::mutate(model = "ranger")

assessment_predictions <- dplyr::bind_rows(xgb_assessment, ranger_assessment)

assessment_metrics <- assessment_predictions %>%
  dplyr::group_by(model) %>%
  dplyr::group_modify(~ fit_metrics(.x)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(assessment_year = split_data$assessment_year)

champion_model <- assessment_metrics %>%
  dplyr::filter(.metric == "rmse") %>%
  dplyr::arrange(.estimate) %>%
  dplyr::slice(1) %>%
  dplyr::pull(model)

champion_fit <- if (identical(champion_model, "xgboost")) xgb_fit else ranger_fit

importance_tbl <- dplyr::bind_rows(
  extract_importance(xgb_fit, "xgboost"),
  extract_importance(ranger_fit, "ranger")
)

performance_plot <- assessment_predictions %>%
  ggplot2::ggplot(ggplot2::aes(x = yield_mg_ha, y = .pred, color = model)) +
  ggplot2::geom_point(alpha = 0.35) +
  ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2) +
  ggplot2::labs(
    title = paste("Observed vs Predicted Yield:", split_data$assessment_year),
    x = "Observed yield (Mg/ha)",
    y = "Predicted yield (Mg/ha)"
  ) +
  ggplot2::theme_minimal()

importance_plot <- importance_tbl %>%
  dplyr::filter(model == champion_model) %>%
  ggplot2::ggplot(ggplot2::aes(x = reorder(Variable, Importance), y = Importance)) +
  ggplot2::geom_col(fill = "#2f6b2f") +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = paste("Top Variables:", champion_model),
    x = NULL,
    y = "Importance"
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  filename = project_path("Output", "figures", "assessment_pred_vs_obs.png"),
  plot = performance_plot,
  width = 9,
  height = 6,
  dpi = 300
)

ggplot2::ggsave(
  filename = project_path("Output", "figures", "champion_variable_importance.png"),
  plot = importance_plot,
  width = 9,
  height = 6,
  dpi = 300
)

readr::write_rds(xgb_fit, project_path("Output", "models", "xgboost_fit.rds"))
readr::write_rds(ranger_fit, project_path("Output", "models", "ranger_fit.rds"))
readr::write_rds(champion_fit, project_path("Output", "models", "champion_fit.rds"))
readr::write_csv(assessment_metrics, project_path("Output", "metrics", "assessment_metrics.csv"))
readr::write_csv(assessment_predictions, project_path("Output", "metrics", "assessment_predictions.csv"))
readr::write_csv(importance_tbl, project_path("Output", "metrics", "variable_importance.csv"))
readr::write_lines(champion_model, project_path("Output", "models", "champion_model.txt"))
readr::write_rds(train_data, project_path("Output", "app", "train_data_for_app.rds"))

message("Training complete. Outputs saved to Output/models, Output/metrics, and Output/figures.")
