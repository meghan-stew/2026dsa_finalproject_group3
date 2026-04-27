required_packages <- c(
  "dplyr",
  "ggplot2",
  "parsnip",
  "plotly",
  "purrr",
  "readr",
  "recipes",
  "rsample",
  "shiny",
  "stringr",
  "tibble",
  "tidyr",
  "tune",
  "vip",
  "workflows",
  "yardstick",
  "xgboost",
  "ranger"
)

check_required_packages <- function(packages = required_packages) {
  missing_packages <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing_packages) > 0) {
    stop(
      paste(
        "Install these packages before running the pipeline:",
        paste(missing_packages, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

project_root <- function() {
  root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

  if (file.exists(file.path(root, "2026dsa_finalproject_group3.Rproj"))) {
    return(root)
  }

  parent <- normalizePath(file.path(root, ".."), winslash = "/", mustWork = FALSE)

  if (file.exists(file.path(parent, "2026dsa_finalproject_group3.Rproj"))) {
    return(parent)
  }

  stop("Run this script from the project root or from the Code folder.", call. = FALSE)
}

project_path <- function(...) {
  file.path(project_root(), ...)
}

ensure_output_dirs <- function() {
  output_dirs <- c(
    project_path("Output", "data"),
    project_path("Output", "figures"),
    project_path("Output", "metrics"),
    project_path("Output", "models"),
    project_path("Output", "app")
  )

  purrr::walk(output_dirs, dir.create, recursive = TRUE, showWarnings = FALSE)
}

read_training_tables <- function() {
  trait <- readr::read_csv(
    project_path("Data", "training", "training_trait.csv"),
    show_col_types = FALSE
  ) %>%
    dplyr::rename_with(tolower)

  meta <- readr::read_csv(
    project_path("Data", "training", "training_meta.csv"),
    show_col_types = FALSE
  ) %>%
    dplyr::rename_with(tolower)

  soil <- readr::read_csv(
    project_path("Data", "training", "training_soil.csv"),
    show_col_types = FALSE
  ) %>%
    dplyr::rename_with(tolower) %>%
    tidyr::separate(site, into = c("site", "soil_year"), sep = "_") %>%
    dplyr::mutate(year = as.integer(soil_year)) %>%
    dplyr::select(-soil_year)

  weather <- readr::read_csv(
    project_path("Data", "weather_daily.csv"),
    show_col_types = FALSE
  ) %>%
    dplyr::rename_with(tolower)

  list(trait = trait, meta = meta, soil = soil, weather = weather)
}

read_testing_tables <- function() {
  submission <- readr::read_csv(
    project_path("Data", "testing", "testing_submission.csv"),
    show_col_types = FALSE
  ) %>%
    dplyr::rename_with(tolower)

  meta <- readr::read_csv(
    project_path("Data", "testing", "testing_meta.csv"),
    show_col_types = FALSE
  ) %>%
    dplyr::rename_with(tolower)

  soil <- readr::read_csv(
    project_path("Data", "testing", "testing_soil.csv"),
    show_col_types = FALSE
  ) %>%
    dplyr::rename_with(tolower) %>%
    tidyr::separate(site, into = c("site", "soil_year"), sep = "_") %>%
    dplyr::mutate(year = as.integer(soil_year)) %>%
    dplyr::select(-soil_year)

  weather <- readr::read_csv(
    project_path("Data", "weather_daily_test.csv"),
    show_col_types = FALSE
  ) %>%
    dplyr::rename_with(tolower)

  list(submission = submission, meta = meta, soil = soil, weather = weather)
}

summarize_weather <- function(weather_df) {
  weather_df %>%
    dplyr::mutate(
      gdd_base10 = pmax(((tmax + tmin) / 2) - 10, 0),
      hot_day = as.integer(tmax >= 30),
      cold_day = as.integer(tmin <= 0),
      spring = yday >= 60 & yday <= 151,
      summer = yday >= 152 & yday <= 243,
      growing = yday >= 91 & yday <= 273
    ) %>%
    dplyr::group_by(site, year) %>%
    dplyr::summarise(
      annual_prcp = sum(prcp, na.rm = TRUE),
      annual_gdd = sum(gdd_base10, na.rm = TRUE),
      annual_tmax = mean(tmax, na.rm = TRUE),
      annual_tmin = mean(tmin, na.rm = TRUE),
      annual_srad = mean(srad, na.rm = TRUE),
      annual_vp = mean(vp, na.rm = TRUE),
      hot_days = sum(hot_day, na.rm = TRUE),
      cold_days = sum(cold_day, na.rm = TRUE),
      spring_prcp = sum(prcp[spring], na.rm = TRUE),
      spring_tmax = mean(tmax[spring], na.rm = TRUE),
      summer_prcp = sum(prcp[summer], na.rm = TRUE),
      summer_tmax = mean(tmax[summer], na.rm = TRUE),
      growing_prcp = sum(prcp[growing], na.rm = TRUE),
      growing_gdd = sum(gdd_base10[growing], na.rm = TRUE),
      growing_tmax = mean(tmax[growing], na.rm = TRUE),
      growing_tmin = mean(tmin[growing], na.rm = TRUE),
      .groups = "drop"
    )
}

build_model_datasets <- function() {
  train_tables <- read_training_tables()
  test_tables <- read_testing_tables()

  weather_train <- summarize_weather(train_tables$weather)
  weather_test <- summarize_weather(test_tables$weather)

  train_model <- train_tables$trait %>%
    dplyr::select(year, site, hybrid, yield_mg_ha) %>%
    dplyr::left_join(train_tables$meta, by = c("site", "year")) %>%
    dplyr::left_join(train_tables$soil, by = c("site", "year")) %>%
    dplyr::left_join(weather_train, by = c("site", "year")) %>%
    dplyr::mutate(
      previous_crop = tidyr::replace_na(previous_crop, "unknown"),
      site = as.factor(site),
      hybrid = as.factor(hybrid),
      previous_crop = as.factor(previous_crop),
      site_year = paste(site, year, sep = "_")
    )

  test_model <- test_tables$submission %>%
    dplyr::select(year, site, hybrid) %>%
    dplyr::left_join(test_tables$meta, by = c("site", "year")) %>%
    dplyr::left_join(test_tables$soil, by = c("site", "year")) %>%
    dplyr::left_join(weather_test, by = c("site", "year")) %>%
    dplyr::mutate(
      previous_crop = tidyr::replace_na(previous_crop, "unknown"),
      site = as.factor(site),
      hybrid = as.factor(hybrid),
      previous_crop = as.factor(previous_crop),
      site_year = paste(site, year, sep = "_")
    )

  list(
    train = train_model,
    test = test_model,
    submission_template = test_tables$submission,
    weather_train = weather_train
  )
}

make_time_split <- function(train_df) {
  assessment_year <- max(train_df$year, na.rm = TRUE)

  analysis_df <- train_df %>%
    dplyr::filter(year < assessment_year)

  assessment_df <- train_df %>%
    dplyr::filter(year == assessment_year)

  list(
    analysis = analysis_df,
    assessment = assessment_df,
    assessment_year = assessment_year
  )
}

fit_metrics <- function(data) {
  yardstick::metric_set(yardstick::rmse, yardstick::rsq, yardstick::mae)(
    data = data,
    truth = yield_mg_ha,
    estimate = .pred
  )
}

extract_importance <- function(fitted_workflow, model_name, top_n = 15) {
  fitted_engine <- workflows::extract_fit_parsnip(fitted_workflow)$fit

  vip::vi(fitted_engine) %>%
    tibble::as_tibble() %>%
    dplyr::slice_max(Importance, n = top_n) %>%
    dplyr::mutate(model = model_name)
}
