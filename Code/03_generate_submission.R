source("Code/utils_pipeline.R")

check_required_packages(c("dplyr", "readr"))
ensure_output_dirs()

test_data <- readr::read_rds(project_path("Output", "data", "test_model_data.rds"))
submission_template <- readr::read_rds(project_path("Output", "data", "submission_template.rds"))
champion_fit <- readr::read_rds(project_path("Output", "models", "champion_fit.rds"))

predictions <- predict(champion_fit, new_data = test_data)

submission_ready <- submission_template %>%
  dplyr::mutate(yield_mg_ha = predictions$.pred)

readr::write_csv(
  submission_ready,
  project_path("Output", "testing_submission_filled.csv")
)

message("Submission file written to Output/testing_submission_filled.csv")
