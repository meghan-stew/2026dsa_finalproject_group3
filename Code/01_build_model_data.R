source("Code/utils_pipeline.R")

check_required_packages(c("dplyr", "purrr", "readr", "tidyr"))
ensure_output_dirs()

datasets <- build_model_datasets()

readr::write_rds(datasets$train, project_path("Output", "data", "train_model_data.rds"))
readr::write_rds(datasets$test, project_path("Output", "data", "test_model_data.rds"))
readr::write_rds(datasets$submission_template, project_path("Output", "data", "submission_template.rds"))
readr::write_rds(datasets$weather_train, project_path("Output", "data", "weather_features_train.rds"))

message("Model-ready training and testing data saved to Output/data.")
