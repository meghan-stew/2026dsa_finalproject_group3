library(tidyverse)
library(tidymodels)

final_model <- read_rds("output/final_model.rds")
test_model_data <- read_rds("Data/test_model_data.rds")
submission_template <- read_csv("Data/testing/testing_submission.csv", show_col_types = FALSE)

submission_predictions <- predict(final_model, new_data = test_model_data) %>%
  pull(.pred)

completed_submission <- submission_template %>%
  mutate(yield_mg_ha = if_else(is.na(yield_mg_ha), submission_predictions, yield_mg_ha))

write_csv(completed_submission, "output/testing_submission_completed.csv")

message("Saved submission file to output/testing_submission_completed.csv")
