library(tidyverse)
library(lubridate)

make_weather_features <- function(weather_daily) {
  weather_daily %>%
    mutate(
      date = as.Date(sprintf("%s-01-01", year)) + (yday - 1),
      month = month(date),
      tmean = (tmax + tmin) / 2,
      gdd10 = pmax(tmean - 10, 0)
    ) %>%
    group_by(site, year) %>%
    summarise(
      total_prcp = sum(prcp, na.rm = TRUE),
      prcp_apr_sep = sum(prcp[month %in% 4:9], na.rm = TRUE),
      prcp_may_jul = sum(prcp[month %in% 5:7], na.rm = TRUE),
      prcp_aug = sum(prcp[month == 8], na.rm = TRUE),
      mean_tmax = mean(tmax, na.rm = TRUE),
      mean_tmin = mean(tmin, na.rm = TRUE),
      mean_tmean = mean(tmean, na.rm = TRUE),
      tmean_apr_sep = mean(tmean[month %in% 4:9], na.rm = TRUE),
      tmax_jul = mean(tmax[month == 7], na.rm = TRUE),
      total_gdd10 = sum(gdd10, na.rm = TRUE),
      gdd10_apr_sep = sum(gdd10[month %in% 4:9], na.rm = TRUE),
      mean_srad = mean(srad, na.rm = TRUE),
      mean_vp = mean(vp, na.rm = TRUE),
      mean_dayl = mean(dayl, na.rm = TRUE),
      .groups = "drop"
    )
}

clean_soil <- function(soil_raw) {
  soil_raw %>%
    rename_with(tolower) %>%
    separate(site, into = c("site", "site_year"), sep = "_", remove = TRUE) %>%
    select(-site_year) %>%
    mutate(year = as.integer(year))
}

clean_meta <- function(meta_raw) {
  meta_raw %>%
    rename_with(tolower) %>%
    mutate(
      year = as.integer(year),
      previous_crop = str_to_lower(previous_crop)
    )
}

training_trait <- read_csv("Data/training/training_trait.csv", show_col_types = FALSE) %>%
  rename_with(tolower) %>%
  mutate(year = as.integer(year))

training_meta <- read_csv("Data/training/training_meta.csv", show_col_types = FALSE) %>%
  clean_meta()

training_soil <- read_csv("Data/training/training_soil.csv", show_col_types = FALSE) %>%
  clean_soil()

testing_submission <- read_csv("Data/testing/testing_submission.csv", show_col_types = FALSE) %>%
  rename_with(tolower) %>%
  mutate(year = as.integer(year))

testing_meta <- read_csv("Data/testing/testing_meta.csv", show_col_types = FALSE) %>%
  clean_meta()

testing_soil <- read_csv("Data/testing/testing_soil.csv", show_col_types = FALSE) %>%
  clean_soil()

weather_train <- read_csv("Data/weather_daily.csv", show_col_types = FALSE)
weather_test <- read_csv("Data/weather_daily_test.csv", show_col_types = FALSE)

weather_train_features <- make_weather_features(weather_train)
weather_test_features <- make_weather_features(weather_test)

train_model_data <- training_trait %>%
  select(year, site, hybrid, yield_mg_ha) %>%
  left_join(training_meta, by = c("year", "site")) %>%
  left_join(training_soil, by = c("year", "site")) %>%
  left_join(weather_train_features, by = c("year", "site")) %>%
  mutate(
    across(c(site, hybrid, previous_crop), as.factor),
    year = as.factor(year)
  )

test_model_data <- testing_submission %>%
  select(year, site, hybrid) %>%
  left_join(testing_meta, by = c("year", "site")) %>%
  left_join(testing_soil, by = c("year", "site")) %>%
  left_join(weather_test_features, by = c("year", "site")) %>%
  mutate(
    across(c(site, hybrid, previous_crop), as.factor),
    year = as.factor(year)
  )

dir.create("output", showWarnings = FALSE)

write_rds(train_model_data, "Data/train_model_data.rds")
write_rds(test_model_data, "Data/test_model_data.rds")

write_csv(train_model_data, "output/train_model_data_preview.csv")
write_csv(test_model_data, "output/test_model_data_preview.csv")

message("Prepared training and testing model data.")
