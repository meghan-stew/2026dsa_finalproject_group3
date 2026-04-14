library(tidyverse)

library(tidyverse)

train <- read_csv("../data/training/training_trait.csv")
meta  <- read_csv("../data/training/training_meta.csv")
soil  <- read_csv("../data/training/training_soil.csv")

train <- train %>% rename_with(tolower)
meta  <- meta  %>% rename_with(tolower)
soil  <- soil  %>% rename_with(tolower)

data_full <- train %>%
  left_join(meta, by = c("site", "year")) %>%
  left_join(soil, by = c("site", "year"))

glimpse(data_full)

colSums(is.na(data_full))


data_model <- data_full %>%
  select(-soilph, -om_pct, -soilk_ppm, -soilp_ppm)

glimpse(data_model)


data_model <- data_model %>%
  filter(!is.na(date_harvested))

data_model <- data_model %>%
  mutate(previous_crop = replace_na(previous_crop, "Unknown"))


 data_model <- data_model %>%
  mutate(
year_dbl = as.double(year)) %>%
  mutate(site_chr = as.character(site)) #added this in case the ML component needs it in character format




data_model <- data_model %>%
  mutate(
    year = as.factor(year),
    site = as.factor(site)
    
  )


glimpse(data_model)

ggplot(data_model, aes(x = yield_mg_ha)) +
  geom_histogram(bins = 40)

ggplot(data_model, aes(x = site, y = yield_mg_ha)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90))

ggplot(data_model, aes(x = factor(year), y = yield_mg_ha)) +
  geom_boxplot()

ggplot(data_model, aes(x = grain_moisture, y = yield_mg_ha)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm")

ggplot(data_model, aes(x = hybrid, y = yield_mg_ha)) +
  geom_boxplot() +
  theme(axis.text.x = element_blank())

top_hybrids <- data_model %>%
  group_by(hybrid) %>%
  summarise(n = n()) %>%
  slice_max(n, n = 10) %>%
  pull(hybrid)

data_model %>%
  filter(hybrid %in% top_hybrids) %>%
  ggplot(aes(x = hybrid, y = yield_mg_ha)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45))

  head(data_model$date_planted)

library(lubridate)

data_model <- data_model %>%
  mutate(
    date_planted = mdy(date_planted),
    date_harvested = mdy(date_harvested),
    planting_month = month(date_planted),
    harvest_month = month(date_harvested)
  )

glimpse(data_model[, c("date_planted", "date_harvested", "planting_month", "harvest_month")])

data_model <- data_model %>%
mutate(days_to_harvest = as.numeric(date_harvested - date_planted))
summary(data_model$days_to_harvest)


ggplot(data_model, aes(x = days_to_harvest, y = yield_mg_ha)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm")


ggplot(data_model, aes(x = factor(planting_month), y = yield_mg_ha)) +
 geom_boxplot()


ggplot(data_model, aes(x = factor(harvest_month), y = yield_mg_ha)) +
  geom_boxplot()

ggplot(data_model, aes(x = longitude, y = yield_mg_ha)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm")

ggplot(data_model, aes(x = latitude, y = yield_mg_ha)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm")

glimpse(data_model)

library(daymetr)
data_model$year[1]


daymet_none <- download_daymet(site = data_model$site[1],
                              lat = data_model$latitude[1],
                              lon =  data_model$longitude[1],
                              start = 2014,
                              end = 2014,
                              simplify = T
                              )

glimpse(daymet_none)

data_model2 <- data_model %>%
  filter(!is.na(latitude)) %>%
  filter(!is.na(longitude))


colSums(is.na(data_model2))




library(daymetr)

daymet_zero <- data_model2 %>%
  mutate(weather = pmap(list(.y = year_dbl,
                             .site = site,
                             .lat = latitude,
                             .lon = longitude
                             ),
                        function(.y, .site, .lat, .lon)
                          download_daymet(
                            site = .site,
                            lat = .lat,
                 
                                       lon = .lon,
                            start = .y,
                            end = .y,
                            simplify = T,
                            silent = T
                          ) %>%
                          rename(.year = year,
                                .site = site
                          )

                        ))

daymet_zero


#to-do for Meghan: make data table sorted by max yield per site, then 

library(tidymodels)
library(finetune)     
library(vip)          
library(xgboost)      
library(ranger)       
library(tidyverse)    
library(doParallel)   


corn_data <- data_model2 %>%
  select(-replicate, -block, -year, -days_to_harvest, -site, -date_planted, -date_harvested)

glimpse(corn_data)


corn_data2 <- corn_data %>%
  mutate(harvest_month = month(harvest_month, label = T)) %>%
  mutate(planting_month = month(planting_month, label = T))
  
  corn_data2

set.seed(931735) # Setting seed to get reproducible results 
corn_split <- initial_split(
  corn_data2, 
  prop = .7, 
  strata = yield_mg_ha  # Stratify by target variable
  )
corn_split

corn_train <- training(corn_split)  # 70% of data
corn_train #Training data frame 

library(tidyverse)
library(lubridate)
library(daymetr)

site_years <- data_model %>%
  distinct(site, year, latitude, longitude) %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  mutate(year = as.integer(as.character(year)))

get_daymet_data <- function(site, year, latitude, longitude) {
  d <- download_daymet(
    site = as.character(site),
    lat = latitude,
    lon = longitude,
    start = year,
    end = year,
    internal = TRUE,
    silent = TRUE
  )
  
  df <- d$data
  
  tibble(
    site = site,
    year = year,
    yday = df$yday,
    prcp = df$prcp..mm.day.,
    tmax = df$tmax..deg.c.,
    tmin = df$tmin..deg.c.,
    srad = df$srad..W.m.2.,
    vp = df$vp..Pa.,
    dayl = df$dayl..s.
  )
}

weather_daily <- purrr::pmap_dfr(
  site_years,
  function(site, year, latitude, longitude) {
    tryCatch(
      get_daymet_data(site, year, latitude, longitude),
      error = function(e) NULL
    )
  }
)

weather_features <- weather_daily %>%
  mutate(
    date = as.Date(yday - 1, origin = paste0(year, "-01-01")) + 1,
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

data_model_weather <- data_model %>%
  mutate(year = as.integer(as.character(year))) %>%
  left_join(weather_features, by = c("site", "year"))

glimpse(weather_daily)
glimpse(weather_features)
glimpse(data_model_weather)

data_model_weather %>%
  group_by(site, year) %>%
  summarise(n_weather = n_distinct(total_prcp)) %>%
  summary()

names(data_model_weather)

final_data <- data_model_weather %>%
  select(
    yield_mg_ha,
    
    hybrid, site, year, previous_crop,
    grain_moisture,
    
    planting_month, harvest_month, days_to_harvest,
    
    total_prcp, prcp_apr_sep, prcp_may_jul, prcp_aug,
    mean_tmean, mean_tmax, mean_tmin, tmean_apr_sep, tmax_jul,
    total_gdd10, gdd10_apr_sep,
    mean_srad, mean_vp, mean_dayl
  )

final_data <- final_data %>%
  mutate(
    hybrid = as.factor(hybrid),
    site = as.factor(site),
    year = as.factor(year),
    previous_crop = as.factor(previous_crop)
  )

final_data <- final_data %>%
  drop_na()
glimpse(final_data)
summary(final_data$yield_mg_ha)

library(tidymodels)
library(finetune)
library(vip)
library(xgboost)
library(tidyverse)
library(doParallel)

set.seed(931735)

corn_split <- initial_split(
  final_data,
  prop = 0.7,
  strata = yield_mg_ha
)

corn_train <- training(corn_split)
corn_test  <- testing(corn_split)

ggplot() +
  geom_density(data = corn_train, aes(x = yield_mg_ha), color = "red") +
  geom_density(data = corn_test, aes(x = yield_mg_ha), color = "blue")


corn_recipe <- recipe(yield_mg_ha ~ ., data = corn_train) %>%
  step_rm(mean_dayl, hybrid) %>%
  step_novel(all_nominal_predictors()) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors())

corn_prep <- corn_recipe %>%
  prep()

corn_prep

xgb_spec <- boost_tree(
  trees = tune(),
  tree_depth = tune(),
  min_n = tune(),
  learn_rate = tune()
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

xgb_spec

set.seed(235)

resampling_foldcv <- vfold_cv(
  corn_train,
  v = 10,
  strata = yield_mg_ha
)

resampling_foldcv


set.seed(432)

xgb_grid <- grid_latin_hypercube(
  trees(range = c(300L, 1200L)),
  tree_depth(range = c(2L, 10L)),
  min_n(range = c(2L, 40L)),
  learn_rate(range = c(-3, -0.5)),
  size = 20
)

xgb_grid

registerDoParallel(cores = 8)

set.seed(76544)

xgb_res <- tune_race_anova(
  object = xgb_spec,
  preprocessor = corn_recipe,
  resamples = resampling_foldcv,
  grid = xgb_grid,
  control = control_race(save_pred = TRUE)
)

stopImplicitCluster()
