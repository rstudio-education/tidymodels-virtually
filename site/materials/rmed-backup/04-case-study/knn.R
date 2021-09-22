# if local
# install.packages("kknn")

library(tidyverse)
library(tidymodels)

# read in the data--------------------------------------------------------------
hotels <-
  read_csv('https://tidymodels.org/start/case-study/hotels.csv') %>%
  mutate_if(is.character, as.factor) %>%
  # randomly select rows
  slice_sample(prop = 0.30) 

# data splitting----------------------------------------------------------------
set.seed(123)
splits      <- initial_split(hotels, strata = children)
hotel_other <- training(splits)
hotel_test  <- testing(splits)

# resample once-----------------------------------------------------------------
set.seed(234)
val_set <- validation_split(hotel_other, 
                            strata = children, 
                            prop = 0.80)

# specify un-tuned parsnip model------------------------------------------------
# EDIT ME TO TUNE!==============================================================
knn_spec <- nearest_neighbor() %>%
  set_engine("kknn") %>%
  set_mode("classification")

# create knn recipe-------------------------------------------------------------
holidays <- c("AllSouls", "AshWednesday", "ChristmasEve", "Easter", 
              "ChristmasDay", "GoodFriday", "NewYearsDay", "PalmSunday")

knn_recipe <- 
  recipe(children ~ ., data = hotel_other) %>% 
  step_date(arrival_date) %>% 
  step_holiday(arrival_date, holidays = holidays) %>% 
  step_rm(arrival_date) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

# create workflow---------------------------------------------------------------
knn_workflow <- 
  workflow() %>% 
  add_model(knn_spec) %>% 
  add_recipe(knn_recipe)

# EDIT ME TO TUNE!==============================================================
knn_results <- 
  knn_workflow %>% 
  fit_resamples(resamples = val_set,
                # change me to control_grid with tune_grid
                control = control_resamples(save_pred = TRUE,
                                            verbose = TRUE),
                metrics = metric_set(roc_auc))
