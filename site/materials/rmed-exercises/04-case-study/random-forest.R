# if local
# install.packages("ranger")

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
rf_spec <- 
  rand_forest(mtry = tune(), min_n = tune()) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")

# create random forest recipe---------------------------------------------------
rf_recipe <- 
  recipe(children ~ ., data = hotel_other) %>% 
  step_date(arrival_date) %>% 
  step_holiday(arrival_date) %>% 
  step_rm(arrival_date) 

# create workflow---------------------------------------------------------------
rf_workflow <- 
  workflow() %>% 
  add_model(rf_spec) %>% 
  add_recipe(rf_recipe)

rf_grid <- expand_grid(mtry = c(1, 5, 10), min_n = c(10, 20, 30))
# train/tune--------------------------------------------------------------------
set.seed(345)
# EDIT ME TO TUNE!==============================================================
rf_results <- 
  rf_workflow %>% 
  tune_grid(resamples = val_set,
            grid = rf_grid,
            # change me to control_grid with tune_grid
            control = control_resamples(save_pred = TRUE,
                                        verbose = TRUE),
            metrics = metric_set(roc_auc))
