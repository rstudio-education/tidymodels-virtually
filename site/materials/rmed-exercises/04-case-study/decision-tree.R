# if local
# install.packages("rpart")

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
tree_spec <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("classification")

# create decision tree recipe---------------------------------------------------
tree_recipe <- 
  recipe(children ~ ., data = hotel_other) %>% 
  step_date(arrival_date) %>% 
  step_holiday(arrival_date) %>% 
  step_rm(arrival_date) 

# create workflow---------------------------------------------------------------
tree_workflow <- 
  workflow() %>% 
  add_model(tree_spec) %>% 
  add_recipe(tree_recipe)

# train/tune--------------------------------------------------------------------
set.seed(345)
# EDIT ME TO TUNE!==============================================================
tree_results <- 
  tree_workflow %>% 
  fit_resamples(resamples = val_set,
                # change me to control_grid with tune_grid
                control = control_resamples(save_pred = TRUE,
                                            verbose = TRUE),
                metrics = metric_set(roc_auc))
