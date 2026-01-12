#learn ch7 workflow with desha data
library(tidymodels)
tidymodels_prefer()
#TidyModel Ch6 fit models with parsnip
lm_model <- 
  linear_reg() %>% 
  set_engine("lm")

lm_wflow <-
  workflow() %>%
  add_model(lm_model)
lm_wflow

lm_wflow <-
  lm_wflow %>%
  add_formula(Effort ~ TeamExp + PointsNonAdjust)
lm_wflow

lm_fit <- fit(lm_wflow, tbDesharnais_train)
lm_fit

lm_wflow <-
  lm_wflow %>%
  remove_formula() %>%
  add_variables(outcome = Effort, predictors = c(TeamExp, ManagerExp))
lm_wflow

fit(lm_wflow, tbDesharnais_train)

lm_wflow %>%
  add_recipe(simple_desh) #to reproduce the error as from book

lm_wflow <-
  lm_wflow %>%
  remove_variables() %>%
  add_recipe(simple_desh)
lm_wflow

lm_fit <- fit(lm_wflow, tbDesharnais_train)
predict(lm_fit, tbDesharnais_test %>% slice(1:6))


#try to change to use another recipe
lm_wflow <-
  lm_wflow %>%
  remove_recipe() %>%
  add_recipe(simple2_desh)
lm_wflow


lm_fit <- fit(lm_wflow, tbDesharnais_train)
predict(lm_fit, tbDesharnais_test %>% slice(1:6))