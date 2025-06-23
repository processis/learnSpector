#learn tidymodel recipe for desharnais data set
library(tidymodels)
library(recipes)

simple_desh <-
  recipe(Effort ~ TeamExp + PointsNonAdjust + Language, data = tbDesharnais_train) %>%
  step_log(PointsNonAdjust, base = 10) %>%
  step_dummy(Language,
               levels = c(2,3)) 
simple_desh

simple2_desh <-
  recipe(Effort ~ TeamExp + PointsNonAdjust + Language, data = tbDesharnais_train) %>%
  step_log(PointsNonAdjust, base = 10) %>%
  step_dummy(all_nominal_predictors()) 
simple2_desh