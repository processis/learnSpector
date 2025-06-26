#######ch10 fold cv  2025.6.25
#use 

set.seed(1001)

desha_folds<-vfold_cv(data=tbDesharnais_train,
                      v=10,
                      repeats = 8)
desha_folds

#desha_folds$splits[[1]]%>%analysis()%>%dim()

desha_recipe<-recipe(
  Effort ~ TeamExp + PointsNonAdjust , data = tbDesharnais_train
)

lm_model <- 
  linear_reg() %>% 
  set_engine("lm") %>% 
  set_mode("regression")

lm_wflow <-
  workflow() %>%
  add_recipe(desha_recipe)%>%
  add_model(lm_model)
lm_wflow

cv_results<-lm_wflow %>%
  fit_resamples(
    resamples=desha_folds,
    metrics = metric_set(rmse,rsq,mae)
  )

cv_metrics<-collect_metrics(cv_results)
print(cv_metrics)

#lm_wflow <-
 # lm_wflow %>%
#  remove_variables() %>%
#  add_recipe(simple_desh) %>%
#  fit_resamples(desha_folds)
#lm_wflow




