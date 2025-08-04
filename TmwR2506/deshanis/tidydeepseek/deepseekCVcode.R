#deepseek 10fold CV mtcars code example
library(tidyverse)
library(tidymodels)
# data prep
data(mtcars)
#split to train and test sets
set.seed(123)
split <- initial_split(mtcars, prop = 0.8)
train_data<-training(split)
test_data<-testing(split)
#create 10 fold cv splits
cv_folds <- vfold_cv(train_data, v=10, strata = mpg)
#define preprocessing recipe
model_recipe<- recipe(mpg ~ ., data = train_data) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors())
#specify and tune model
#tune linear regression with penalty (elastic net)
model_spec <- linear_reg(
  penalty = tune(),  #tune hyperparameter
  mixture = tune()    #tune alpha (0 = ridge, 1 = lasso)
) %>%
  set_engine("glmnet")
#create workflow
model_wf <- workflow() %>%
  add_recipe(model_recipe) %>%
  add_model(model_spec)
#tune model with CV
#set up tuning grid
tune_grid <- grid_regular(
  penalty(range = c(-5,0)),
  mixture(range = c(0,1)),
  levels =5
)
#run tuning
tune_results <- tune_grid(
  model_wf,
  resamples = cv_folds,
  grid = tune_grid,
  metrics = metric_set(rmse, rsq)

)
#select best model 
best_model <- select_best(tune_results, metric = "rmse")

final_wf <- finalize_workflow(model_wf, best_model)

#fit final model and predict on test set
final_fit <-fit(final_wf, data = train_data ) #fit on entire training data

#predict on hole out test set
test_predictions <- predict(final_fit, new_data = test_data) %>%
  bind_cols(test_data) #combine predictins with actual values
#evaluate performance
test_metrics <- test_predictions %>%
  metrics(truth = mpg, estimate = .pred)
#view metrics
print(test_metrics)
#visualize
ggplot(test_predictions, aes(x = mpg, y = .pred)) +
         geom_point(alpha = 0.5) +
         geom_abline(lty = 2) +
         labs(title =  "Actual vs Prdicted MPG ",
              x = "Actual MPG",
              y = "Predicted MPG")+
         coord_obs_pred()
       