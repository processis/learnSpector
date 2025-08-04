#deepseek rmse code example
library(tidymodels)
data("mtcars")
#split
set.seed(123)
split <- initial_split(mtcars, prop = 0.75)
train_data <- training(split)
test_data <- testing(split)
#create linear reg model
lm_spec <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")
#fit model
lm_fit <- lm_spec %>%
  fit(mpg ~ wt + hp + cyl, data = train_data)

#make predictio
test_preds <- predict(lm_fit, new_data = test_data) %>%
  bind_cols(test_data)
test_preds
#calculate rmse
rmse_result <- test_preds %>%
  rmse(truth = mpg, estimate = .pred)
print(rmse_result)

