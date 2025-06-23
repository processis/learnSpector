tbDeshanais_te
#25.3.2 use Tidyverse tibble to prepare datasets 
#but use Caret to run all models
# Read Desharnais public dataset from promise uottawa repository
# random took out 7 data for final validation
#replace old: desharnais <- read.table("desharnaisLogEffort77.csv",sep = ",", header = TRUE)  #old non-tidy read
library(tidyverse)
library(tidymodels)
tidymodels_prefer()
tbDesharnais <- read_csv("Desharnais77.csv",col_names = TRUE,na = '-1',
                         cols(
                           'Project' = col_integer(),
                           'TeamExp' = col_integer(),
                           'ManagerExp' = col_integer(),
                           'YearEnd' = col_integer(),
                           'Length' = col_integer(),
                           'Effort' = col_integer(),
                           'Transactions' = col_integer(),
                           'Entities' = col_integer(),
                           'PointsNonAdjust' = col_integer(),
                           'Adjustment' = col_integer(),
                           'PointsAjust' = col_integer(),
                           'Language' = col_character(),
                         )
)
#
print(tbDesharnais)
#histograms , check skew
ggplot(tbDesharnais, aes(x = Effort)) + geom_histogram(bins = 10, col="white")
#histograms , check skew
ggplot(tbDesharnais, aes(x = PointsNonAdjust)) + geom_histogram(bins = 10, col="white")
#histograms , check skew
ggplot(tbDesharnais, aes(x = PointsAjust)) + geom_histogram(bins = 10, col="white")
# mutate all these 3 skewed variables by Log
tbDesharnais <- tbDesharnais %>% mutate(Effort = log10(Effort))
tbDesharnais <- tbDesharnais %>% mutate(PointsAjust = log10(PointsAjust))
tbDesharnais <- tbDesharnais %>% mutate(PointsNonAdjust = log10(PointsNonAdjust))
# also for Length
ggplot(tbDesharnais, aes(x = Length)) + geom_histogram(bins = 10, col="white")
tbDesharnais <- tbDesharnais %>% mutate(Length = log10(Length))
#
#use random number to split training vs testing
set.seed(502)
tbDesharnais_split <- initial_split(tbDesharnais, prop = 0.85) # strata = Effort not use
tbDesharnais_train <- training(tbDesharnais_split)
tbDesharnais_test  <-  testing(tbDesharnais_split)
#
#TidyModel Ch6 fit models with parsnip
lm_model <- 
  linear_reg() %>% 
  set_engine("lm")

lm_form_fit <- 
  lm_model %>% 
    fit(Effort ~ PointsNonAdjust + TeamExp, data = tbDesharnais_train)

lm_xy_fit <- 
  lm_model %>% 
  fit_xy(
    x = tbDesharnais_train %>% select(PointsNonAdjust, TeamExp),
    y = tbDesharnais_train %>% pull(Effort)
  )

lm_form_fit
lm_xy_fit
#extract fit
lm_form_fit %>% extract_fit_engine()
#Normal methods can be applied to this object, such as printing and plotting:
lm_form_fit %>% extract_fit_engine() %>% vcov()
#
model_res <- 
  lm_form_fit %>% 
  extract_fit_engine() %>% 
  summary()
#
#
#6 Fitting Models with parnip    www.tmwr.org/models
linear_reg() %>% set_engine("lm")
linear_reg() %>% set_engine("glmnet")
linear_reg() %>% set_engine("stan")
# translate provide details
linear_reg() %>% set_engine("lm") %>% translate()
linear_reg(penalty = 1) %>% set_engine("glmnet") %>% translate()
linear_reg() %>% set_engine("stan") %>% translate()
#follow ames e.g. for desharnais
lm_model <- linear_reg() %>% set_engine("lm")
lm_form_fit <- lm_model %>% fit(Effort ~ TeamExp + PointsNonAdjust, data = tbDesharnais_train)
lm_xy_fit <- lm_model %>%
  fit_xy(
    x = tbDesharnais_train %>% select(PointsNonAdjust,TeamExp),
    y = tbDesharnais_train %>% pull(Effort)
  )
lm_form_fit
lm_xy_fit
#
#
tidy(lm_form_fit)
#make predictions, numeric data
tbDesharnais_test_small <- tbDesharnais_test %>% slice(1:12)
predict(lm_form_fit, new_data = tbDesharnais_test_small)
#merge with original data
#tbDesharnais_test_small %>% 
#  select(Effort) %>% 
#  bind_cols(predict(lm_form_fit, tbDesharnais_test_small)) %>% 
  # Add 95% prediction intervals to the results:
#  bind_cols(predict(lm_form_fit, tbDesharnais_test_small, type = "pred_int")) 
#
#tbDesharnais_test_small <- bind_cols(tbDesharnais_test_small, tbDesharnais_test %>% select(Effort))
#tbDesharnais_test_small
#
#ch9 judge model effectiveness
tbDesharnais_test_res <- bind_cols(tbDesharnais_test_res, tbDesharnais_test %>% select(Effort))
tbDesharnais_test_res
#plot the data  before computing metrics: 
ggplot(tbDesharnais_test_res, aes(x = Effort, y = .pred)) + 
  # Create a diagonal line:
  geom_abline(lty = 2) + 
  geom_point(alpha = 0.5) + 
  labs(y = "Predicted Effort (log10)", x = "Effort(log10)") +
  # Scale and size the x- and y-axis uniformly:
  coord_obs_pred()
#RMSE function
rmse(tbDesharnais_test_res, truth = Effort, estimate = .pred)
# add metric , add Rsq and mean abs error
tbDesharnais_metrics <- metric_set(rmse, rsq, mae)
tbDesharnais_metrics(tbDesharnais_test_res, truth = Effort, estimate = .pred)

