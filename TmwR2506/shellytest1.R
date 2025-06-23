library(tidyverse)
library(tidymodels)
tidymodels_prefer()

tbDesharnais <- read_csv("/media/user/娱乐/learnSpector/TmwR2506/cleanDesharnais77.csv",col_names = TRUE,na = '-1',
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


#######################################################3

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

rf_recipe <- 
  recipe(Effort ~ ., data = tbDesharnais_train) %>%
  step_rm(Project, PointsAjust, Adjustment) %>%  # 移除不需要的变量
  step_dummy(all_nominal_predictors()) %>%       # 处理分类变量
  step_normalize(all_numeric_predictors())       # 标准化数值变量

# 查看recipe
rf_recipe


#####################################################

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


###########################################




rf_model <- rand_forest(
  trees = 1000, min_n = 5       # 节点最小观测数
) %>%
  set_engine("ranger"
             #, importance = "permutation"
             ) %>%  # 计算变量重要性
  set_mode("regression")


rf_workflow <- workflow() %>%
  add_recipe(simple_desh) %>%
  add_model(rf_model)

rf_workflow

rf_fit <- fit(rf_workflow, tbDesharnais_train)
predict(rf_fit, tbDesharnais_test %>% slice(1:6))

#try to change to use another recipe

rf_workflow <- workflow() %>%
  add_recipe(simple2_desh) %>%
  add_model(rf_model)

rf_workflow

rf_fit <- fit(rf_workflow, tbDesharnais_train)
predict(rf_fit, tbDesharnais_test %>% slice(1:6))





################################



 
# 创建随机森林模型
rf_model <- 
  rand_forest(
    mtry = tune(),      # 每个分割时考虑的预测变量数量
    trees = 1000,       # 树的数量
    min_n = tune()      # 节点中所需的最小数据点数量
  ) %>% 
  set_engine("ranger", importance = "impurity") %>%  # 使用ranger引擎
  set_mode("regression")                            # 回归问题


#创建工作流
rf_wflow <- 
  workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(rf_recipe)


# 设置交叉验证
set.seed(502)
tb_folds <- vfold_cv(tbDesharnais_train, v = 5)

# 设置调参网格
rf_grid <- grid_regular(
  mtry(range = c(2, 8)),  # 尝试2到8个预测变量
  min_n(),                # 尝试默认的最小节点大小范围
  levels = 5              # 每个参数的尝试级别数
)

# 调参
rf_tune <- tune_grid(
  rf_wflow,
  resamples = tb_folds,
  grid = rf_grid,
  metrics = metric_set(rmse, rsq)
)

# 查看最佳参数
show_best(rf_tune, metric = "rmse")

# 选择最佳参数
best_rf <- select_best(rf_tune, metric = "rmse")

# 最终化工作流
final_rf_wflow <- 
  rf_wflow %>% 
  finalize_workflow(best_rf)

# 训练最终模型
final_rf_fit <- fit(final_rf_wflow, tbDesharnais_train)

# 在测试集上评估
test_results <- tbDesharnais_test %>% 
  bind_cols(predict(final_rf_fit, tbDesharnais_test)) %>% 
  rename(predicted_effort = .pred)

# 计算性能指标
test_metrics <- metric_set(rmse, rsq)(test_results, 
                                      truth = Effort, 
                                      estimate = predicted_effort)
print(test_metrics)

# 查看变量重要性
library(vip)
final_rf_fit %>% 
  extract_fit_parsnip() %>% 
  vip(geom = "point")

# 预测新数据示例
predict(final_rf_fit, tbDesharnais_test %>% slice(1:6))
