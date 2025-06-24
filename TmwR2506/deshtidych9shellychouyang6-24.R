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




tbDesharnais <- tbDesharnais %>% mutate(Effort = log10(Effort))
tbDesharnais <- tbDesharnais %>% mutate(PointsAjust = log10(PointsAjust))
tbDesharnais <- tbDesharnais %>% mutate(PointsNonAdjust = log10(PointsNonAdjust))
# also for Length
ggplot(tbDesharnais, aes(x = Length)) + geom_histogram(bins = 10, col="white")
tbDesharnais <- tbDesharnais %>% mutate(Length = log10(Length))

tbDesharnais$TeamExp<-as.numeric(tbDesharnais$TeamExp)
tbDesharnais$ManagerExp<-as.numeric(tbDesharnais$ManagerExp)
View(tbDesharnais)

tbDesharnais_train <- subset(tbDesharnais,Project!=56)
tbDesharnais_train <- subset(tbDesharnais_train,Project!=31)
tbDesharnais_train <- subset(tbDesharnais_train,Project!=41)
tbDesharnais_train <- subset(tbDesharnais_train,Project!=22)
tbDesharnais_train <- subset(tbDesharnais_train,Project!=78)
tbDesharnais_train <- subset(tbDesharnais_train,Project!=73)
tbDesharnais_train <- subset(tbDesharnais_train,Project!=32)
tbDesharnais_train <- subset(tbDesharnais_train,Project!=13)

library(dplyr)

#tbDesharnais_test <-tbDesharnais%>%filter(Project%in%c(56,31,41,22,78,73,32,13))



tbDesharnais_test <- tbDesharnais[c(56,31,41,22,78,73,32,13), ]



#################################


tbDesharnais_train <- subset(tbDesharnais,Project!=1)
tbDesharnais_train <- subset(tbDesharnais_train,Project!=2)
tbDesharnais_train <- subset(tbDesharnais_train,Project!=3)
tbDesharnais_train <- subset(tbDesharnais_train,Project!=4)
tbDesharnais_train <- subset(tbDesharnais_train,Project!=5)
tbDesharnais_train <- subset(tbDesharnais_train,Project!=6)
tbDesharnais_train <- subset(tbDesharnais_train,Project!=7)


tbDesharnais_test <- tbDesharnais[c(1,2,3,4,5,6,7), ]


#######################################################  +ManagerExp
simple_desh <-
  recipe(Effort ~ TeamExp + PointsNonAdjust + Language, data = tbDesharnais_train) %>%
  step_log(PointsNonAdjust, base = 10) %>%
  step_dummy(Language,
             levels = c(2,3)) 
simple_desh

simple2_desh <-
  recipe(Effort ~ TeamExp + PointsNonAdjust+ Language, data = tbDesharnais_train) %>%
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


################################################################



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
  add_variables(outcome = Effort, predictors = c(TeamExp,ManagerExp))
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

#############################################3s



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

#############################################################


#ch9 judge model effectiveness
desha_res <- predict(lm_fit, new_data = tbDesharnais_test %>% select(-Effort))
desha_res
#match predicted with observed outcome
desha_res <- bind_cols(desha_res, tbDesharnais_test %>%
                         select(Effort))
desha_res
#plot data first
ggplot(desha_res, aes(x= Effort, y = .pred)) +
  #create a diagonal line
  geom_abline(lty =2) +
  geom_point(alpha = 0.5) +
  labs(y = "Predicted Effort", x = "F Points") +
  # scale and size x y axis uniformly
  coord_obs_pred()
# calculate rmse
rmse(desha_res, truth = Effort, estimate = .pred)
#
#compute rmse , rsq, mae   using metric set
desha_metrics <- metric_set(rmse,rsq,mae)
desha_metrics(desha_res, truth = Effort, estimate = .pred)



###############################################################


# 使用训练好的随机森林模型进行预测
rf_res <- predict(final_rf_fit, new_data = tbDesharnais_test %>% select(-Effort))
rf_res

# 将预测结果与实际观测值合并
rf_res <- bind_cols(rf_res, tbDesharnais_test %>% select(Effort))
rf_res

# 绘制预测值与实际值的散点图
ggplot(rf_res, aes(x = Effort, y = .pred)) +
  geom_abline(lty = 2, color = "gray50") +  # 添加对角线参考线
  geom_point(alpha = 0.6, color = "steelblue") +  # 半透明点
  labs(y = "Predicted Effort (log10)", 
       x = "Observed Effort (log10)",
       title = "Random Forest Model Performance") +
  coord_obs_pred() +  # 统一坐标轴比例
  theme_minimal()

# 计算RMSE
rmse(rf_res, truth = Effort, estimate = .pred)

# 计算多个评估指标 (RMSE, R-squared, MAE)
rf_metrics <- metric_set(rmse, rsq, mae)
rf_metrics(rf_res, truth = Effort, estimate = .pred)

# 可以添加残差分析图
rf_res %>% 
  mutate(residual = Effort - .pred) %>% 
  ggplot(aes(x = .pred, y = residual)) +
  geom_hline(yintercept = 0, lty = 2, color = "red") +
  geom_point(alpha = 0.6, color = "steelblue") +
  labs(x = "Predicted Values", 
       y = "Residuals",
       title = "Residual Analysis") +
  theme_minimal()

# 如果需要比较多个模型的结果（比如和之前的线性模型比较）
comparison <- bind_rows(
  lm_fit %>% 
    predict(new_data = tbDesharnais_test) %>% 
    bind_cols(tbDesharnais_test %>% select(Effort)) %>% 
    mutate(model = "Linear Regression"),
  
  final_rf_fit %>% 
    predict(new_data = tbDesharnais_test) %>% 
    bind_cols(tbDesharnais_test %>% select(Effort)) %>% 
    mutate(model = "Random Forest")
)

# 绘制比较图
ggplot(comparison, aes(x = Effort, y = .pred, color = model)) +
  geom_abline(lty = 2) +
  geom_point(alpha = 0.5) +
  facet_wrap(~model) +
  labs(y = "Predicted Effort", x = "Observed Effort") +
  coord_obs_pred() +
  theme(legend.position = "none")

# 计算各模型指标对比
comparison %>% 
  group_by(model) %>% 
  summarise(
    rmse = rmse_vec(truth = Effort, estimate = .pred),
    rsq = rsq_vec(truth = Effort, estimate = .pred),
    mae = mae_vec(truth = Effort, estimate = .pred)
  )




