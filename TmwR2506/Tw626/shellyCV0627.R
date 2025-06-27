# 加载必要的包
library(tidymodels)  # 包含recipes、parsnip、tune等
library(readr)       # 读取CSV文件
library(vip)         # 变量重要性分析

# 1. 读取数据并预处理
data <- read_csv("desharnais77CH-1.csv") %>% 
  mutate(logEffort = log(Effort))  # 对Effort取对数


data$TeamExp<-as.numeric(data$TeamExp)
data$ManagerExp<-as.numeric(data$ManagerExp)


train_data <- subset(data,Project!=73)
train_data <- subset(train_data,Project!=66)
train_data <- subset(train_data,Project!=56)
train_data <- subset(train_data,Project!=41)
train_data <- subset(train_data,Project!=32)
train_data <- subset(train_data,Project!=22)
#tbDesharnais_train <- subset(tbDesharnais_train,Project!=32)
train_data <- subset(train_data,Project!=13)


test_data <- data[c(73,66,56,41,32,22,13), ]

# 2. 划分训练集和测试集（Project 1-7为测试集）
test_data <- data %>% filter(Project %in% 1:7)
train_data <- data %>% filter(!Project %in% 1:7)



# 3. 定义线性回归模型
lm_model <- linear_reg() %>% 
  set_engine("lm") %>% 
  set_mode("regression")

# 4. 创建预处理recipe（标准化预测变量）
recipe_lm <- recipe(logEffort ~ TeamExp + PointsAjust, data = train_data) %>%
  step_normalize(all_numeric_predictors())  # 标准化TeamExp和PointsAjust

# 5. 创建工作流
workflow <- workflow() %>% 
  add_recipe(recipe_lm) %>% 
  add_model(lm_model)

# 6. 设置10折交叉验证
set.seed(123)  # 确保可重复性
folds <- vfold_cv(train_data, v = 10)

# 7. 执行交叉验证
cv_results <- workflow %>% 
  fit_resamples(folds)

# 8. 查看交叉验证性能指标
cv_metrics <- cv_results %>% collect_metrics()
print("交叉验证结果：")
print(cv_metrics)

# 9. 在完整训练集上拟合最终模型
final_model <- workflow %>% 
  fit(data = train_data)

# 10. 在测试集上评估
test_predictions <- test_data %>% 
  bind_cols(predict(final_model, new_data = test_data))

test_metrics <- test_predictions %>% 
  metrics(truth = logEffort, estimate = .pred)

print("测试集性能：")
print(test_metrics)

# 11. 查看模型系数（可选）
tidy(final_model) %>% print()


################################################RIDGE
# 3. 创建recipe（数据预处理配方）
ridge_recipe <- recipe(logEffort ~ ., data = train_data) %>%
  # 移除不需要的变量（如原始Effort和Project）
  step_rm(Effort, Project) %>%
  # 对所有数值预测变量进行中心化和标准化
  step_normalize(all_numeric_predictors())

# 4. 创建模型规格
ridge_spec <- linear_reg(penalty = tune(), mixture = 0) %>%  # mixture=0表示纯ridge回归
  set_engine("glmnet")

# 5. 创建工作流
ridge_workflow <- workflow() %>%
  add_recipe(ridge_recipe) %>%
  add_model(ridge_spec)

# 6. 设置交叉验证和参数网格
set.seed(123)
train_folds <- vfold_cv(train_data, v = 5)  # 5折交叉验证

ridge_grid <- grid_regular(penalty(), levels = 50)  # 创建50个penalty值的网格

# 7. 调参
ridge_tune <- tune_grid(
  ridge_workflow,
  resamples = train_folds,
  grid = ridge_grid
)

# 8. 选择最佳模型
best_ridge <- select_best(ridge_tune, metric = "rmse")

# 9. 最终拟合
final_ridge <- finalize_workflow(ridge_workflow, best_ridge)
final_fit <- fit(final_ridge, data = train_data)

# 10. 在测试集上评估
test_results <- test_data %>%
  bind_cols(predict(final_fit, new_data = test_data)) %>%
 mutate(Effort_pred = exp(.pred))  # 将log转换的预测值转换回原始尺度

# 计算测试集性能指标
test_metrics <- test_results %>%
  metrics(truth = Effort, estimate = Effort_pred)

# 打印测试结果
test_metrics

# 11. 可选：查看重要变量
tidy(final_fit) %>% arrange(desc(abs(estimate)))


#####################################################ENET

# 加载必要的库
library(tidymodels)
library(readr)

# 1. 读取数据
data <- read_csv("desharnais77CH-1.csv") %>% 
  mutate(logEffort = log(Effort))  # 对Effort取对数

# 2. 划分训练集和测试集（Project 1-7为测试集）
train_data <- data %>% filter(!Project %in% 1:7)
test_data <- data %>% filter(Project %in% 1:7)

# 3. 创建recipe（数据预处理配方）
enet_recipe <- recipe(logEffort ~ ., data = train_data) %>%
  # 移除不需要的变量（保留Project可用于分组分析时可去掉这行）
  step_rm(Effort, Project) %>%
  # 对所有数值预测变量进行中心化和标准化（对正则化方法很重要）
  step_normalize(all_numeric_predictors())

# 4. 创建Elastic Net模型规格
# mixture参数控制L1/L2比例（0=ridge，1=lasso，0<mixture<1=elastic net）
enet_spec <- linear_reg(
  penalty = tune(),  # 正则化强度
  mixture = tune()   # 弹性网混合参数
) %>% 
  set_engine("glmnet")

# 5. 创建工作流
enet_workflow <- workflow() %>%
  add_recipe(enet_recipe) %>%
  add_model(enet_spec)

# 6. 设置交叉验证和参数网格
set.seed(123)
train_folds <- vfold_cv(train_data, v = 5)  # 5折交叉验证

# 创建参数网格（同时调优penalty和mixture）
enet_grid <- grid_regular(
  penalty(),
  mixture(),
  levels = c(penalty = 20, mixture = 10)  # penalty 20个值，mixture 10个值
)

# 7. 调参
enet_tune <- tune_grid(
  enet_workflow,
  resamples = train_folds,
  grid = enet_grid,
  metrics = metric_set(rmse, rsq)  # 监控RMSE和R平方
)

# 8. 可视化调参结果
autoplot(enet_tune) + 
  theme_minimal()

# 9. 选择最佳模型（按RMSE选择）
best_enet <- select_best(enet_tune, metric = "rmse")

# 10. 最终拟合
final_enet <- finalize_workflow(enet_workflow, best_enet)
final_fit <- fit(final_enet, data = train_data)

# 11. 在测试集上评估
test_results <- test_data %>%
  bind_cols(predict(final_fit, new_data = test_data)) %>%
  mutate(
    Effort_pred = exp(.pred),  # 将log转换的预测值转换回原始尺度
    Residual = Effort - Effort_pred  # 计算残差
  )

# 计算测试集性能指标
test_metrics <- test_results %>%
  metrics(truth = Effort, estimate = Effort_pred)

# 打印测试结果
print(test_metrics)

# 12. 查看模型系数
final_coefs <- tidy(final_fit) %>% 
  arrange(desc(abs(estimate)))

print(final_coefs)

# 13. 可选：比较不同mixture值的效果
compare_mixtures <- enet_tune %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  ggplot(aes(x = penalty, y = mean, color = factor(mixture))) +
  geom_line() +
  scale_x_log10() +
  labs(title = "Elastic Net Performance by Mixture Value",
       color = "Mixture (L1 Ratio)") +
  theme_minimal()

print(compare_mixtures)


############################################RF  模型失败
# 1. 读取数据并明确列名
data <- read_csv("desharnais77.csv") %>%
  mutate(Project = as.factor(Project))

# 确认实际存在的列名
cat("可用列名:\n")
print(names(data))

# 2. 安全划分数据集（先划分再预处理）
train_data <- data %>% filter(!Project %in% 1:7)
test_data <- data %>% filter(Project %in% 1:7)

# 3. 创建安全的预处理配方（仅使用确实存在的变量）
# 确定实际可用的预测变量（排除Effort和Project）
available_vars <- setdiff(names(train_data), c("Effort", "Project"))
cat("\n将使用的预测变量:\n")
print(available_vars)

data_recipe <- recipe(train_data) %>%
  update_role(all_of(available_vars), new_role = "predictor") %>%
  update_role(Effort, new_role = "outcome") %>%
  update_role(Project, new_role = "ID") %>%  # 标记为ID列不用于建模
  step_rm(Project) %>%  # 明确移除
  step_log(Effort, base = 10, offset = 1) %>%
  step_nzv(all_numeric_predictors()) %>%
  step_corr(all_numeric_predictors(), threshold = 0.9) %>%
  step_normalize(all_numeric_predictors())

# 4. 检查配方变量
cat("\n配方中使用的变量:\n")
print(summary(data_recipe))

# 5. 预处理应用
prepped_recipe <- prep(data_recipe, training = train_data)
train_processed <- bake(prepped_recipe, new_data = train_data)
test_processed <- bake(prepped_recipe, new_data = test_data)

# 6. 定义随机森林模型
rf_model <- rand_forest(
  mtry = tune(),
  trees = 500,  # 固定树的数量加速调优
  min_n = tune()
) %>%
  set_mode("regression") %>%
  set_engine("ranger")

# 7. 创建工作流
rf_workflow <- workflow() %>%
  add_recipe(data_recipe) %>%
  add_model(rf_model)

# 8. 设置精简的调优网格（确保快速运行）
set.seed(123)
rf_grid <- grid_regular(
  mtry(range = c(2, min(10, length(available_vars)))),
  min_n(range = c(2, 10)),
  levels = 3
)

# 9. 运行带错误检查的交叉验证
tryCatch({
  rf_tune <- tune_grid(
    rf_workflow,
    resamples = vfold_cv(train_processed, v = 3),  # 使用3折加速
    grid = rf_grid,
    metrics = metric_set(rmse),
    control = control_grid(verbose = TRUE)
  )
  
  # 10. 检查调优结果
  if(all(is.na(rf_tune$.metrics[[1]]$rmse))) {
    stop("所有模型都失败了，请检查预处理步骤")
  }
  
  # 11. 成功时继续
  best_params <- select_best(rf_tune, metric = "rmse")
  final_rf <- finalize_workflow(rf_workflow, best_params) %>%
    fit(data = train_processed)
  
  # 12. 测试集评估
  test_results <- test_processed %>%
    bind_cols(predict(final_rf, new_data = test_processed)) %>%
    rename(predicted = .pred)
  
  test_metrics <- metric_set(rmse)(test_results, truth = Effort, estimate = predicted)
  
  # 打印结果
  cat("\n=== 最终结果 ===\n")
  print(best_params)
  print(test_metrics)
  
  # 可视化
  ggplot(test_results, aes(Effort, predicted)) +
    geom_point() +
    geom_abline(slope = 1, color = "red") +
    ggtitle("实际值 vs 预测值")
  
}, error = function(e) {
  cat("\n!!! 错误详情 !!!\n")
  print(e)
  cat("\n预处理后的训练数据列名:\n")
  print(names(train_processed))
  cat("\n工作流使用的变量:\n")
  print(rf_workflow$pre$actions$recipe$recipe$var_info)
})