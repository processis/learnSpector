#1.加载必要的包
library(tidymodels)  # 加载核心tidymodels包
library(readr)       # 用于读取数据
library(vip)         # 用于变量重要性分析

#2.读取数据
# 读取数据
data <- read_csv("/media/user/娱乐/learnSpector/TmwR2506/cleanDesharnais77.csv")  # 如果没有readr，也可以用read.csv()

# 查看数据结构
glimpse(data)

# 划分训练集和测试集
set.seed(123)  # 确保可重复性
data_split <- initial_split(data, prop = 0.75)  # 75%训练，25%测试
train_data <- training(data_split)
test_data <- testing(data_split)


#3. 创建预处理流程(recipes)

# 创建一个基础recipe
# 假设我们的目标变量是"response"，其他都是预测变量
recipe_base <- recipe(Effort ~ TeamExp + PointsNonAdjust + Language, data = train_data) %>%
  step_naomit(all_predictors()) %>%  # 删除有缺失值的行
  step_normalize(all_numeric_predictors()) %>%  # 标准化数值变量
  step_dummy(all_nominal_predictors())  # 将分类变量转换为虚拟变量

# 查看预处理后的数据
prepped_data <- prep(recipe_base, training = train_data) %>% juice()
glimpse(prepped_data)

#4. 定义模型
#线性回归模型
lm_model <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")  # 对于分类问题可以用"classification"

#随机森林模型

rf_model <- rand_forest(
  mtry = tune(),         # 将在调优中确定
  trees = 1000,          # 树的数量
  min_n = tune()         # 节点最小观测数
) %>%
  set_engine("ranger", importance = "permutation") %>%  # 计算变量重要性
  set_mode("regression")

#5. 创建工作流
# 线性回归工作流
lm_workflow <- workflow() %>%
  add_recipe(recipe_base) %>%
  add_model(lm_model)

# 随机森林工作流
rf_workflow <- workflow() %>%
  add_recipe(recipe_base) %>%
  add_model(rf_model)

#6. 模型训练与评估

#线性回归

# 拟合模型
lm_fit <- lm_workflow %>%
  fit(data = train_data)

# 查看模型摘要
lm_fit %>% 
  extract_fit_parsnip() %>% 
  tidy()

# 在测试集上评估
lm_results <- test_data %>%
  bind_cols(predict(lm_fit, new_data = test_data)) %>%
  metrics(truth = Effort, estimate = .pred)

print(lm_results)

#随机森林（带调优）

# 设置交叉验证
set.seed(123)
folds <- vfold_cv(train_data, v = 5)

# 定义参数网格
rf_grid <- grid_regular(
  mtry(range = c(2, 10)),
  min_n(range = c(2, 20)),
  levels = 5
)

# 调优
rf_tune <- tune_grid(
  rf_workflow,
  resamples = folds,
  grid = rf_grid,
  metrics = metric_set(rmse, rsq)
)

# 查看最佳参数
show_best(rf_tune, metric = "rmse")

# 选择最佳参数
best_rf_params <- select_best(rf_tune, metric = "rmse")

# 使用最佳参数训练最终模型
final_rf_workflow <- rf_workflow %>%
  finalize_workflow(best_rf_params)

final_rf_fit <- final_rf_workflow %>%
  fit(data = train_data)

# 查看变量重要性
final_rf_fit %>%
  extract_fit_parsnip() %>%
  vip()

# 在测试集上评估
rf_results <- test_data %>%
  bind_cols(predict(final_rf_fit, new_data = test_data)) %>%
  metrics(truth = Effort, estimate = .pred)

print(rf_results)

#7. 比较模型性能

# 比较两个模型的RMSE
bind_rows(
  lm_results %>% mutate(model = "Linear Regression"),
  rf_results %>% mutate(model = "Random Forest")
) %>%
  filter(.metric == "rmse") %>%
  select(model, .estimate)

#8. 保存和加载模型

# 保存模型
saveRDS(final_rf_fit, "final_rf_model.rds")

# 加载模型
loaded_model <- readRDS("final_rf_model.rds")

# 使用加载的模型进行预测
predict(loaded_model, new_data = test_data)



