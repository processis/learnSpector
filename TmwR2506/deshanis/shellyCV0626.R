# 加载必要的包
library(glmnet)

# 创建弹性网络模型
en_model <- 
  linear_reg(penalty = tune(), mixture = tune()) %>% 
  set_engine("glmnet")

# 创建工作流
en_wflow <- 
  workflow() %>% 
  add_model(en_model) %>% 
  add_recipe(rf_recipe)  # 使用之前定义的rf_recipe

# 设置10折交叉验证
set.seed(502)
tb_folds_10 <- vfold_cv(tbDesharnais_train, v = 10)

# 设置调参网格
en_grid <- grid_regular(
  penalty(range = c(-5, 1)),  # lambda范围(10^-5到10^1)
  mixture(range = c(0, 1)),    # alpha范围(0到1)
  levels = 10                  # 每个参数的尝试级别数
)

# 调参
en_tune <- tune_grid(
  en_wflow,
  resamples = tb_folds_10,
  grid = en_grid,
  metrics = metric_set(rmse, rsq)
)

# 查看最佳参数
show_best(en_tune, metric = "rmse")

# 选择最佳参数
best_en <- select_best(en_tune, metric = "rmse")

# 最终化工作流
final_en_wflow <- 
  en_wflow %>% 
  finalize_workflow(best_en)

# 训练最终模型
final_en_fit <- fit(final_en_wflow, tbDesharnais_train)

# 在测试集上评估
en_test_results <- tbDesharnais_test %>% 
  bind_cols(predict(final_en_fit, tbDesharnais_test)) %>% 
  rename(predicted_effort = .pred)

# 计算性能指标
en_test_metrics <- metric_set(rmse, rsq)(en_test_results, 
                                         truth = Effort, 
                                         estimate = predicted_effort)
print(en_test_metrics)

# 可视化调参结果
autoplot(en_tune) + 
  theme_minimal() +
  ggtitle("Elastic Net Tuning Results")

# 查看系数
final_en_fit %>% 
  extract_fit_parsnip() %>% 
  tidy()

# 预测新数据示例
predict(final_en_fit, tbDesharnais_test %>% slice(1:7))

# 比较不同模型性能
comparison <- bind_rows(
  lm_fit %>% 
    predict(new_data = tbDesharnais_test) %>% 
    bind_cols(tbDesharnais_test %>% select(Effort)) %>% 
    mutate(model = "Linear Regression"),
  
  final_rf_fit %>% 
    predict(new_data = tbDesharnais_test) %>% 
    bind_cols(tbDesharnais_test %>% select(Effort)) %>% 
    mutate(model = "Random Forest"),
  
  final_en_fit %>% 
    predict(new_data = tbDesharnais_test) %>% 
    bind_cols(tbDesharnais_test %>% select(Effort)) %>% 
    mutate(model = "Elastic Net")
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