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