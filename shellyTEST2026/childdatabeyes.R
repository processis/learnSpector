# 加载必要的包
library(tidyverse)
library(rstanarm)
library(bayesplot)
library(BayesFactor)
library(ggplot2)
library(GGally)
library(corrplot)
library(performance)
# 设置随机种子保证可重复性
set.seed(123)
# 1. 读取数据（这里使用内置数据集mtcars作为示例）
data <- mtcars
str(data)
head(data)
# 2. 基本描述性统计
summary(data)
# 3. 直方图（数值变量分布）
par(mfrow = c(2, 3))
hist(data$mpg, main = "MPG分布", xlab = "MPG", col = "lightblue", breaks = 15)
hist(data$hp, main = "马力分布", xlab = "Horsepower", col = "lightgreen", breaks = 15)
hist(data$wt, main = "重量分布", xlab = "Weight", col = "lightcoral", breaks = 15)
# 4. 散点图矩阵
pairs(data[, c("mpg", "hp", "wt", "disp")], 
      main = "散点图矩阵",
      col = "darkblue",
      pch = 19)
# 使用GGally包创建更美观的散点图矩阵
ggpairs(data[, c("mpg", "hp", "wt", "disp")],
        title = "散点图矩阵和相关矩阵")
# 5. 频率主义回归分析（作为对比）
freq_lm <- lm(mpg ~ hp + wt + cyl, data = data)
summary(freq_lm)
# 6. 贝叶斯线性回归
# 使用rstanarm包进行贝叶斯回归
bayes_lm <- stan_glm(
  mpg ~ hp + wt + cyl,
  data = data,
  family = gaussian(),
  prior = normal(0, 2.5),      # 回归系数的先验分布
  prior_intercept = normal(20, 5),  # 截距的先验
  prior_aux = exponential(1),  # 残差方差的先验
  chains = 4,                  # MCMC链数
  iter = 2000,                 # 迭代次数
  warmup = 500,                # 预烧期
  seed = 123
)
# 查看贝叶斯回归结果
print(bayes_lm, digits = 3)
summary(bayes_lm)
# 7. 贝叶斯回归诊断
# 后验分布图
posterior <- as.array(bayes_lm)
mcmc_areas(posterior, 
           pars = c("hp", "wt", "cyl"),
           prob = 0.95) +
  ggtitle("后验分布（95%可信区间）")
# 迹线图
mcmc_trace(posterior, pars = c("hp", "wt", "cyl"))
# 自相关图
mcmc_acf(posterior, pars = c("hp", "wt", "cyl"))
# 8. 斯皮尔曼相关（频率方法，作为参考）
cor_spearman <- cor(data[, c("mpg", "hp", "wt", "disp")], 
                    method = "spearman")
print("斯皮尔曼相关系数矩阵：")
print(cor_spearman)
# 可视化相关矩阵
corrplot(cor_spearman, 
         method = "color",
         type = "upper",
         title = "斯皮尔曼相关矩阵",
         mar = c(0,0,1,0))
# 9. 贝叶斯相关分析
# 使用BayesFactor包
if(require(BayesFactor)) {
  # 贝叶斯相关分析
  bayes_cor <- correlationBF(data$mpg, data$hp)
  print(bayes_cor)
  
  # 获取后验分布
  posterior_cor <- correlationBF(data$mpg, data$hp, posterior = TRUE)
  hist(posterior_cor, main = "相关后验分布", xlab = "相关系数")
}
# 10. 方差分析（ANOVA）
# 频率方法ANOVA
freq_anova <- aov(mpg ~ factor(cyl) + factor(am), data = data)
summary(freq_anova)
# 贝叶斯ANOVA
bayes_anova <- stan_glm(
  mpg ~ factor(cyl) + factor(am),
  data = data,
  family = gaussian(),
  prior = normal(0, 2.5),
  chains = 4,
  iter = 2000
)
print(bayes_anova, digits = 3)
# 11. 卡方检验（频率方法）
# 创建列联表
contingency_table <- table(data$cyl, data$am)
print("列联表：")
print(contingency_table)
# 卡方检验
chi_test <- chisq.test(contingency_table)
print(chi_test)
# 可视化列联表
mosaicplot(contingency_table,
           main = "气缸数与变速箱类型的列联表",
           color = TRUE,
           shade = TRUE)
# 12. 逻辑回归
# 将mpg转换为二分类变量（高/低油耗）
data$mpg_binary <- ifelse(data$mpg > median(data$mpg), 1, 0)
# 频率主义逻辑回归
freq_logit <- glm(mpg_binary ~ hp + wt,
                  data = data,
                  family = binomial())
summary(freq_logit)
# 贝叶斯逻辑回归
bayes_logit <- stan_glm(
  mpg_binary ~ hp + wt,
  data = data,
  family = binomial(link = "logit"),
  prior = normal(0, 2.5),
  prior_intercept = normal(0, 2.5),
  chains = 4,
  iter = 2000,
  seed = 123
)
# 查看贝叶斯逻辑回归结果
print(bayes_logit, digits = 3)
# 后验预测检查
pp_check(bayes_logit) +
  ggtitle("贝叶斯逻辑回归后验预测检查")
# 13. 模型比较
# 比较不同贝叶斯模型
bayes_lm_simple <- stan_glm(mpg ~ hp, data = data, 
                            family = gaussian(),
                            prior = normal(0, 2.5))
bayes_lm_complex <- stan_glm(mpg ~ hp + wt + cyl + disp, 
                             data = data,
                             family = gaussian(),
                             prior = normal(0, 2.5))
# 使用留一交叉验证(LOO)比较模型
if(require(loo)) {
  loo_simple <- loo(bayes_lm_simple)
  loo_complex <- loo(bayes_lm_complex)
  print(loo_compare(loo_simple, loo_complex))
}
# 14. 预测新数据
# 创建新数据进行预测
new_data <- data.frame(
  hp = c(100, 150, 200),
  wt = c(2.5, 3.0, 4.0),
  cyl = c(4, 6, 8)
)
# 使用贝叶斯模型进行预测
predictions <- posterior_predict(bayes_lm, newdata = new_data)
# 计算预测的汇总统计
pred_summary <- apply(predictions, 2, function(x) {
  c(mean = mean(x),
    sd = sd(x),
    lower = quantile(x, 0.025),
    upper = quantile(x, 0.975))
})
print("新数据预测结果：")
print(pred_summary)
# 15. 保存结果
# 保存模型对象
saveRDS(bayes_lm, "bayes_linear_model.rds")
saveRDS(bayes_logit, "bayes_logistic_model.rds")
# 保存图形
pdf("bayesian_analysis_plots.pdf")
# 重新生成关键图形
mcmc_areas(posterior, pars = c("hp", "wt", "cyl"))
mcmc_trace(posterior, pars = c("hp", "wt", "cyl"))
dev.off()
# 16. 创建分析报告摘要
cat("\n=== 贝叶斯分析报告摘要 ===\n")
cat("1. 贝叶斯线性回归模型:\n")
cat("   - 参数后验均值:\n")
print(round(posterior_interval(bayes_lm, prob = 0.95), 3))
cat("\n2. 模型诊断:\n")
cat("   - R-hat统计量 (应接近1):\n")
print(round(summary(bayes_lm)[, "Rhat"], 3))
cat("\n3. 贝叶斯逻辑回归:\n")
cat("   - 预测准确性:\n")
# 计算分类准确率
pred_prob <- posterior_linpred(bayes_logit, transform = TRUE)
pred_class <- ifelse(colMeans(pred_prob) > 0.5, 1, 0)
accuracy <- mean(pred_class == data$mpg_binary)
cat(paste("     准确率:", round(accuracy * 100, 2), "%\n"))
# 重置图形参数
par(mfrow = c(1, 1))