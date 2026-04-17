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
datachl <- read.csv("/media/user/软件/learnSpector/shellyTEST2026/儿童发育数据.csv")
str(datachl)
head(datachl)

# 2. 基本描述性统计
summary(datachl)

# 3. 直方图（数值变量分布）

hist(datachl$month, main = "month分布", col = "lightblue")
hist(datachl$height, main = "heigth分布", col = "lightgreen")


# 4. 散点图矩阵
pairs(datachl[, c("month", "height")], 
      main = "散点图矩阵",
      col = "darkblue")

# 5. 频率主义回归分析（作为对比）
datachlfreq_lm <- lm(height ~ month, data = datachl)
summary(datachlfreq_lm)

# 6. 贝叶斯线性回归
# 使用rstanarm包进行贝叶斯回归
bayeschl_lm <- stan_glm(
  height ~ month,
  data = datachl,
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
print(bayeschl_lm, digits = 3)
summary(bayeschl_lm)

# 7. 贝叶斯回归诊断
# 后验分布图
posterior <- as.array(bayeschl_lm)
mcmc_areas(posterior, 
           pars = c("height", "month"),
           prob = 0.95) +
  ggtitle("后验分布（95%可信区间）")
# 迹线图
mcmc_trace(posterior, pars = c("height", "month"))
# 自相关图
mcmc_acf(posterior, pars = c("height", "month"))



