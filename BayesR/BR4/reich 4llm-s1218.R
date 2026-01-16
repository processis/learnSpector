# 加载必要库
library(rjags)
library(tidyr)
library(dplyr)

# 读取数据
data_file <- "cherry_blossom_sample修改.csv"
df <- read.csv(data_file)

# 删除缺失值
df_clean <- df[!is.na(df$net), ]

# 创建个体标识符 - 假设age序列重新开始时是新个体
df_clean$id <- cumsum(c(1, diff(df_clean$age) <= 0))

# 查看每个个体的观测数量
obs_per_id <- table(df_clean$id)
print("每个个体的观测数量:")
print(obs_per_id)

# 为了匹配原程序的结构，我们选择至少有3个观测值的个体
# 并且只使用这些个体进行建模
valid_ids <- names(obs_per_id[obs_per_id >= 3])
df_valid <- df_clean[df_clean$id %in% valid_ids, ]

# 将数据转换为宽格式
# 首先，我们需要为每个个体创建时间点标识
df_valid <- df_valid %>%
  group_by(id) %>%
  mutate(time_point = rank(age)) %>%
  ungroup()

# 转换为宽格式
wide_data <- df_valid %>%
  select(id, age, net, time_point) %>%
  pivot_wider(
    id_cols = id,
    names_from = time_point,
    values_from = c(age, net),
    names_sep = "_"
  )

# 查看宽格式数据
print(head(wide_data))

# 提取年龄矩阵和响应矩阵
# 我们需要找出所有个体共有的时间点数量
# 但注意，不同个体的时间点数量可能不同

# 更简单的方法：使用长格式直接建模，调整JAGS模型












# 使用长格式数据构建模型
# 首先准备数据

# 创建唯一的个体ID（数值型）
df_clean$subject <- as.numeric(as.factor(df_clean$id))

# 准备JAGS数据
data_jags <- list(
  Y = df_clean$net,
  age = df_clean$age,
  subject = df_clean$subject,
  n = length(unique(df_clean$subject)),  # 个体数量
  N = nrow(df_clean)                     # 总观测数
)

# 检查数据
print(paste("个体数量:", data_jags$n))
print(paste("总观测数:", data_jags$N))

# 修改JAGS模型以适应不平衡设计
model_string <- textConnection("model{
  # Likelihood for unbalanced design
  for(i in 1:N){
    Y[i] ~ dnorm(alpha[subject[i], 1] + alpha[subject[i], 2] * age[i], taue)
  }
  
  # Random effects
  for(i in 1:n){
    alpha[i, 1:2] ~ dmnorm(mu[1:2], Omega[1:2, 1:2])
  }
  
  # Priors
  for(j in 1:2){
    mu[j] ~ dnorm(0, 0.0001)
  }
  taue ~ dgamma(0.1, 0.1)
  Omega[1:2, 1:2] ~ dwish(R[,], 2.1)
  
  R[1,1] <- 1/2.1
  R[1,2] <- 0
  R[2,1] <- 0
  R[2,2] <- 1/2.1
}")

# 设置参数
params <- c("mu", "alpha", "taue", "Omega")
burn <- 10000
n.iter <- 20000
thin <- 10
n.chains <- 2

# 运行模型
model <- jags.model(model_string, data = data_jags, n.chains = n.chains, quiet = TRUE)
update(model, burn, progress.bar = "none")
samples <- coda.samples(model, variable.names = params,
                        n.iter = n.iter, thin = thin, progress.bar = "none")


summary(samples)

# 合并链
samples_combined <- rbind(samples[[1]], samples[[2]])

# 提取参数
Omega <- samples_combined[, 1:4]
alpha_intercept <- samples_combined[, 5:(4 + data_jags$n)]
alpha_slope <- samples_combined[, (5 + data_jags$n):(4 + 2 * data_jags$n)]
mu <- samples_combined[, (5 + 2 * data_jags$n - 1):(4 + 2 * data_jags$n + 1)]
tau_e <- samples_combined[, 5 + 2 * data_jags$n + 1]
sig <- 1/sqrt(tau_e)

# 计算随机效应的协方差矩阵和相关矩阵
n_samples <- nrow(Omega)
S <- matrix(NA, nrow = n_samples, ncol = 4)
r <- numeric(n_samples)

for(i in 1:n_samples){
  Omega_mat <- matrix(Omega[i, ], 2, 2)
  S_mat <- solve(Omega_mat)
  S[i, ] <- as.vector(S_mat)
  r[i] <- S_mat[1, 2] / sqrt(S_mat[1, 1] * S_mat[2, 2])
}

# 绘制相关系数的后验分布
hist(r, breaks = 50, prob = TRUE, main = "随机截距和斜率的相关性后验分布",
     xlab = "截距和斜率之间的相关系数", col = "lightblue", border = "white")

# 添加先验信息的参考线（如果适用）
abline(v = mean(r), col = "red", lwd = 2)
abline(v = quantile(r, c(0.025, 0.975)), col = "blue", lty = 2)

# 选择几个个体进行可视化
# 获取每个个体的唯一年龄范围
subject_ages <- split(df_clean$age, df_clean$subject)
subject_nets <- split(df_clean$net, df_clean$subject)

# 选择几个有代表性的个体
these <- c(1, 5, 10)  # 选择前三个个体

# 创建年龄序列用于预测
age_range <- range(df_clean$age, na.rm = TRUE)
ages_seq <- seq(age_range[1], age_range[2], length.out = 50)

# 绘制图形
plot(NA, xlim = age_range, ylim = range(df_clean$net, na.rm = TRUE),
     xlab = "Age", ylab = "Net", main = "个体生长曲线")

colors <- c("red", "blue", "green")

for(sub_idx in 1:length(these)){
  sub <- these[sub_idx]
  col <- colors[sub_idx]
  
  # 绘制观测数据点
  points(subject_ages[[sub]], subject_nets[[sub]], pch = 19, col = col)
  
  # 计算后验预测
  n_samples <- nrow(alpha_intercept)
  pred_matrix <- matrix(NA, nrow = n_samples, ncol = length(ages_seq))
  
  for(j in 1:length(ages_seq)){
    pred_matrix[, j] <- alpha_intercept[, sub] + alpha_slope[, sub] * ages_seq[j]
  }
  
  # 计算分位数
  q_lower <- apply(pred_matrix, 2, quantile, 0.025)
  q_median <- apply(pred_matrix, 2, quantile, 0.5)
  q_upper <- apply(pred_matrix, 2, quantile, 0.975)
  
  # 绘制曲线
  lines(ages_seq, q_median, col = col, lwd = 2)
  lines(ages_seq, q_lower, col = col, lty = 2)
  lines(ages_seq, q_upper, col = col, lty = 2)
  
  # 在最大年龄处添加预测区间
  max_age <- max(ages_seq)
  pred_max_age <- alpha_intercept[, sub] + alpha_slope[, sub] * max_age + 
    rnorm(n_samples, 0, sig)
  
  q_pred <- quantile(pred_max_age, c(0.025, 0.975))
  
  lines(c(max_age, max_age), q_pred, col = col, lwd = 2)
  lines(max_age + 0.5 * c(-1, 1), rep(q_pred[1], 2), col = col, lwd = 2)
  lines(max_age + 0.5 * c(-1, 1), rep(q_pred[2], 2), col = col, lwd = 2)
}

# 添加图例
legend("topleft", legend = paste("个体", these), 
       col = colors[1:length(these)], pch = 19, lwd = 2, cex = 1.2, bty = "n")

# 总结结果
cat("固定效应（总体均数）的后验均值：\n")
cat("截距:", mean(mu[, 1]), "\n")
cat("斜率:", mean(mu[, 2]), "\n\n")

cat("随机效应方差-协方差矩阵的后验均值：\n")
S_mean <- matrix(colMeans(S), 2, 2)
colnames(S_mean) <- rownames(S_mean) <- c("截距", "斜率")
print(S_mean)
cat("\n")

cat("截距和斜率相关系数的后验均值：", mean(r), "\n")
cat("95%可信区间：", quantile(r, c(0.025, 0.975)), "\n")