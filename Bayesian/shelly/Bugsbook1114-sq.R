library(rjags)
library(coda)

library(ggplot2)
library(reshape2)


#########5.3.1

data <-  list(y = 10, n = 100)




# 初始值
inits_list <- list(
  list(alpha = 40, beta = 1),
  list(alpha = mean(data_list$y), beta = 1/sd(data_list$y)^2)
)


# JAGS模型代码
model_string <- textConnection("model {
tau ~ dgamma(20, 2000)
sigma <- 1/sqrt(tau)
theta ~ dnorm(5, 0.25)
n <- 2*pow((1.28 + 1.96)*sigma/theta, 2) # n for 90% power
power <- phi(sqrt(84/2)*theta/sigma - 1.96) # power for n = 84
p70 <- step(power - 0.7) # Pr(power > 70%)
}
")



#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("tau","sigma","theta","n","power","p70")
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)



############5.3.2



data <-  list(y = 10, n = 100)




# 初始值
inits_list <- list(
  list(alpha = 40, beta = 1),
  list(alpha = mean(data_list$y), beta = 1/sd(data_list$y)^2)
)


# JAGS模型代码
model_string <- textConnection("model {
# tau ~ dgamma(20, 2000)
tau ~ dgamma(10, 1000) # discounted by 2
# theta ~ dnorm(5, 0.25)
theta ~ dnorm(4, 0.125)I(0,) # 4 added to var and shifted
# by -1, constrained to be >0
sigma <- 1/sqrt(tau)
n <- 2*pow((1.28 + 1.96)*sigma/theta, 2) # n for 90% power
power <- phi(sqrt(84/2)*theta/sigma - 1.96) # power for n = 84
p70 <- step(power - 0.7) # Pr(power > 70%)
}
")



#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("tau","sigma","theta","n","power","p70")
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)




#############5.4.1





data <-  list(y = 10, n = 100)




# 初始值
inits_list <- list(
  list(alpha = 40, beta = 1),
  list(alpha = mean(data_list$y), beta = 1/sd(data_list$y)^2)
)


# JAGS模型代码
model_string <- textConnection("model {
r <- 15; n <- 20 # data
######################################
r ~ dbin(p, n) # likelihood
p <- theta[pick]
pick ~ dcat(q[])
q[1] <- 0.9
q[2] <- 0.1
theta[1] <- 0.5 # if unbiased
theta[2] ~ dunif(0, 1) # if biased
biased <- pick - 1 # 1 if biased, 0 otherwise
}
")



#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("tau","sigma","theta","n","power","p70")
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)


#############5.5.2


data <-  list(M = 5000)




# 初始值
inits_list <- list(
  list(alpha = 40, beta = 1),
  list(alpha = mean(data_list$y), beta = 1/sd(data_list$y)^2)
)


# JAGS模型代码
model_string <- textConnection("model {
Y <- 100
########################
Y ~ dcat(p[])
# sampling distribution is uniform over first N integers
# use step function to change p[j] to 0 for j>N
for (j in 1:M) {
p[j] <- step(N - j + 0.01)/N
}
N ~ dcat(p.unif[])
for (j in 1:M) {
p.unif[j] <- 1/M
}
}
")



#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("tau","sigma","theta","n","power","p70")
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)


################6.3.1

data <-  list(x = structure(
  .Data = c(1.0, 1.5, 1.5, 1.5, 2.5, 4.0, 5.0, 5.0, 7.0,
            8.0, 8.5, 9.0, 9.5, 9.5, 10.0, 12.0, 12.0, 13.0,
            13.0, 14.5, 15.5, 15.5, 16.5, 17.0, 22.5, 29.0, 31.5,
            1.0, 1.5, 1.5, 1.5, 2.5, 4.0, 5.0, 5.0, 7.0,
            8.0, 8.5, 9.0, 9.5, 9.5, 10.0, 12.0, 12.0, 13.0,
            13.0, 14.5, 15.5, 15.5, 16.5, 17.0, 22.5, 29.0, 31.5,
            1.0, 1.5, 1.5, 1.5, 2.5, 4.0, 5.0, 5.0, 7.0,
            8.0, 8.5, 9.0, 9.5, 9.5, 10.0, 12.0, 12.0, 13.0,
            13.0, 14.5, 15.5, 15.5, 16.5, 17.0, 22.5, 29.0, 31.5,
            1.0, 1.5, 1.5, 1.5, 2.5, 4.0, 5.0, 5.0, 7.0,
            8.0, 8.5, 9.0, 9.5, 9.5, 10.0, 12.0, 12.0, 13.0,
            13.0, 14.5, 15.5, 15.5, 16.5, 17.0, 22.5, 29.0, 31.5),
  .Dim = c(4, 27)),
  y = structure(
    .Data = c(1.80, 1.85, 1.87, 1.77, 2.02, 2.27, 2.15, 2.26, 2.47,
              2.19, 2.26, 2.40, 2.39, 2.41, 2.50, 2.32, 2.32, 2.43,
              2.47, 2.56, 2.65, 2.47, 2.64, 2.56, 2.70, 2.72, 2.57,
              1.80, 1.85, 1.87, 1.77, 2.02, 2.27, 2.15, 2.26, 2.47,
              2.19, 2.26, 2.40, 2.39, 2.41, 2.50, 2.32, 2.32, 2.43,
              2.47, 2.56, 2.65, 2.47, 2.64, 2.56, 2.70, 2.72, 2.57,
              1.80, 1.85, 1.87, 1.77, 2.02, 2.27, 2.15, 2.26, 2.47,
              2.19, 2.26, 2.40, 2.39, 2.41, 2.50, 2.32, 2.32, 2.43,
              2.47, 2.56, 2.65, 2.47, 2.64, 2.56, 2.70, 2.72, 2.57,
              1.80, 1.85, 1.87, 1.77, 2.02, 2.27, 2.15, 2.26, 2.47,
              2.19, 2.26, 2.40, 2.39, 2.41, 2.50, 2.32, 2.32, 2.43,
              2.47, 2.56, 2.65, 2.47, 2.64, 2.56, 2.70, 2.72, 2.57),
    .Dim = c(4, 27)), N = 27)






# 初始值
inits_list <- list(
  list(alpha = c(NA, NA, 3, 3), beta = c(2, NA, 2, 2), gamma = c(NA, NA, 0.9, 0.9), K = c(0.1, 0.1), Linf = c(NA, 3), L0 = c(1, 1), log.sigma = c(-5, 0, -5, -5))
  
 # list(alpha = mean(data_list$y), beta = 1/sd(data_list$y)^2)
)


# JAGS模型代码
model_string <- textConnection("model {
for(j in 1:N) {
for (i in 1:4) {
y[i,j] ~ dnorm(mu[i,j], tau[i])
}
mu[1,j] <- Linf[1] - (Linf[1] - L0[1])*exp(-K[1]*x[1,j])
mu[2,j] <- Linf[2] - (Linf[2] - L0[2])*exp(-K[2]*x[2,j])
mu[3,j] <- alpha[3] - beta[3]*pow(gamma[3], x[3,j])
mu[4,j] <- alpha[4] - beta[4]*pow(gamma[4], x[4,j])
}
L0[1] ~ dunif(0, 100)
L0[2] ~ dnorm(0, 0.0001)I(0, Linf[2])
Linf[1] <- L0[1] + beta[1]
Linf[2] ~ dnorm(0, 0.0001)I(L0[2], )
K[1] ~ dunif(0, 100)
K[2] ~ dunif(0, 100)
for (i in 1:2) {alpha[i] <- Linf[i]}
for (i in 3:4) {alpha[i] ~ dunif(0, 100)}
beta[1] ~ dunif(0, 100)
beta[2] <- Linf[2] - L0[2]
for (i in 3:4) {beta[i] ~ dunif(0, 100)}
for (i in 1:2) {gamma[i] <- exp(-K[i])}
gamma[3] ~ dunif(0, 1)
gamma[4] ~ dgamma(0.001, 0.001)I(0, 1)
for (i in 1:4) {
tau[i] <- 1/sigma2[i]
log(sigma2[i]) <- 2*log.sigma[i]
log.sigma[i] ~ dunif(-10, 10)
}
}

")



#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("alpha","beta","gamma","sigma2")
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)








#############6.4.1         XXXXX

data <-  list(Y = structure(
  .Data = c(47.8, 48.8, 49.0, 49.7,
            46.4, 47.3, 47.7, 48.4,
            46.3, 46.8, 47.8, 48.5,
            45.1, 45.3, 46.1, 47.2,
            47.6, 48.5, 48.9, 49.3,
            52.5, 53.2, 53.3, 53.7,
            51.2, 53.0, 54.3, 54.5,
            49.8, 50.0, 50.3, 52.7,
            48.1, 50.8, 52.3, 54.4,
            45.0, 47.0, 47.3, 48.3,
            51.2, 51.4, 51.6, 51.9,
            48.5, 49.2, 53.0, 55.5,
            52.1, 52.8, 53.7, 55.0,
            48.2, 48.9, 49.3, 49.8,
            49.6, 50.4, 51.2, 51.8,
            50.7, 51.7, 52.7, 53.3,
            47.2, 47.7, 48.4, 49.5,
            53.3, 54.6, 55.1, 55.3,
            46.2, 47.5, 48.1, 48.4,
            46.3, 47.6, 51.3, 51.8),
  .Dim = c(20, 4)),
  x = c(8.0, 8.5, 9.0, 9.5),
  R = structure(
    .Data = c(4, 0, 0, 0,
              0, 4, 0, 0,
              0, 0, 4, 0,
              0, 0, 0, 4),
    .Dim = c(4, 4)))




# 初始值
inits_list <- list(
  list(alpha = 40, beta = 1),
  list(alpha = mean(data_list$y), beta = 1/sd(data_list$y)^2)
)


# JAGS模型代码
model_string <- textConnection("model {
for (i in 1:20) {Y[i, 1:4] ~ dmnorm(mu[], Sigma.inv[,])}
for (j in 1:4) {mu[j] <- alpha + beta*x[j]}
alpha ~ dnorm(0, 0.0001)
beta ~ dnorm(0, 0.0001)
Sigma.inv[1:4, 1:4] ~ dwish(R[,], 4)
Sigma[1:4, 1:4] <- inverse(Sigma.inv[,])
}
")



#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("alpha","beta","Sigma","Sigma.inv")
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)


###########6.5.2  XXXXXXXXXXX

data <- list(y = structure(.Data = c(15,21,29,16,18,21,16,26,33,27,41,60,33,38,41,20,27,42),
                           .Dim = c(6, 3)),
             x = c(0, 10, 33, 100, 333, 1000))



# 初始值
inits_list <- list(
  list(alpha = 0, beta = 0, gamma = 0)
)


# JAGS模型代码
model_string <- textConnection("model {
for (i in 1:6) {
for (j in 1:3) {
y[i,j] ~ dpois(mu[i])
}
log(mu[i]) <- alpha + beta*log(x[i] + 10) + gamma*x[i]
}
for (i in 1:6) {
y.pred[i] ~ dpois(mu[i])
}
alpha ~ dnorm(0, 0.0001)
beta ~ dnorm(0, 0.0001)
gamma ~ dnorm(0, 0.0001)
}
")



#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("alpha","beta","gamma","y.pred","mu")
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)



##################################6.5.2-deepseek
library(R2jags)
library(coda)
library(ggplot2)
library(dplyr)

# 数据（与您的代码相同）
data_jags <- list(
  y = structure(
    .Data = c(15, 21, 29, 16, 18, 21, 16, 26, 33, 27, 41, 60, 33, 38, 41, 20, 27, 42),
    .Dim = c(6, 3)
  ),
  x = c(0, 10, 33, 100, 333, 1000)
)

# 将数据转换为适合绘图的长格式，并计算每个剂量的均值和标准差
n_reps <- 3
dose_rep <- rep(data_jags$x, each = n_reps)
colonies <- as.vector(data_jags$y)
obs_data <- data.frame(dose = dose_rep, colonies = colonies)

# 计算每个剂量的统计量
summary_data <- obs_data %>%
  group_by(dose) %>%
  summarise(
    mean_colonies = mean(colonies),
    sd_colonies = sd(colonies),
    n = n(),
    se_colonies = sd_colonies / sqrt(n)
  )

# 初始值
inits_list <- list(
  list(alpha = 0, beta = 0, gamma = 0),
  list(alpha = 1, beta = 1, gamma = -0.001)
)

# JAGS模型代码（与您的相同）
model_string <- "
model {
  for (i in 1:6) {
    for (j in 1:3) {
      y[i,j] ~ dpois(mu[i])
    }
    log(mu[i]) <- alpha + beta * log(x[i] + 10) + gamma * x[i]
  }
  
  # 后验预测
  for (i in 1:6) {
    y.pred[i] ~ dpois(mu[i])
  }
  
  # 先验分布
  alpha ~ dnorm(0, 0.0001)
  beta ~ dnorm(0, 0.0001)
  gamma ~ dnorm(0, 0.0001)
}
"

# 运行JAGS模型
set.seed(123)
model <- jags.model(
  textConnection(model_string),
  data = data_jags,
  inits = inits_list,
  n.chains = 2,
  quiet = TRUE
)

# Burn-in
update(model, 10000, progress.bar = "none")

# 抽样
params <- c("alpha", "beta", "gamma", "mu", "y.pred")
samples <- coda.samples(
  model,
  variable.names = params,
  n.iter = 20000,
  progress.bar = "none",
  thin = 1
)

# 合并链并提取参数后验样本
samples_combined <- as.matrix(samples)

# 提取参数
alpha_post <- samples_combined[, "alpha"]
beta_post <- samples_combined[, "beta"]
gamma_post <- samples_combined[, "gamma"]

# 创建新剂量的网格用于绘制平滑曲线
plot_doses <- seq(min(data_jags$x), max(data_jags$x), length.out = 200)

# 计算后验预测分布（对于新剂量）
n_samples <- length(alpha_post)
mu_grid <- matrix(NA, nrow = n_samples, ncol = length(plot_doses))

for (i in 1:n_samples) {
  mu_grid[i, ] <- exp(
    alpha_post[i] + 
      beta_post[i] * log(plot_doses + 10) + 
      gamma_post[i] * plot_doses
  )
}

# 计算后验汇总统计量
mu_grid_summary <- data.frame(
  dose = plot_doses,
  mu_mean = apply(mu_grid, 2, mean),
  mu_lower = apply(mu_grid, 2, quantile, probs = 0.025),
  mu_upper = apply(mu_grid, 2, quantile, probs = 0.975)
)

# 获取原始剂量点处的模型拟合值
mu_samples <- samples_combined[, grep("mu\\[", colnames(samples_combined))]
mu_points <- data.frame(
  dose = data_jags$x,
  model_mean = apply(mu_samples, 2, mean),
  model_lower = apply(mu_samples, 2, quantile, probs = 0.025),
  model_upper = apply(mu_samples, 2, quantile, probs = 0.975)
)

# 绘制图形：折线连接形式
ggplot() +
  # 观测数据点（带误差条）
  geom_point(
    data = summary_data,
    aes(x = dose, y = mean_colonies),
    size = 3,
    color = "black"
  ) +
  # 观测数据均值间的折线连接
  geom_line(
    data = summary_data,
    aes(x = dose, y = mean_colonies),
    color = "black",
    linetype = "dashed",
    linewidth = 0.8,
    alpha = 0.7
  ) +
  # 观测数据的误差条（标准差）
  geom_errorbar(
    data = summary_data,
    aes(x = dose, ymin = mean_colonies - sd_colonies, ymax = mean_colonies + sd_colonies),
    width = 15,
    color = "black",
    linewidth = 0.6
  ) +
  # 模型拟合的平滑曲线
  geom_line(
    data = mu_grid_summary,
    aes(x = dose, y = mu_mean),
    color = "red",
    linewidth = 1.5
  ) +
  # 模型拟合的95%可信区间（阴影区域）
  geom_ribbon(
    data = mu_grid_summary,
    aes(x = dose, ymin = mu_lower, ymax = mu_upper),
    alpha = 0.2,
    fill = "red"
  ) +
  # 在原始剂量点处添加模型拟合点
  geom_point(
    data = mu_points,
    aes(x = dose, y = model_mean),
    color = "blue",
    size = 3,
    shape = 17
  ) +
  # 模型拟合点间的折线连接（蓝色虚线）
  geom_line(
    data = mu_points,
    aes(x = dose, y = model_mean),
    color = "blue",
    linetype = "dotted",
    linewidth = 0.8,
    alpha = 0.7
  ) +
  # 坐标轴和标签
  labs(
    title = "菌落数量 vs 剂量",
    x = "剂量",
    y = "菌落数量",
    caption = "黑色点和虚线：观测数据均值及连接线 | 红色实线：模型拟合曲线 | 蓝色三角和虚线：剂量点处的模型拟合值及连接线"
  ) +
  # 主题设置
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    plot.caption = element_text(size = 10, color = "gray40", hjust = 0),
    legend.position = "none"
  ) +
  # 坐标轴设置
  scale_x_continuous(
    breaks = data_jags$x,
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05)),
    limits = c(0, max(summary_data$mean_colonies + summary_data$sd_colonies, 
                      mu_grid_summary$mu_upper) * 1.1)
  )

# 可选：保存图形
# ggsave("colonies_vs_dose_lines.png", width = 10, height = 6, dpi = 300)



########################################



data <- list( max = 1000,
              y = structure(.Data = c(15,21,29,16,18,21,16,26,33,27,41,60,33,38,41,20,27,42),
                            .Dim = c(6, 3)),
              x = c(0, 10, 33, 100, 333, 1000))



# 初始值
inits_list <- list(
  list(alpha = 0, beta = 0, gamma = 10)
)


# JAGS模型代码
model_string <- textConnection("model {
for (i in 1:6) {
for (j in 1:3) {
y[i,j] ~ dnegbin(p[i], r)
}
p[i] <- r/(mu[i] + r)
log(mu[i]) <- alpha + beta*log(x[i] + 10) + gamma*x[i]
}
for (i in 1:6) {
y.pred[i] ~ dnegbin(p[i], r)
}
r ~ dcat(pi[])
for (i in 1:max) {
pi[i] <- 1/max
}
alpha ~ dnorm(0, 0.0001)
beta ~ dnorm(0, 0.0001)
gamma ~ dnorm(0, 0.0001)
}

")



#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("alpha","beta","gamma","y.pred","mu")
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)



























######8.2.1

# pack as a list for JAGS
data <-  list(N=66, y=c(
  28, 26, 33, 24, 34, -44, 27, 16, 40, -2,
  29, 22, 24, 21, 25, 30, 23, 29, 31, 19,
  24, 20, 36, 32, 36, 28, 25, 21, 28, 29,
  37, 25, 28, 26, 30, 32, 36, 26, 30, 22,
  36, 23, 27, 27, 28, 27, 31, 27, 26, 33,
  26, 32, 32, 24, 39, 28, 24, 25, 32, 25,
  29, 27, 28, 29, 16, 23))

# 初始值
#inits_list <- list(
#  list(mu = 0, tau = 1),
#  list(mu = mean(data_list$y), tau = 1/sd(data_list$y)^2)
#)


# JAGS模型代码
model_string <- textConnection("model {
for (i in 1:N) {
y[i] ~ dt(mu, tau ,4)
}
mu ~ dunif(-100, 100)
tau ~ dgamma(0.001, 0.001)
}
")



#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("mu","tau")
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)



############

# pack as a list for JAGS
data <-  list(N=66, y=c(
  28, 26, 33, 24, 34, -44, 27, 16, 40, -2,
  29, 22, 24, 21, 25, 30, 23, 29, 31, 19,
  24, 20, 36, 32, 36, 28, 25, 21, 28, 29,
  37, 25, 28, 26, 30, 32, 36, 26, 30, 22,
  36, 23, 27, 27, 28, 27, 31, 27, 26, 33,
  26, 32, 32, 24, 39, 28, 24, 25, 32, 25,
  29, 27, 28, 29, 16, 23))


# 初始值
#inits_list <- list(
#  list(mu = 0, tau = 1),
#  list(mu = mean(data_list$y), tau = 1/sd(data_list$y)^2)
#)


# JAGS模型代码
model_string <- textConnection("model {
for (i in 1:N) {
y[i] ~ dnorm(mu, invsigma2[i])
invsigma2[i] <- tau*lambda[i]/4
lambda[i] ~ dchisqr(4)
}
mu ~ dunif(-100, 100)
tau ~ dgamma(0.001, 0.001)
}
")



#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("mu","tau","y","invsigma2","lambda")  #, "deviance"
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)



#################8.3.1

data <- list(y=c(41,25,24,23,25,42,24,53,26,25,58,31),
             n=c(143,187,323,122,164,405,239,482,195,177,581,301))





# 初始值
inits_list <- list(
  list(alpha = 40, beta = 1),
  list(alpha = mean(data_list$y), beta = 1/sd(data_list$y)^2)
)


# JAGS模型代码
model_string <- textConnection("model {
for (i in 1:12) {
y[i] ~ dbin(theta, n[i])
res[i] <- (y[i] - n[i]*theta)/sqrt(n[i]*theta*(1-theta))
res2[i] <- res[i]*res[i]
}
theta ~ dunif(0, 1)
X2.obs <- sum(res2[]) # sum of squared stand. resids
}
")



#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("y","res","res2","theta","X2.obs")
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)



##################8.3.2          XXXXXXXXXXXX


data <-  list(Y = structure(
  .Data = c(47.8, 48.8, 49.0, 49.7,
            46.4, 47.3, 47.7, 48.4,
            46.3, 46.8, 47.8, 48.5,
            45.1, 45.3, 46.1, 47.2,
            47.6, 48.5, 48.9, 49.3,
            52.5, 53.2, 53.3, 53.7,
            51.2, 53.0, 54.3, 54.5,
            49.8, 50.0, 50.3, 52.7,
            48.1, 50.8, 52.3, 54.4,
            45.0, 47.0, 47.3, 48.3,
            51.2, 51.4, 51.6, 51.9,
            48.5, 49.2, 53.0, 55.5,
            52.1, 52.8, 53.7, 55.0,
            48.2, 48.9, 49.3, 49.8,
            49.6, 50.4, 51.2, 51.8,
            50.7, 51.7, 52.7, 53.3,
            47.2, 47.7, 48.4, 49.5,
            53.3, 54.6, 55.1, 55.3,
            46.2, 47.5, 48.1, 48.4,
            46.3, 47.6, 51.3, 51.8),
  .Dim = c(20, 4)),
  x = c(8.0, 8.5, 9.0, 9.5),
  R = structure(
    .Data = c(4, 0, 0, 0,
              0, 4, 0, 0,
              0, 0, 4, 0,
              0, 0, 0, 4),
    .Dim = c(4, 4)))




# 初始值
inits_list <- list(
  list(alpha = 40, beta = 1),
  list(alpha = mean(data_list$y), beta = 1/sd(data_list$y)^2)
)


# JAGS模型代码
model_string <- textConnection("model {
for (i in 1:20) {Y[i, 1:4] ~ dmnorm(mu[], Sigma.inv[,])}
for (j in 1:4) {mu[j] <- alpha + beta*x[j]}
alpha ~ dnorm(0, 0.0001)
beta ~ dnorm(0, 0.0001)
Sigma.inv[1:4, 1:4] ~ dwish(R[,], 4)
Sigma[1:4, 1:4] <- inverse(Sigma.inv[,])
for (i in 1:20) {
for (j in 1:4) {
res[i, j] <- Y[i, j] - mu[j]
temp[i, j] <- inprod(Sigma.inv[j, 1:4], res[i, 1:4])
}
M.squared[i] <- inprod(res[i, 1:4], temp[i, 1:4])
M[i] <- sqrt(M.squared[i])
}
}
")



#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("alpha","beta","Sigma","Sigma.inv","res","temp","M.squared","M")
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)

#箱线图

# 提取样本数据并转换为数据框
samples_matrix <- as.matrix(samples)
samples_df <- as.data.frame(samples_matrix)

# 提取res2变量（残差平方）
#res2_columns <- grep("res2", colnames(samples_df), value = TRUE)

#res_columns <- grep("res", colnames(samples_df), value = TRUE)

# 将数据转换为长格式
samples_long <- melt(samples_df)

# M.squared M
M.squared_data <- samples_long[grep("M.squared", samples_long$variable), ]
M_data <- samples_long[grep("M", samples_long$variable), ]

#res_data <- samples_long[grep("res", samples_long$variable), ]

# 绘制M的箱线图
ggplot(M_data, aes(x = variable, y = value)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  labs(title = "Boxplot of theta Parameter",
       x = "Parameter",
       y = "Value") +
  theme_minimal()

# 绘制res2的箱线图（按医院）
ggplot(M.squared_data, aes(x = variable, y = value)) +
  geom_boxplot(fill = "lightgreen", alpha = 0.7) +
  labs(title = "Boxplot of Squared Residuals by Hospital",
       x = "Hospital",
       y = "Squared Residuals") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##########8.3.3



# pack as a list for JAGS
data <- list(x = c(1.0, 1.5, 1.5, 1.5, 2.5, 4.0, 5.0, 5.0, 7.0,
                   8.0, 8.5, 9.0, 9.5, 9.5, 10.0, 12.0, 12.0, 13.0,
                   13.0, 14.5, 15.5, 15.5, 16.5, 17.0, 22.5, 29.0, 31.5),
             y = c(1.80, 1.85, 1.87, 1.77, 2.02, 2.27, 2.15, 2.26, 2.47,
                   2.19, 2.26, 2.40, 2.39, 2.41, 2.50, 2.32, 2.32, 2.43,
                   2.47, 2.56, 2.65, 2.47, 2.64, 2.56, 2.70, 2.72, 2.57),
             N = 27)





# Initialize Y.rep with reasonable values
#init <- list(
#  list(mu = 0, tau = 1, d = 1)
#list(Y.rep = data$Y + rnorm(66, 0, 5)),
# list(Y.rep = data$Y + rnorm(66, 0, 5))
#)





# JAGS模型代码
model_string <- textConnection("model {
for(i in 1:N) {
y[i] ~ dnorm(mu[i], inv.sigma2)
mu[i] <- alpha - beta*pow(gamma, x[i])
res[i] <- (y[i] - mu[i])/sigma
p.res[i] <- phi(res[i])
}
alpha ~ dunif(0, 100)
beta ~ dunif(0, 100)
gamma ~ dunif(0, 1)
inv.sigma2 <- 1/pow(sigma, 2)
log(sigma) <- log.sigma
log.sigma ~ dunif(-10, 10)
}
")




#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
#model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

model <- jags.model(model_string, 
                    data = data, 
                    #inits = init,
                    n.chains = 2,
                    quiet = TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("alpha","beta","gamma",)  #, "deviance"
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)


############8.3.4




# pack as a list for JAGS
data <- list(N=12,
             y=c(4,3,2,2,3,4,2,5,3,3,6,3),
             n=c(14,19,32,12,16,41,24,48,20,18,58,30))





# Initialize Y.rep with reasonable values
#init <- list(
#  list(mu = 0, tau = 1, d = 1)
#list(Y.rep = data$Y + rnorm(66, 0, 5)),
# list(Y.rep = data$Y + rnorm(66, 0, 5))
#)





# JAGS模型代码
model_string <- textConnection("model {
for (i in 1:N) {
y[i] ~ dbin(theta, n[i])
prop[i] <- y[i]/n[i] # (extra 0.00001 avoids numerical errors if prop[i] = 0 or 1)
Ds[i] <- 2*n[i]*(prop[i]*log((prop[i]+0.00001)/theta)
+ (1-prop[i])*log((1-prop[i]+0.00001)/(1-theta)))
# sign of deviance residual
sign[i] <- 2*step(prop[i] - theta) - 1
dev.res[i] <- sign[i]*sqrt(Ds[i])
}
dev.sat <- sum(Ds[])
theta ~ dunif(0, 1)
}
")




#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
#model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

model <- jags.model(model_string, 
                    data = data, 
                    #inits = init,
                    n.chains = 2,
                    quiet = TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("theta","dev.sat")  #, "deviance"
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)


#############8.4.1





# pack as a list for JAGS
data <- list(N = 12, y=c(41,25,24,23,25,42,24,53,26,25,58,31),
             n=c(143,187,323,122,164,405,239,482,195,177,581,301))



# Initialize Y.rep with reasonable values
#init <- list(
#  list(mu = 0, tau = 1, d = 1)
#list(Y.rep = data$Y + rnorm(66, 0, 5)),
# list(Y.rep = data$Y + rnorm(66, 0, 5))
#)





# JAGS模型代码
model_string <- textConnection("model {
for (i in 2:N) { # remove Bristol
y[i] ~ dbin(theta, n[i])
}
theta ~ dunif(0, 1)
# predicted number of deaths in centre 1 (Bristol)
y1.pred ~ dbin(theta, n[1])
P.bris <- step(y1.pred-y[1]-0.00001) + 0.5*equals(y1.pred, y[1])
}
")




#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
#model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

model <- jags.model(model_string, 
                    data = data, 
                    #inits = init,
                    n.chains = 2,
                    quiet = TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("y1.pred","P.bris")  #, "deviance"
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)







############## 8.4.2


# pack as a list for JAGS
data <- list(
  N = 30,
  y=c(0,0,0,1,0,1,0,0,0,1,0,0,1,0,1,0,0,0,0,1,1,0,0,0,1,0,0,1,0,1))



# Initialize Y.rep with reasonable values
#init <- list(
#  list(mu = 0, tau = 1, d = 1)
  #list(Y.rep = data$Y + rnorm(66, 0, 5)),
  # list(Y.rep = data$Y + rnorm(66, 0, 5))
#)





# JAGS模型代码
model_string <- textConnection("model {
for(i in 1:N) {
y[i] ~ dbern(theta)
y.rep[i] ~ dbern(theta)
}
theta ~ dunif(0,1)
switch.obs[1] <- 0
switch.rep[1] <- 0
for (i in 2:N) {
switch.obs[i] <- 1 - equals(y[i-1], y[i])
switch.rep[i] <- 1 - equals(y.rep[i-1], y.rep[i])
}
s.obs <- sum(switch.obs[])
s.rep <- sum(switch.rep[])
P.switch <- step(s.obs-s.rep-0.5)
+ 0.5*equals(s.obs, s.rep)
run.obs[1] <- 1
run.rep[1] <- 1
max.run.obs[1] <- 1
max.run.rep[1] <- 1
for (i in 2:N) {
run.obs[i] <- 1 + run.obs[i-1]
* equals(y[i-1], y[i])
run.rep[i] <- 1 + run.rep[i-1]
* equals(y.rep[i-1], y.rep[i])
max.run.obs[i] <- max(max.run.obs[i-1], run.obs[i])
max.run.rep[i] <- max(max.run.rep[i-1], run.rep[i])
}
P.run <- step(max.run.obs[N]-max.run.rep[N]-0.5)
+ 0.5*equals(max.run.obs[N], max.run.rep[N])
}
")




#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
#model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

model <- jags.model(model_string, 
                    data = data, 
                    #inits = init,
                    n.chains = 2,
                    quiet = TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("P.run","P.switch","max.run.rep"," s.rep","theta ")  #, "deviance"
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)



############## 8.4.3          ERROR


# pack as a list for JAGS
data <-  list(N=66, T.obs = 23.7, Y=c(
  28, 26, 33, 24, 34, -44, 27, 16, 40, -2,
  29, 22, 24, 21, 25, 30, 23, 29, 31, 19,
  24, 20, 36, 32, 36, 28, 25, 21, 28, 29,
  37, 25, 28, 26, 30, 32, 36, 26, 30, 22,
  36, 23, 27, 27, 28, 27, 31, 27, 26, 33,
  26, 32, 32, 24, 39, 28, 24, 25, 32, 25,
  29, 27, 28, 29, 16, 23))


# 初始值
#inits_list <- list(
#  list(mu = 0, tau = 1),
#  list(mu = mean(data_list$y), tau = 1/sd(data_list$y)^2)
#)





# JAGS模型代码
model_string <- textConnection("model {
for(i in 1:N){
Y[i] ~ dnorm(mu,tau)
Y.rep[i] ~ dnorm(mu,tau)
}
mu ~ dunif(-100, 100)
tau ~ dgamma(0.001, 0.001)
N.50 <- round(N/2)
N.25 <- round(N/4)
Y.rep.min <- ranked(Y.rep[], 1)
Y.rep.50 <- ranked(Y.rep[], N.50)
Y.rep.25 <- ranked(Y.rep[], N.25)
T.rep <- (Y.rep.min - Y.rep.50)/(Y.rep.25 - Y.rep.50)
P.T <- step(T.rep - T.obs)
V.obs <- sd(Y[])*sd(Y[])
V.rep <- sd(Y.rep[])*sd(Y.rep[])
P.V <- step(V.rep - V.obs)
}
")




#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("mu","tau","P.T","P.V","T.rep","V.rep")  #, "deviance"
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)


############## 8.4.4   error


# pack as a list for JAGS
data <-  list(N=66, Y=c(
  28, 26, 33, 24, 34, -44, 27, 16, 40, -2,
  29, 22, 24, 21, 25, 30, 23, 29, 31, 19,
  24, 20, 36, 32, 36, 28, 25, 21, 28, 29,
  37, 25, 28, 26, 30, 32, 36, 26, 30, 22,
  36, 23, 27, 27, 28, 27, 31, 27, 26, 33,
  26, 32, 32, 24, 39, 28, 24, 25, 32, 25,
  29, 27, 28, 29, 16, 23))


# Initialize Y.rep with reasonable values
init <- list(
  list(mu = 0, tau = 1),
  list(Y.rep = data$Y + rnorm(66, 0, 5)),
  list(Y.rep = data$Y + rnorm(66, 0, 5))
)





# JAGS模型代码
model_string <- textConnection("model {
for (i in 1:N) {
Y[i] ~ dnorm(mu, tau)
Y.rep[i] ~ dnorm(mu, tau)
T.data.obs[i] <- pow((Y[i] - mean(Y[]))/sd(Y[]),3)
T.data.rep[i] <- pow((Y.rep[i] - mean(Y.rep[]))/sd(Y.rep[]),3)
T.para.obs[i] <- pow((Y[i] - mu)/sigma,3)
T.para.rep[i] <- pow((Y.rep[i] - mu)/sigma,3)
}
mu ~ dunif(-100, 100)
tau ~ dgamma(0.001, 0.001)
sigma <- 1/sqrt(tau)
T.data.obs.tot <- sum(T.data.obs[])
T.data.rep.tot <- sum(T.data.rep[])
T.para.obs.tot <- sum(T.para.obs[])
T.para.rep.tot <- sum(T.para.rep[])
P.data <- step(T.data.obs.tot - T.data.rep.tot)
P.para <- step(T.para.obs.tot - T.para.rep.tot)
}
")




#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
#model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

model <- jags.model(model_string, 
                    data = data, 
                    #inits = init,
                    n.chains = 2,
                    quiet = TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("mu","tau","P.data"," P.para","T.data.obs.tot ","T.data.rep.tot","T.para.obs.tot","T.para.rep.tot" ,"sigma")  #, "deviance"
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)


#########8.4.5

# pack as a list for JAGS
data <-  list(x = c(1.0, 1.5, 1.5, 1.5, 2.5, 4.0, 5.0, 5.0, 7.0,
                    8.0, 8.5, 9.0, 9.5, 9.5, 10.0, 12.0, 12.0, 13.0,
                    13.0, 14.5, 15.5, 15.5, 16.5, 17.0, 22.5, 29.0, 31.5),
              y = c(1.80, 1.85, 1.87, 1.77, 2.02, 2.27, 2.15, 2.26, 2.47,
                    2.19, 2.26, 2.40, 2.39, 2.41, 2.50, 2.32, 2.32, 2.43,
                    2.47, 2.56, 2.65, 2.47, 2.64, 2.56, 2.70, 2.72, 2.57),
              N = 27)


# Initialize Y.rep with reasonable values
init <- list(
  list(mu = 0, tau = 1),
  list(Y.rep = data$Y + rnorm(66, 0, 5)),
  list(Y.rep = data$Y + rnorm(66, 0, 5))
)





# JAGS模型代码
model_string <- textConnection("model {
for(i in 1:N) {
y[i] ~ dnorm(mu[i], inv.sigma2)
mu[i] <- alpha - beta*pow(gamma, x[i])
y.pred[i] ~ dnorm(mu[i], inv.sigma2)
P.pred[i] <- step(y[i] - y.pred[i])
}
alpha ~ dunif(0, 100)
beta ~ dunif(0, 100)
gamma ~ dunif(0, 1)
inv.sigma2 <- 1/pow(sigma, 2)
log(sigma) <- log.sigma
log.sigma ~ dunif(-10, 10)
}

")




#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
#model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

model <- jags.model(model_string, 
                    data = data, 
                    #inits = init,
                    n.chains = 2,
                    quiet = TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("y.pred","P.pred","alpha"," beta","gamma ")  #, "deviance"
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)


###########8.4.6        ERROR


# pack as a list for JAGS
data <-  list(N=35,K=10,T=4,
              y=c(0,0,0,0,0,0,0,0,0,0,
                  1,1,1,1,1,2,2,2,2,2,
                  2,2,2,2,2,3,3,3,3,3,
                  3,3,4,4,5))



# Initialize Y.rep with reasonable values
init <- list(
  list(alpha = 3, beta = 2, gamma = 0.9, log.sigma = -5)
  #list(Y.rep = data$Y + rnorm(66, 0, 5)),
  # list(Y.rep = data$Y + rnorm(66, 0, 5))
)





# JAGS模型代码
model_string <- textConnection("model {
# remember that k = number of claims + 1
for (i in 1:N) {
y[i] ~ dpois(lambda)
y.rep[i] ~ dpois(lambda)
for (k in 1:K) {
eq[i,k] <- equals(y[i], k-1) # needed to construct
# aggregate data
eq.rep[i,k] <- equals(y.rep[i], k-1)
}
}
for (k in 1:K) {
m[k] <- sum(eq[,k]) # aggregate counts
m.rep[k] <- sum(eq.rep[,k])
# log of expected counts
logE[k] <- log(N) - lambda + (k-1)*log(lambda)
- logfact(k-1)
# likelihood ratio statistic
LR[k] <- 2*m[k]*(log(m[k]+0.00001) - logE[k])
LR.rep[k] <- 2*m.rep[k]*(log(m.rep[k]+0.00001)
- logE[k])
}
G <- sum(LR[])
G.rep <- sum(LR.rep[])
P <- step(G - G.rep)
lambda ~ dgamma(0.5, 0.001) # Jeffreys prior
# more powerful invariant test for excess zero counts
T.rep <- m.rep[1]*m.rep[3]/(m.rep[2]*m.rep[2])
P.inv <- step(T - T.rep)
}

")




#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
#model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

model <- jags.model(model_string, 
                    data = data, 
                    #inits = init,
                    n.chains = 2,
                    quiet = TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("P","G","T.rep"," P.inv")  #, "deviance"
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)








############## 8.5.1

# pack as a list for JAGS
data <-  list(N=66, Y=c(
  28, 26, 33, 24, 34, -44, 27, 16, 40, -2,
  29, 22, 24, 21, 25, 30, 23, 29, 31, 19,
  24, 20, 36, 32, 36, 28, 25, 21, 28, 29,
  37, 25, 28, 26, 30, 32, 36, 26, 30, 22,
  36, 23, 27, 27, 28, 27, 31, 27, 26, 33,
  26, 32, 32, 24, 39, 28, 24, 25, 32, 25,
  29, 27, 28, 29, 16, 23))


# Initialize Y.rep with reasonable values
init <- list(
  list(mu = 0, tau = 1, d = 1)
  #list(Y.rep = data$Y + rnorm(66, 0, 5)),
  # list(Y.rep = data$Y + rnorm(66, 0, 5))
)





# JAGS模型代码
model_string <- textConnection("model {
for (i in 1:N) {
Y[i] ~ dnorm(mu, invsigma2[i])
invsigma2[i] <- tau/s[i]
s[i] <- 4/lambda[i]
lambda[i] ~ dchisqr(4)
}
mu ~ dunif(-100, 100)
tau ~ dgamma(0.001, 0.001)
}
")




#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
#model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

model <- jags.model(model_string, 
                    data = data, 
                    #inits = init,
                    n.chains = 2,
                    quiet = TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("mu","tau","d"," mu","two ")  #, "deviance"
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)




#######################

# pack as a list for JAGS
data <-  list(N=66, Y=c(
  28, 26, 33, 24, 34, -44, 27, 16, 40, -2,
  29, 22, 24, 21, 25, 30, 23, 29, 31, 19,
  24, 20, 36, 32, 36, 28, 25, 21, 28, 29,
  37, 25, 28, 26, 30, 32, 36, 26, 30, 22,
  36, 23, 27, 27, 28, 27, 31, 27, 26, 33,
  26, 32, 32, 24, 39, 28, 24, 25, 32, 25,
  29, 27, 28, 29, 16, 23))



# Initialize Y.rep with reasonable values
init <- list(
  list(mu = 0, tau = 1, d = 1)
  #list(Y.rep = data$Y + rnorm(66, 0, 5)),
 # list(Y.rep = data$Y + rnorm(66, 0, 5))
)





# JAGS模型代码
model_string <- textConnection("model {
for (i in 1:N) {
Y[i] ~ dt(mu, tau, nu)
}
mu ~ dunif(-100, 100)
tau ~ dgamma(0.001, 0.001)
nu <- pow(2, d)
two <- equals(nu, 2)
d ~ dcat(p[])
for (i in 1:10) {
p[i] <- 1/10
}
}
")




#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
#model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

model <- jags.model(model_string, 
                    data = data, 
                    #inits = init,
                    n.chains = 2,
                    quiet = TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("mu","tau","d"," mu","two ")  #, "deviance"
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)


#############8.6.1

# pack as a list for JAGS
data <- list(x = c(1.0, 1.5, 1.5, 1.5, 2.5, 4.0, 5.0, 5.0, 7.0,
                   8.0, 8.5, 9.0, 9.5, 9.5, 10.0, 12.0, 12.0, 13.0,
                   13.0, 14.5, 15.5, 15.5, 16.5, 17.0, 22.5, 29.0, 31.5),
             y = c(1.80, 1.85, 1.87, 1.77, 2.02, 2.27, 2.15, 2.26, 2.47,
                   2.19, 2.26, 2.40, 2.39, 2.41, 2.50, 2.32, 2.32, 2.43,
                   2.47, 2.56, 2.65, 2.47, 2.64, 2.56, 2.70, 2.72, 2.57),
             N = 27)



# Initialize Y.rep with reasonable values
init <- list(
  list(alpha = 0, beta = 0, gamma = 0, r = 10)
  #list(Y.rep = data$Y + rnorm(66, 0, 5)),
  # list(Y.rep = data$Y + rnorm(66, 0, 5))
)





# JAGS模型代码
model_string <- textConnection("model {
for(i in 1:N) {
y[i] ~ dnorm(mu[i], inv.sigma2)
mu[i] <- alpha - beta*pow(gamma, x[i])
}
alpha ~ dunif(0, 100)
beta ~ dunif(0, 100)
gamma ~ dunif(0, 1)
inv.sigma2 <- 1/pow(sigma, 2)
log(sigma) <- log.sigma
log.sigma ~ dunif(-10, 10)
}
")




#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
#model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

model <- jags.model(model_string, 
                    data = data, 
                    #inits = init,
                    n.chains = 2,
                    quiet = TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("alpha","beta","gamma"," sigma")  #, "deviance"
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)



############## 8.6.4     XXXXXXXXXXXXXXXXX


# pack as a list for JAGS
data <- list(max = 1000,
             y = structure(.Data = c(15,21,29,16,18,21,16,26,33,27,41,60,33,38,41,20,27,42),
                           .Dim = c(6, 3)),
             x = c(0, 10, 33, 100, 333, 1000))



# Initialize Y.rep with reasonable values
init <- list(
  list(alpha = 0, beta = 0, gamma = 0, r = 10)
  #list(Y.rep = data$Y + rnorm(66, 0, 5)),
  # list(Y.rep = data$Y + rnorm(66, 0, 5))
)





# JAGS模型代码
model_string <- textConnection("model {
for (i in 1:6) {
for (j in 1:3) {
y[i,j] ~ dnegbin(p[i], r)
}
p[i] <- r/(mu[i] + r)
log(mu[i]) <- alpha + beta*log(x[i] + 10) + gamma*x[i]
}
r ~ dunif(1, max)
alpha ~ dnorm(0, 0.0001)
beta ~ dnorm(0, 0.0001)
gamma ~ dnorm(0, 0.0001)
}
")




#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
#model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

model <- jags.model(model_string, 
                    data = data, 
                    #inits = init,
                    n.chains = 2,
                    quiet = TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("alpha","beta","gamma"," r")  #, "deviance"
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)


###############################3


# pack as a list for JAGS
data <- list(y = structure(.Data = c(15,21,29,16,18,21,16,26,33,27,41,60,33,38,41,20,27,42),
                           .Dim = c(6, 3)),
             x = c(0, 10, 33, 100, 333, 1000))



# Initialize Y.rep with reasonable values
init <- list(
  list(alpha = 0, beta = 0, gamma = 0, logr.cont = 1)
  #list(Y.rep = data$Y + rnorm(66, 0, 5)),
  # list(Y.rep = data$Y + rnorm(66, 0, 5))
)





# JAGS模型代码
model_string <- textConnection("model {
for (i in 1:6) {
for (j in 1:3) {
y[i,j] ~ dnegbin(p[i], r)
}
p[i] <- r/(mu[i] + r)
log(mu[i]) <- alpha + beta*log(x[i] + 10) + gamma*x[i]
}
logr.cont ~ dunif(0, 10)
log(r.cont) <- logr.cont
r <- round(r.cont)
alpha ~ dnorm(0, 0.0001)
beta ~ dnorm(0, 0.0001)
gamma ~ dnorm(0, 0.0001)
}
")




#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
#model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

model <- jags.model(model_string, 
                    data = data, 
                    #inits = init,
                    n.chains = 2,
                    quiet = TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("alpha","beta","gamma"," r"," logr.cont ","r.cont")  #, "deviance"
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)


##################8.7.1

data <- list(y = structure(.Data = c(15,21,29,16,18,21,16,26,33,27,41,60,33,38,41,20,27,42),
                           .Dim = c(6, 3)),
             x = c(0, 10, 33, 100, 333, 1000))



# Initialize Y.rep with reasonable values
init <- list(
  list(alpha = 0, beta = 0, gamma = 0, logr.cont = 1)
  #list(Y.rep = data$Y + rnorm(66, 0, 5)),
  # list(Y.rep = data$Y + rnorm(66, 0, 5))
)





# JAGS模型代码
model_string <- textConnection("model {
q[1] <- 0.5; q[2] <- 0.5 # prior assumptions
r <- 8; n <- 8 # data
r ~ dbin(theta[pick], n) # likelihood
pick ~ dcat(q[])
theta[1] <- 0.5 # if random (assumption 1)
theta[2] ~ dunif(0.5, 0.55) # if psychic
psychic <- pick - 1 # 1 if psychic, 0 otherwise
}
")




#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
#model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

model <- jags.model(model_string, 
                    data = data, 
                    #inits = init,
                    n.chains = 2,
                    quiet = TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("alpha","beta","gamma"," r"," logr.cont ","r.cont")  #, "deviance"
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)


###########8.10.2



data <- list(r = 2, n = 5, m = 10, a = c(3,3), b = c(27,3))


# Initialize Y.rep with reasonable values
init <- list(
  list(alpha = 0, beta = 0, gamma = 0, logr.cont = 1)
  #list(Y.rep = data$Y + rnorm(66, 0, 5)),
  # list(Y.rep = data$Y + rnorm(66, 0, 5))
)





# JAGS模型代码
model_string <- textConnection("model {
theta ~ dbeta(a[pick], b[pick])
pick ~ dcat(q[1:2])
q[1] <- 0.50
q[2] <- 0.50
q.post[1] <- equals(pick, 1) # = 1 if prior 1 picked
q.post[2] <- equals(pick, 2) # = 1 if prior 2 picked
r ~ dbin(theta, n) # sampling distribution
r.pred ~ dbin(theta, m) # predictive distribution
}
")




#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
#model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

model <- jags.model(model_string, 
                    data = data, 
                    #inits = init,
                    n.chains = 2,
                    quiet = TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("q","q.post","theta"," r.pred")  #, "deviance"
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)


###########8.10.3



data <- list(r = 2, n = 5, m = 10, a = c(3,3), b = c(27,3))


# Initialize Y.rep with reasonable values
init <- list(
  list(alpha = 0, beta = 0, gamma = 0, logr.cont = 1)
  #list(Y.rep = data$Y + rnorm(66, 0, 5)),
  # list(Y.rep = data$Y + rnorm(66, 0, 5))
)





# JAGS模型代码
model_string <- textConnection("model {
y.obs <- 4
df[1] <-1; df[2] <- 2; df[3] <- 4
df[4] <- 10; df[5] <- 50; df[6] <- 1000
for (i in 1:6) {
y[i] <- y.obs # replicate data
y[i] ~ dnorm(mu[i], 1)
mu[i] ~ dnorm(0, lambda[i])
lambda[i] <- X[i]/df[i] # precision is chi-square/df
X[i] ~ dchisqr(df[i])
# compare with prior distributions
mu.rep[i] ~ dnorm(0, lambda.rep[i])
lambda.rep[i] <- X.rep[i]/df[i]
X.rep[i] ~ dchisqr(df[i])
}
}
")




#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
#model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

model <- jags.model(model_string, 
                    data = data, 
                    #inits = init,
                    n.chains = 2,
                    quiet = TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("q","q.post","theta"," r.pred")  #, "deviance"
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)



############9.1.1

data <- list(y = c(177,236,285,350,NA), x = c(8,15,22,29,36))


# Initialize Y.rep with reasonable values
init <- list(
  list(alpha = 0, beta = 0, gamma = 0, logr.cont = 1)
  #list(Y.rep = data$Y + rnorm(66, 0, 5)),
  # list(Y.rep = data$Y + rnorm(66, 0, 5))
)





# JAGS模型代码
model_string <- textConnection("model {
for (i in 1:5) {
y[i] ~ dnorm(mu[i], tau)
mu[i] <- alpha + beta*(x[i] - mean(x[]))
}
alpha ~ dflat()
beta ~ dflat()
tau <- 1/sigma2
log(sigma2) <- 2*log.sigma
log.sigma ~ dflat()
}
")




#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
#model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

model <- jags.model(model_string, 
                    data = data, 
                    #inits = init,
                    n.chains = 2,
                    quiet = TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("q","q.post","theta"," r.pred")  #, "deviance"
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)


############9.1.2

data <- list(y = c(177,236,285,350,NA), x = c(8,15,22,29,36), miss = c(0,0,0,0,1))


# Initialize Y.rep with reasonable values
init <- list(
  list(alpha = 0, beta = 0, gamma = 0, logr.cont = 1)
  #list(Y.rep = data$Y + rnorm(66, 0, 5)),
  # list(Y.rep = data$Y + rnorm(66, 0, 5))
)





# JAGS模型代码
model_string <- textConnection("model {
for (i in 1:5) {
y[i] ~ dnorm(mu[i], tau)
mu[i] <- alpha + beta*(x[i] - mean(x[]))
# selection model for missing data mechanism
miss[i] ~ dbern(p[i])
logit(p[i]) <- a + b*(y[i]-250)
}
a ~ dlogis(0, 1)
b <- log(1.02)
alpha ~ dflat()
beta ~ dflat()
tau <- 1/sigma2
log(sigma2) <- 2*log.sigma
log.sigma ~ dflat()
}

")




#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
#model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

model <- jags.model(model_string, 
                    data = data, 
                    #inits = init,
                    n.chains = 2,
                    quiet = TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("q","q.post","theta"," r.pred")  #, "deviance"
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)



############9.1.3

data <- list(alpha = 3, beta = 2, gamma = 0.9, log.sigma = -5, mu.x = 2, sd.x = 0.01)


# Initialize Y.rep with reasonable values
init <- list(
  list(alpha = 0, beta = 0, gamma = 0, logr.cont = 1)
  #list(Y.rep = data$Y + rnorm(66, 0, 5)),
  # list(Y.rep = data$Y + rnorm(66, 0, 5))
)





# JAGS模型代码
model_string <- textConnection("model {
for(j in 1:27) {
y[j] ~ dnorm(mu[j], tau)
mu[j] <- alpha - beta*pow(gamma, x[j])
# prior on covariate
x[j] ~ dlnorm(mu.x, p.x)
}
alpha ~ dunif(0, 100)
beta ~ dunif(0, 100)
gamma ~ dunif(0, 1)
tau <- 1/pow(sigma, 2)
log(sigma) <- log.sigma
log.sigma ~ dunif(-10, 10)
# priors on mean and precision of covariate model
mu.x ~ dunif(-10, 10)
p.x <- 1/pow(sd.x, 2)
sd.x ~ dunif(0, 10)
}

")




#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
#model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

model <- jags.model(model_string, 
                    data = data, 
                    #inits = init,
                    n.chains = 2,
                    quiet = TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("alpha","beta","gamma"," sigma","x","p.x","sd.x")  #, "deviance"
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)


############9.2.1

data <- list(x = c(1.0, 1.5, 1.5, 1.5, 2.5, 4.0, 5.0, 5.0, 7.0,
                   8.0, 8.5, 9.0, 9.5, 9.5, 10.0, 12.0, 12.0, 13.0,
                   13.0, 14.5, 15.5, 15.5, 16.5, 17.0, 22.5, 29.0, 31.5, 35, 40),
             y = c(1.80, 1.85, 1.87, 1.77, 2.02, 2.27, 2.15, 2.26, 2.47,
                   2.19, 2.26, 2.40, 2.39, 2.41, 2.50, 2.32, 2.32, 2.43,
                   2.47, 2.56, 2.65, 2.47, 2.64, 2.56, 2.70, 2.72, 2.57, NA, NA),
             N = 29)


# Initialize Y.rep with reasonable values
init <- list(
  list(alpha = 0, beta = 0, gamma = 0, logr.cont = 1)
  #list(Y.rep = data$Y + rnorm(66, 0, 5)),
  # list(Y.rep = data$Y + rnorm(66, 0, 5))
)





# JAGS模型代码
model_string <- textConnection("model {
for(i in 1:N) {
y[i] ~ dnorm(mu[i], inv.sigma2)
mu[i] <- alpha - beta*pow(gamma, x[i])
}
alpha ~ dunif(0, 100)
beta ~ dunif(0, 100)
gamma ~ dunif(0, 1)
inv.sigma2 <- 1/pow(sigma, 2)
log(sigma) <- log.sigma
log.sigma ~ dunif(-10, 10)
}
")




#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
#model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

model <- jags.model(model_string, 
                    data = data, 
                    #inits = init,
                    n.chains = 2,
                    quiet = TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("alpha","beta","gamma"," inv.sigma2")  #, "deviance"
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)


############9.3.2

data <- list(x = c(1.0, 1.5, 1.5, 1.5, 2.5, 4.0, 5.0, 5.0, 7.0,
                   8.0, 8.5, 9.0, 9.5, 9.5, 10.0, 12.0, 12.0, 13.0,
                   13.0, 14.5, 15.5, 15.5, 16.5, 17.0, 22.5, 29.0, 31.5),
             y = c(1.80, 1.85, 1.87, 1.77, 2.02, 2.27, 2.15, 2.26, 2.47,
                   2.19, 2.26, 2.40, 2.39, 2.41, 2.50, 2.32, 2.32, 2.43,
                   2.47, 2.56, 2.65, 2.47, 2.64, 2.56, 2.70, 2.72, 2.57),
             n = 27)

# Initialize Y.rep with reasonable values
init <- list(
  list(alpha = 0, beta = 0, gamma = 0, logr.cont = 1)
  #list(Y.rep = data$Y + rnorm(66, 0, 5)),
  # list(Y.rep = data$Y + rnorm(66, 0, 5))
)





# JAGS模型代码
model_string <- textConnection("model {
for(j in 1:n) {
y[j] ~ dnorm(mu[j], tau)
mu[j] <- alpha - beta*pow(gamma, z[j])
x[j] ~ dnorm(z[j], 1)
z[j] ~ dunif(0, 100)
}
alpha ~ dunif(0, 100)
beta ~ dunif(0, 100)
gamma ~ dunif(0, 1)
tau <- 1/sigma2
log(sigma2) <- 2*log.sigma
log.sigma ~ dunif(-10, 10)
for (j in 1:n) {resx[j] <- x[j] - z[j]}
}
")




#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
#model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

model <- jags.model(model_string, 
                    data = data, 
                    #inits = init,
                    n.chains = 2,
                    quiet = TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("alpha","beta","gamma"," tau")  #, "deviance"
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)



############9.9.1

data <- list(N = 11, r=c(25,24,23,25,42,24,53,26,25,58,31),
             n=c(187,323,122,164,405,239,482,195,177,581,301))

# Initialize Y.rep with reasonable values
init <- list(
  list(alpha = 0, beta = 0, gamma = 0, logr.cont = 1)
  #list(Y.rep = data$Y + rnorm(66, 0, 5)),
  # list(Y.rep = data$Y + rnorm(66, 0, 5))
)





# JAGS模型代码
model_string <- textConnection("model {
for (i in 1:N) {
numbers1toN[i] <- i
p[i] ~ dbeta(0.5, 0.5)
r[i] ~ dbin(p[i], n[i])
hosp.rank[i] <- rank(p[], i) # rank of hospital i
prob.lowest[i] <- equals(hosp.rank[i], 1) # =1 if hosp i is lowest
prob.highest[i] <- equals(hosp.rank[i], N) # =1 if hosp i is highest
}
hosp.lowest <- inprod(numbers1toN[], prob.lowest[])
# index of lowest hosp
hosp.highest <- inprod(numbers1toN[], prob.highest[])
# index of highest hosp
}
")




#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
#model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

model <- jags.model(model_string, 
                    data = data, 
                    #inits = init,
                    n.chains = 2,
                    quiet = TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("alpha","beta","gamma"," tau")  #, "deviance"
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)



############10.3.1

data <- list(y = structure(.Data = c(15,21,29,16,18,21,16,26,33,27,41,60,33,38,41,20,27,42),
                           .Dim = c(6, 3)),
             x = c(0, 10, 33, 100, 333, 1000))

# Initialize Y.rep with reasonable values
init <- list(
  list(alpha = 0, beta = 0, gamma = 0, logr.cont = 1)
  #list(Y.rep = data$Y + rnorm(66, 0, 5)),
  # list(Y.rep = data$Y + rnorm(66, 0, 5))
)





# JAGS模型代码
model_string <- textConnection("model {
for (i in 1:6) {
for (j in 1:3) {
y[i,j] ~ dpois(mu[i,j])
log(mu[i,j]) <- log.fit[i] + lambda[i,j]
lambda[i,j] ~ dnorm(0, inv.omega.lambda.squared)
}
log.fit[i] <- alpha + beta*log(x[i] + 10)
+ gamma*x[i]
log(fit[i]) <- log.fit[i]
y.pred[i] ~ dpois(mu.pred[i])
log(mu.pred[i]) <- log.fit[i] + lambda.pred[i]
lambda.pred[i] ~ dnorm(0, inv.omega.lambda.squared)
}
alpha ~ dnorm(0, 0.0001)
beta ~ dnorm(0, 0.0001)
gamma ~ dnorm(0, 0.0001)
omega.lambda ~ dunif(0, 100)
inv.omega.lambda.squared <- 1/pow(omega.lambda, 2)
}
")




#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
#model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

model <- jags.model(model_string, 
                    data = data, 
                    #inits = init,
                    n.chains = 2,
                    quiet = TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("alpha","beta","gamma"," mu")  #, "deviance"
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)


############10.3.3

data <- list(y = structure(.Data = c(15,21,29,16,18,21,16,26,33,27,41,60,33,38,41,20,27,42),
                           .Dim = c(6, 3)),
             x = c(0, 10, 33, 100, 333, 1000))

# Initialize Y.rep with reasonable values
init <- list(
  list(alpha = 0, beta = 0, gamma = 0, logr.cont = 1)
  #list(Y.rep = data$Y + rnorm(66, 0, 5)),
  # list(Y.rep = data$Y + rnorm(66, 0, 5))
)





# JAGS模型代码
model_string <- textConnection("model {
for (i in 1:N) {
for (j in 1:3) {
y[i,j] ~ dnorm(psi[i,j], inv.sigma.squared)
psi[i,j] <- alpha[i] + beta[i]*(t[i,j] - tbar)
+ gamma*(y0[i] - y0bar)
}
alpha[i] ~ dnorm(mu.alpha, inv.omega.alpha.squared)
beta[i] ~ dnorm(mu.beta, inv.omega.beta.squared)
}
inv.sigma.squared <- 1/sigma.squared
inv.omega.alpha.squared <- 1/omega.alpha.squared
inv.omega.beta.squared <- 1/omega.beta.squared
sigma.squared <- pow(sigma, 2)
omega.alpha.squared <- pow(omega.alpha, 2)
omega.beta.squared <- pow(omega.beta, 2)
log(sigma) <- log.sigma
log.sigma ~ dunif(-10, 10)
omega.alpha ~ dunif(0, 100)
omega.beta ~ dunif(0, 100)
mu.alpha ~ dnorm(0, 0.0001)
mu.beta ~ dnorm(0, 0.0001)
gamma ~ dnorm(0, 0.0001)

y0bar <- mean(y0[])
}
")




#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
#model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

model <- jags.model(model_string, 
                    data = data, 
                    #inits = init,
                    n.chains = 2,
                    quiet = TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("alpha","beta","gamma"," mu")  #, "deviance"
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)


############10.4.1

data <- list(
  D = 30,
  y = c(
    1.09,   0.75,   0.53,   0.34,   0.23,   0.02,
    2.03,   1.28,   1.2,   1.02,   0.83,   0.28,
    1.44,   1.3,   0.95,   0.68,   0.52,   0.06,
    1.55,   0.96,   0.8,   0.62,   0.46,   0.08,
    1.35,   0.78,   0.5,   0.33,   0.18,   0.02,
    1.08,   0.59,   0.37,   0.23,   0.17,   0.0,
    1.32,   0.74,   0.46,   0.28,   0.27,   0.03,
    0.02,   0.0,   1.63,   1.01,   0.73,   0.55,
    0.41,   0.01,   0.06,   0.02,   1.26,   0.73,
    0.4,   0.3,   0.21,   0.0,   1.3,   0.7,
    0.4,   0.25,   0.14,   0.0),
  offset = c(1,   7,   13,   19,   25,   31,   37,   45,   53,   59,   65),
  time = c(
    2.0,   4.0,   6.0,   8.0,   10.0,   24.0,
    2.0,   4.0,   6.0,   8.0,   10.0,   24.0,
    2.0,   4.0,   6.0,   8.0,   10.0,   24.0,
    2.0,   4.0,   6.0,   8.0,   10.0,   24.0,
    2.0,   4.0,   6.0,   8.0,   10.0,   24.0,
    2.0,   4.0,   6.0,   8.0,   10.0,   24.0,
    2.0,   4.0,   6.0,   8.0,   10.0,   24.0,
    28.0,   32.0,   2.0,   4.0,   6.0,   8.0,
    10.0,   24.0,   28.0,   32.0,   2.0,   4.0,
    6.0,   8.0,   10.0,   24.0,   2.0,   4.0,
    6.0,   8.0,   10.0,   24.0),
  m = c(1.064710737,   2.708050201),
  T = structure(
    .Data = c(
      1.0E-4,   0.0,
      0.0,   1.0E-4),
    .Dim = c(2, 2)),
  R = structure(
    .Data = c(
      0.08,   0.0,
      0.0,   0.08),
    .Dim = c(2, 2)),
  k = 2.0)


# Initialize Y.rep with reasonable values
init <- list(
  list(alpha = 0, beta = 0, gamma = 0, logr.cont = 1)
  #list(Y.rep = data$Y + rnorm(66, 0, 5)),
  # list(Y.rep = data$Y + rnorm(66, 0, 5))
)





# JAGS模型代码
model_string <- textConnection("model {
for (i in 1:10) {
for (j in offset[i]:(offset[i+1]-1)) {
y[j] ~ dnorm(psi[j], inv.sigma.squared[i])
psi[j] <- D*exp(-CL[i]*time[j]/V[i])/V[i]
}
CL[i] <- exp(theta[i, 1])
V[i] <- exp(theta[i, 2])
theta[i, 1:2] ~ dmnorm(mu.theta[], inv.Omega[,])
sigma[i] <- abs(z[i])/sqrt(gamma[i])
z[i] ~ dnorm(0, inv.B.squared)
gamma[i] ~ dgamma(0.5, 0.5)
inv.sigma.squared[i] <- 1/pow(sigma[i], 2)
}
inv.B.squared <- 1/pow(B, 2)
B ~ dunif(0, 100)
mu.theta[1:2] ~ dmnorm(m[], T[,])
inv.Omega[1:2, 1:2] ~ dwish(R[,], k)
Omega[1:2, 1:2] <- inverse(inv.Omega[,])
}
")




#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
#model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

model <- jags.model(model_string, 
                    data = data, 
                    #inits = init,
                    n.chains = 2,
                    quiet = TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("mu.theta","Omega","B")  #, "deviance"
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)



data <- list(
  D = 30,
  y = c(
    1.09,   0.75,   0.53,   0.34,   0.23,   0.02,
    2.03,   1.28,   1.2,   1.02,   0.83,   0.28,
    1.44,   1.3,   0.95,   0.68,   0.52,   0.06,
    1.55,   0.96,   0.8,   0.62,   0.46,   0.08,
    1.35,   0.78,   0.5,   0.33,   0.18,   0.02,
    1.08,   0.59,   0.37,   0.23,   0.17,   0.0,
    1.32,   0.74,   0.46,   0.28,   0.27,   0.03,
    0.02,   0.0,   1.63,   1.01,   0.73,   0.55,
    0.41,   0.01,   0.06,   0.02,   1.26,   0.73,
    0.4,   0.3,   0.21,   0.0,   1.3,   0.7,
    0.4,   0.25,   0.14,   0.0),
  offset = c(1,   7,   13,   19,   25,   31,   37,   45,   53,   59,   65),
  time = c(
    2.0,   4.0,   6.0,   8.0,   10.0,   24.0,
    2.0,   4.0,   6.0,   8.0,   10.0,   24.0,
    2.0,   4.0,   6.0,   8.0,   10.0,   24.0,
    2.0,   4.0,   6.0,   8.0,   10.0,   24.0,
    2.0,   4.0,   6.0,   8.0,   10.0,   24.0,
    2.0,   4.0,   6.0,   8.0,   10.0,   24.0,
    2.0,   4.0,   6.0,   8.0,   10.0,   24.0,
    28.0,   32.0,   2.0,   4.0,   6.0,   8.0,
    10.0,   24.0,   28.0,   32.0,   2.0,   4.0,
    6.0,   8.0,   10.0,   24.0,   2.0,   4.0,
    6.0,   8.0,   10.0,   24.0),
  m = c(1.064710737,   2.708050201),
  T = structure(
    .Data = c(
      1.0E-4,   0.0,
      0.0,   1.0E-4),
    .Dim = c(2, 2)),
  R = structure(
    .Data = c(
      0.08,   0.0,
      0.0,   0.08),
    .Dim = c(2, 2)),
  k = 2.0)

# Initialize Y.rep with reasonable values
init <- list(
  list(alpha = 0, beta = 0, gamma = 0, logr.cont = 1)
  #list(Y.rep = data$Y + rnorm(66, 0, 5)),
  # list(Y.rep = data$Y + rnorm(66, 0, 5))
)





# JAGS模型代码
model_string <- textConnection("model {
for (i in 1:10) {
for (j in offset[i]:(offset[i+1]-1)) {
y[j] ~ dnorm(psi[j], inv.sigma.squared[i])
psi[j] <- D*exp(-CL[i]*time[j]/V[i])/V[i]
}
CL[i] <- exp(theta[i, 1])
V[i] <- exp(theta[i, 2])
theta[i, 1:2] ~ dmnorm(mu.theta[], inv.Omega[,])
log.sigma[i] ~ dnorm(mu.sigma, inv.omega.sigma.squared)
log(sigma[i]) <- log.sigma[i]
inv.sigma.squared[i] <- 1/pow(sigma[i], 2)
}
mu.sigma ~ dnorm(0, 0.0001)
med.sigma <- exp(mu.sigma)
omega.sigma ~ dunif(0, 100)
inv.omega.sigma.squared <- 1 / pow(omega.sigma, 2)
mu.theta[1:2] ~ dmnorm(m[], T[,])
inv.Omega[1:2, 1:2] ~ dwish(R[,], k)
Omega[1:2, 1:2] <- inverse(inv.Omega[,])
}


")




#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
#model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

model <- jags.model(model_string, 
                    data = data, 
                    #inits = init,
                    n.chains = 2,
                    quiet = TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("mu.theta","Omega","B")  #, "deviance"
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)













############10.8.1

data <- list(y = structure(.Data = c(15,21,29,16,18,21,16,26,33,27,41,60,33,38,41,20,27,42),
                           .Dim = c(6, 3)),
             x = c(0, 10, 33, 100, 333, 1000))

# Initialize Y.rep with reasonable values
init <- list(
  list(alpha = 0, beta = 0, gamma = 0, logr.cont = 1)
  #list(Y.rep = data$Y + rnorm(66, 0, 5)),
  # list(Y.rep = data$Y + rnorm(66, 0, 5))
)





# JAGS模型代码
model_string <- textConnection("model {
for (i in 1:N) {
for (j in 1:3) {
y[i,j] ~ dnorm(psi[i,j], inv.sigma.squared)
psi[i,j] <- alpha[i] + beta[i]*(t[i,j] - tbar)
+ gamma*( mu0 [i] - y0bar)
}
alpha[i] ~ dnorm(mu.alpha, inv.omega.alpha.squared)
beta[i] ~ dnorm(mu.beta, inv.omega.beta.squared)
mu0[i] ~ dnorm(mu.eps, inv.omega.eps.squared)
y0[i] ~ dnorm(mu0[i], inv.sigma.squared)
}
inv.sigma.squared <- 1/sigma.squared
inv.omega.alpha.squared <- 1/omega.alpha.squared
inv.omega.beta.squared <- 1/omega.beta.squared
inv.omega.eps.squared <- 1/omega.eps.squared
sigma.squared <- pow(sigma, 2)
omega.alpha.squared <- pow(omega.alpha, 2)
omega.beta.squared <- pow(omega.beta, 2)
omega.eps.squared <- pow(omega.eps, 2)
log(sigma) <- log.sigma
log.sigma ~ dunif(-10, 10)
omega.alpha ~ dunif(0, 100)
omega.beta ~ dunif(0, 100)
omega.eps ~ dunif(0, 100)
mu.alpha ~ dnorm(0, 0.0001)
mu.beta ~ dnorm(0, 0.0001)
mu.eps ~ dnorm(0, 0.0001)
gamma ~ dnorm(0, 0.0001)

y0bar <- mean(y0[])
}


")




#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
#model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

model <- jags.model(model_string, 
                    data = data, 
                    #inits = init,
                    n.chains = 2,
                    quiet = TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("alpha","beta","gamma"," mu")  #, "deviance"
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)



###############9.5.1

data <- list(y = c(-1, -0.3, 0.1, 0.2, 0.7, 1.2, 1.7, NA))

# Initialize Y.rep with reasonable values
init <- list(
  list(alpha = 0, beta = 0, gamma = 0, logr.cont = 1)
  #list(Y.rep = data$Y + rnorm(66, 0, 5)),
  # list(Y.rep = data$Y + rnorm(66, 0, 5))
)





# JAGS模型代码
model_string <- textConnection("model {
for (i in 1:8) {
z[i] <- 0
z[i] ~ dpois(phi[i])
phi[i] <- log(sigma) + 0.5*pow((y[i] - mu)/sigma, 2)
}
y[8] ~ dflat()
sigma ~ dunif(0, 100)
mu ~ dunif(-100, 100)
}


")




#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
#model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

model <- jags.model(model_string, 
                    data = data, 
                    #inits = init,
                    n.chains = 2,
                    quiet = TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("alpha","beta","gamma"," mu")  #, "deviance"
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)


#############9.6.1


data <- list(y = c(6,6,6,7,7,7,NA,NA,NA))

# Initialize Y.rep with reasonable values
init <- list(
  list(alpha = 0, beta = 0, gamma = 0, logr.cont = 1)
  #list(Y.rep = data$Y + rnorm(66, 0, 5)),
  # list(Y.rep = data$Y + rnorm(66, 0, 5))
)





# JAGS模型代码
model_string <- textConnection("model {
for (i in 1:6) {y[i] ~ dnorm(mu, 1)} # uncensored data
for (i in 7:9) {y[i] ~ dnorm(mu, 1)I(8,)} # censored data
mu ~ dunif(0, 100)
}


")




#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
#model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

model <- jags.model(model_string, 
                    data = data, 
                    #inits = init,
                    n.chains = 2,
                    quiet = TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("alpha","beta","gamma"," mu")  #, "deviance"
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)


##########9.6.2

data <- list(y = c(6,6,6,7,7,7))

# Initialize Y.rep with reasonable values
init <- list(
  list(alpha = 0, beta = 0, gamma = 0, logr.cont = 1)
  #list(Y.rep = data$Y + rnorm(66, 0, 5)),
  # list(Y.rep = data$Y + rnorm(66, 0, 5))
)





# JAGS模型代码
model_string <- textConnection("model {
for (i in 1:6) {
z[i] <- 1
z[i] ~ dbern(p[i])
p[i] <- exp(-0.5*(y[i] - mu)*(y[i] - mu))/phi(8 - mu)
}
mu ~ dunif(0, 100)
}

")




#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
#model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

model <- jags.model(model_string, 
                    data = data, 
                    #inits = init,
                    n.chains = 2,
                    quiet = TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("alpha","beta","gamma"," mu")  #, "deviance"
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)


##########9.6.3

data <- list(y=c(6,6,6,7,7,7,8,8,8))

# Initialize Y.rep with reasonable values
init <- list(
  list(alpha = 0, beta = 0, gamma = 0, logr.cont = 1)
  #list(Y.rep = data$Y + rnorm(66, 0, 5)),
  # list(Y.rep = data$Y + rnorm(66, 0, 5))
)





# JAGS模型代码
model_string <- textConnection("model {
for (i in 1:9) {
lower[i] <- y[i] - 0.5
upper[i] <- y[i] + 0.5
z[i] ~ dnorm(mu, 1)I(lower[i], upper[i])
}
mu ~ dunif(0, 100)
}

")




#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
#model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

model <- jags.model(model_string, 
                    data = data, 
                    #inits = init,
                    n.chains = 2,
                    quiet = TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("alpha","beta","gamma"," mu")  #, "deviance"
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)



############9.7.3

data <- list(N = 12, y=c(41,25,24,23,25,42,24,53,26,25,58,31),
             n=c(143,187,323,122,164,405,239,482,195,177,581,301))

# Initialize Y.rep with reasonable values
init <- list(
  list(alpha = 0, beta = 0, gamma = 0, logr.cont = 1)
  #list(Y.rep = data$Y + rnorm(66, 0, 5)),
  # list(Y.rep = data$Y + rnorm(66, 0, 5))
)





# JAGS模型代码
model_string <- textConnection("model {
for (i in 1:N) {
y[i] ~ dbin(theta[i], n[i])
logit(theta[i]) <- alpha + beta[i]
beta[i] <- b[i] - mean(b[])
b[i] ~ dunif(-10,10)
}
alpha ~ dunif(-10,10)
}
")




#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
#model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

model <- jags.model(model_string, 
                    data = data, 
                    #inits = init,
                    n.chains = 2,
                    quiet = TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("alpha","beta","b"," y")  #, "deviance"
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)


###########9.9.1

data <- list(N = 11, r=c(25,24,23,25,42,24,53,26,25,58,31),
             n=c(187,323,122,164,405,239,482,195,177,581,301))

# Initialize Y.rep with reasonable values
init <- list(
  list(alpha = 0, beta = 0, gamma = 0, logr.cont = 1)
  #list(Y.rep = data$Y + rnorm(66, 0, 5)),
  # list(Y.rep = data$Y + rnorm(66, 0, 5))
)





# JAGS模型代码
model_string <- textConnection("model {
for (i in 1:N) {
numbers1toN[i] <- i
p[i] ~ dbeta(0.5, 0.5)
r[i] ~ dbin(p[i], n[i])
hosp.rank[i] <- rank(p[], i) # rank of hospital i
prob.lowest[i] <- equals(hosp.rank[i], 1) # =1 if hosp i is lowest
prob.highest[i] <- equals(hosp.rank[i], N) # =1 if hosp i is highest
}
hosp.lowest <- inprod(numbers1toN[], prob.lowest[])
# index of lowest hosp
hosp.highest <- inprod(numbers1toN[], prob.highest[])
# index of highest hosp
}

")




#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
#model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

model <- jags.model(model_string, 
                    data = data, 
                    #inits = init,
                    n.chains = 2,
                    quiet = TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("alpha","beta","b"," y")  #, "deviance"
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)



#########10.1.1



data <- list(y=c(25,24,23,25,42,24,53,26,25,58,31),
             n=c(187,323,122,164,405,239,482,195,177,581,301))

# Initialize Y.rep with reasonable values
init <- list(
  list(alpha = 0, beta = 0, gamma = 0, logr.cont = 1)
  #list(Y.rep = data$Y + rnorm(66, 0, 5)),
  # list(Y.rep = data$Y + rnorm(66, 0, 5))
)





# JAGS模型代码
model_string <- textConnection("model {
for (i in 1:11) {
y[i] ~ dbin(theta[i], n[i])
logit(theta[i]) <- logit.theta[i]
logit.theta[i] ~ dnorm(mu, inv.omega.squared)
}
inv.omega.squared <- 1/pow(omega, 2)
omega ~ dunif(0, 100)
mu ~ dunif(-100, 100)
}

")




#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
#model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

model <- jags.model(model_string, 
                    data = data, 
                    #inits = init,
                    n.chains = 2,
                    quiet = TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("inv.omega.squared","omega","mu")  #, "deviance"
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)


################10.7.1

data <- list(N = 12, y=c(41,25,24,23,25,42,24,53,26,25,58,31),
             n=c(143,187,323,122,164,405,239,482,195,177,581,301))

# Initialize Y.rep with reasonable values
init <- list(
  list(alpha = 0, beta = 0, gamma = 0, logr.cont = 1)
  #list(Y.rep = data$Y + rnorm(66, 0, 5)),
  # list(Y.rep = data$Y + rnorm(66, 0, 5))
)





# JAGS模型代码
model_string <- textConnection("model {
for (i in 2:N) {
y[i] ~ dbin(theta[i], n[i])
logit(theta[i]) <- logit.theta[i]
logit.theta[i] ~ dnorm(mu, inv.omega.squared)
}
inv.omega.squared <- 1/pow(omega, 2)
omega ~ dunif(0, 10)
mu ~ dunif(-10, 10)
# Mixed predictions of centre 1:
logit.theta1.cv ~ dnorm(mu, inv.omega.squared)
# generate replicate log-odds:
logit(theta1.cv) <- logit.theta1.cv
# generate replicate deaths:
y1.cv ~ dbin(theta1.cv, n[1])
# use mid p-value:
P.mixed <- step(y1.cv - y[1] - 0.00001)
+ 0.5*equals(y1.cv, y[1])
}

")




#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
#model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

model <- jags.model(model_string, 
                    data = data, 
                    #inits = init,
                    n.chains = 2,
                    quiet = TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("y1.cv","omega","P.mixed")  #, "deviance"
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)


##############10.7.2


data <- list(N = 12, y=c(41,25,24,23,25,42,24,53,26,25,58,31),
             n=c(143,187,323,122,164,405,239,482,195,177,581,301))

# Initialize Y.rep with reasonable values
init <- list(
  list(alpha = 0, beta = 0, gamma = 0, logr.cont = 1)
  #list(Y.rep = data$Y + rnorm(66, 0, 5)),
  # list(Y.rep = data$Y + rnorm(66, 0, 5))
)





# JAGS模型代码
model_string <- textConnection("model {
for (i in 1:N) {
y[i] ~ dbin(theta[i], n[i])
logit(theta[i]) <- logit.theta[i]
logit.theta[i] ~ dnorm(mu, inv.omega.squared)
}
inv.omega.squared <- 1/pow(omega, 2)
omega ~ dunif(0, 10)
mu ~ dunif(-10, 10)
# Mixed predictions of centre 1:
logit.theta1.cv ~ dnorm(mu, inv.omega.squared)
# generate replicate log-odds:
logit(theta1.cv) <- logit.theta1.cv
# generate replicate deaths:
y1.cv ~ dbin(theta1.cv, n[1])
# use mid p-value:
P.mixed <- step(y1.cv - y[1] - 0.00001)
+ 0.5*equals(y1.cv, y[1])
}


")




#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
#model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

model <- jags.model(model_string, 
                    data = data, 
                    #inits = init,
                    n.chains = 2,
                    quiet = TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("y1.cv","omega","P.mixed")  #, "deviance"
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)



##########10.3.4

data <- list(y = structure(.Data = c(15,21,29,16,18,21,16,26,33,27,41,60,33,38,41,20,27,42),
                           .Dim = c(6, 3)),
             x = c(0, 10, 33, 100, 333, 1000))

# Initialize Y.rep with reasonable values
init <- list(
  list(alpha = 0, beta = 0, gamma = 0, logr.cont = 1)
  #list(Y.rep = data$Y + rnorm(66, 0, 5)),
  # list(Y.rep = data$Y + rnorm(66, 0, 5))
)





# JAGS模型代码
model_string <- textConnection("model {
for (i in 1:npupil) {
Goals[i] ~ dcat(p[School[i],])
}
for (j in 1:nschool) {
for (k in 1:3) {
p[j,k] <- q[j,k]/sum(q[j,])
q[j,k] ~ dgamma(a[k], 1)
}
}
for (k in 1:3) {
a[k] ~ dgamma(1, 0.001)
p.pop[k] <- a[k]/sum(a[]) # population mean of p[,k]
}
dummy <- Gender[1] # stop WinBUGS complaining about unused variable
}


")




#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
#model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

model <- jags.model(model_string, 
                    data = data, 
                    #inits = init,
                    n.chains = 2,
                    quiet = TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("y1.cv","omega","P.mixed")  #, "deviance"
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)



##########################33


data <- list(y = structure(.Data = c(15,21,29,16,18,21,16,26,33,27,41,60,33,38,41,20,27,42),
                           .Dim = c(6, 3)),
             x = c(0, 10, 33, 100, 333, 1000))

# Initialize Y.rep with reasonable values
init <- list(
  list(alpha = 0, beta = 0, gamma = 0, logr.cont = 1)
  #list(Y.rep = data$Y + rnorm(66, 0, 5)),
  # list(Y.rep = data$Y + rnorm(66, 0, 5))
)





# JAGS模型代码
model_string <- textConnection("model {
for (i in 1:npupil) {
Goals[i] ~ dcat(p[i,])
for (k in 1:3) {
p[i,k] <- q[i,k]/sum(q[i,])
log(q[i,k]) <- a[i,k]
}
a[i,1] <- b[1] + b.boy*Gender[i]
a[i,2] <- b[2]
a[i,3] <- 0
}
b[1] ~ dnorm(0, 0.0001)
b[2] ~ dnorm(0, 0.0001)
b.boy ~ dnorm(0, 0.0001)
or.boy <- exp(b.boy)

qboy[1] <- exp(b[1] + b.boy); qboy[2] <- exp(b[2]); qboy[3] <- 1
qgirl[1] <- exp(b[1]); qgirl[2] <- exp(b[2]); qgirl[3] <- 1

# Probabilities of preferring 1) sports 2) popularity 3) grades for boys and girls separately
for (k in 1:3) {
   p.boy[k] <- qboy[k]/sum(qboy[])
   p.girl[k] <- qgirl[k]/sum(qgirl[])
}
dummy <- School[1] + nschool
} 


")




#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
#model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

model <- jags.model(model_string, 
                    data = data, 
                    #inits = init,
                    n.chains = 2,
                    quiet = TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("y1.cv","omega","P.mixed")  #, "deviance"
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none",thin=1)

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)


