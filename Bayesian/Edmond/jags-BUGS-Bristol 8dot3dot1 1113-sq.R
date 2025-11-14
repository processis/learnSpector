#2025.11.13
#try to adopt BUGS 8.3.1 code to JAGS
#use beta binomial model for a proportion in JAGS
#on BUGS book 8.3.1 Bristol data set
#
library(rjags)
library(coda)

library(ggplot2)
library(reshape2)

#load 12 hospital Data:
  

hos_death <- c(41,25,24,23,25,42,24,53,26,25,58,31)
hos_total <- c(143,187,323,122,164,405,239,482,195,177,581,301)
n <- length(hos_total)



# pack as a list for JAGS
data <-  list(y=hos_death, n=hos_total,N=n)

#define model as  a string
model_string <- textConnection("model{

#likelihood  #use N instead of 12 in book, more generic
for (i in 1:N) {
  y[i] ~ dbin(theta, n[i])
  res[i] <- (y[i] - n[i]*theta)/sqrt(n[i]*theta*(1-theta))
  res2[i] <- res[i]*res[i] 
}

# prior for b : beta
theta ~ dunif(0,1)  # uniform dist as prior on theta

X2.obs <- sum(res2[]) #sum of squared stand.resids
 

}")

#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("res2","theta")
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none")

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=7, height=5)  # Adjust dimensions as needed

plot(samples)

# For base R - specify dimensions
dev.off()  # Close current device
pdf("output.pdf", width=8, height=6)  # Or adjust dimensions
plot(samples)
dev.off()


#箱线图

# 提取样本数据并转换为数据框
samples_matrix <- as.matrix(samples)
samples_df <- as.data.frame(samples_matrix)

# 提取res2变量（残差平方）
res2_columns <- grep("res2", colnames(samples_df), value = TRUE)

res_columns <- grep("res", colnames(samples_df), value = TRUE)

# 将数据转换为长格式
samples_long <- melt(samples_df)

# 分离theta和res2变量
theta_data <- samples_long[grep("theta", samples_long$variable), ]
res2_data <- samples_long[grep("res2", samples_long$variable), ]

res_data <- samples_long[grep("res", samples_long$variable), ]

# 绘制theta的箱线图
ggplot(theta_data, aes(x = variable, y = value)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  labs(title = "Boxplot of theta Parameter",
       x = "Parameter",
       y = "Value") +
  theme_minimal()

# 绘制res2的箱线图（按医院）
ggplot(res2_data, aes(x = variable, y = value)) +
  geom_boxplot(fill = "lightgreen", alpha = 0.7) +
  labs(title = "Boxplot of Squared Residuals by Hospital",
       x = "Hospital",
       y = "Squared Residuals") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





# 将数据转换为长格式
samples_long <- melt(samples_df)

# 创建变量类型分组
samples_long$type <- ifelse(grepl("theta", samples_long$variable), "theta", "res2")

# 绘制箱线图
ggplot(samples_long, aes(x = variable, y = value, fill = type)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("theta" = "lightblue", "res2" = "lightgreen")) +
  labs(title = "Boxplot of theta and res2 Parameters",
       x = "Parameter",
       y = "Value",
       fill = "Parameter Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
