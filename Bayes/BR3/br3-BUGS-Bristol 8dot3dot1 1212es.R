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
  

hos_death <- c(4,3,2,2,3,4,2,5,3,3,6,3)
hos_total <- c(14,19,32,12,16,41,24,48,20,18,58,30)
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

# prior 
theta ~ dunif(0,1)  # uniform dist as prior on theta

}")

#load data and compile MCMC code , no inits
model <- jags.model(model_string,data = data,  n.chains=2, quiet=TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("res","theta")
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none")

#sum
summary(samples)

# Open a new device with controlled size
dev.new(width=20, height=20)  # Adjust dimensions as needed

plot(samples)


#箱线图

# 提取样本数据并转换为数据框
samples_matrix <- as.matrix(samples)
samples_df <- as.data.frame(samples_matrix)

# 提取res2变量（残差平方）
res_columns <- grep("res", colnames(samples_df), value = TRUE)

# 将数据转换为长格式
samples_long <- melt(samples_df)

# 分离theta和res2变量
#theta_data <- samples_long[grep("theta", samples_long$variable), ]
res_data <- samples_long[grep("res", samples_long$variable), ]



# 绘制res2的箱线图（按医院）
ggplot(res_data, aes(x = variable, y = value)) +
  geom_boxplot(fill = "lightgreen", alpha = 0.7) +
  labs(title = "Boxplot of Residuals by Hospital",
       x = "Hospital",
       y = "Residuals") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#model comparison
dic1<- dic.samples(model, n.iter=10000, progress.bar="none")
dic1
