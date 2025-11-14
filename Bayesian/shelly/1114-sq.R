library(rjags)
library(coda)

library(ggplot2)
library(reshape2)


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


