
library(rjags)
library(coda)

library(ggplot2)
library(reshape2)


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

