#2025.12. 12
# example 10.1.1
#on BUGS book 8.4.1 Bristol data set
# hierarchical model
# use 1 to 12 hospitals (total 12) 
#
library(rjags)
library(coda)

library(ggplot2)
library(reshape2)

#load 11 hospital Data:
hos_death <- c(4,3,2,2,3,4,2,5,3,3,6,3)
hos_total <- c(14,19,32,12,16,41,24,48,20,18,58,30)

#hos_death <- c(25,24,23,25,42,24,53,26,25,58,31)
#hos_total <- c(87,323,122,164,405,239,482,195,177,581,301)
n <- length(hos_total)



# pack as a list for JAGS
data <-  list(y=hos_death, n=hos_total,N=n)

#define model as  a string
model_string <- textConnection("model{

#likelihood  #use N instead, more generic
for (i in 1:N) {
  y[i] ~ dbin(theta[i], n[i])
  logit(theta[i]) <- logit.theta[i]
  logit.theta[i]   ~ dnorm(mu, inv.omega.squared)
}
inv.omega.squared <- 1/pow(omega,2)
omega             ~ dunif(0,100)
mu                ~ dunif(-100,100)


}")

#load data and compile MCMC code , no inits
#inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
model <- jags.model(model_string,data = data,  n.chains=2,quiet=TRUE)

#burn in 10000 samples
update(model, 10000, progress.bar="none")

#gen 20000 post burn in samples  and retain param in params
params  <- c("theta", "y")
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none")

#sum
summary(samples)

#model comparison
dic2<- dic.samples(model, n.iter=10000, progress.bar="none")  
dic2
