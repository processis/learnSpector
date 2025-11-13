#2025.11.13
#try to adopt BUGS 8.3.1 code to JAGS
#use beta binomial model for a proportion in JAGS
#on BUGS book 8.3.1 Bristol data set
#
library(rjags)
library(coda)

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

plot(samples)
