#Using JAGS for concussions data

library(rjags)

# Number of concussions in 2012-2015
Y <- c(171, 152, 123, 199)
n <- 4
N <- 256

model_string <- textConnection("model{
   # Likelihood
    for(i in 1:n){
      Y[i] ~ dpois(N*lambda[i])
    }
   # Priors
    for(i in 1:n){
      lambda[i] ~ dgamma(1,gamma)
    }
    gamma   ~  dgamma(a, b)
 }")

inits <- list(lambda=rgamma(n,1,1),gamma=1)
data  <- list(Y=Y,N=N,n=n,a=0.1,b=0.1)
model <- jags.model(model_string,data = data, inits=inits, n.chains=2)

update(model, 10000, progress.bar="none")

params  <- c("lambda")
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none")

summary(samples)

plot(samples)