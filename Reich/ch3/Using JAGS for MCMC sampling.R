#Using JAGS for MCMC sampling

library(rjags)

mass <- c(29.9, 1761, 1807, 2984, 3230, 5040, 5654)
age  <- c(2, 15, 14, 16, 18, 22, 28)
n    <- length(age)

# JAGS require all the data to be packaged as a list
data <- list(mass=mass,age=age,n=n) 

model_string <- textConnection("model{

   # Likelihood (dnorm uses a precision, not variance)
   for(i in 1:n){
     mass[i] ~ dnorm(beta1 + beta2*age[i],tau) #tau = 1/sigma^2
   }

   # Priors
   tau   ~  dgamma(0.1, 0.1)
   sigma <- 1/sqrt(tau)
   beta1 ~  dnorm(0, 0.001)
   beta2 ~  dnorm(0, 0.001)

 }")


inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
model <- jags.model(model_string,data = data, inits=inits, n.chains=2,quiet=TRUE)

update(model, 10000, progress.bar="none")

params  <- c("beta1","beta2","sigma")
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none")

summary(samples)

plot(samples)