#JAGS code for simple linear regression for the paleo data

mass <- c(29.9, 1761, 1807, 2984, 3230, 5040, 5654)
 age <- c(2, 15, 14, 16, 18, 22, 28)
 n <- length(age)

 # Fit in JAGS
 #install.packages("rjags")
 library(rjags)

 model_string <- textConnection("model{
 for(i in 1:n){
 mass[i] ~ dnorm(beta1 + beta2*age[i],tau)
 }
 tau ~ dgamma(0.01, 0.01)
 beta1 ~ dnorm(0, 0.0000001)
 beta2 ~ dnorm(0, 0.0000001)
 }")

data <- list(mass=mass,age=age,n=n)
 inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
 model <- jags.model(model_string, data = data,
                       inits=inits,n.chains=2)

update(model, 10000)
samples <- coda.samples(model, n.iter=20000,
                           variable.names=c("beta1","beta2"))
summary(samples)


#OpenBUGS code for simple linear regression for the paleo data.

mass <- c(29.9, 1761, 1807, 2984, 3230, 5040, 5654)
 age <- c(2, 15, 14, 16, 18, 22, 28)
 n <- length(age)

 #install.packages("R2OpenBUGS")
 library(R2OpenBUGS)

 model_string <- function() {
   for(i in 1:n){
     mass[i] ~ dnorm(mn[i],tau)
     mn[i] <- beta1 + beta2*age[i]
     }
   tau ~ dgamma(0.01, 0.01)
   beta1 ~ dnorm(0, 0.0000001)
   beta2 ~ dnorm(0, 0.0000001)
   }

 data <- list(mass=mass,age=age,n=n)
 inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
 fit <- bugs(model.file=model_string,
                data=data,inits=inits,
                parameters.to.save=c("beta1","beta2"),
                n.iter=30000,n.burnin=10000,n.chains=2)
 fit
 
 #STAN code for simple linear regression for the paleo data.
 
 mass <- c(29.9, 1761, 1807, 2984, 3230, 5040, 5654)
  age <- c(2, 15, 14, 16, 18, 22, 28)
  n <- length(age)
 
  #install.packages("rstan")
  library(rstan)
 
  stan_model <- "

 data {
 int<lower=0> n;
 vector [n] mass;
 vector [n] age;
 }

 parameters {
 real beta1;
 real beta2;
 real<lower=0> sigma;
 }

 model {
 vector [n] mu;
 beta1 ~ normal(0,1000000);
 beta2 ~ normal(0,1000000);
 sigma ~ cauchy(0.0,1000);
 mu = beta1 + beta2*age;
 mass ~ normal(mu,sigma);
 }
 "
 
  data <- list(n=n,age=age,mass=mass)
  fit_stan <- stan(model_code = stan_model,
                      data = data, chains=2, warmup = 10000, iter =
                       30000)
  fit_stan
  
  
#NIMBLE code for simple linear regression for the paleo data.
  
  mass <- c(29.9, 1761, 1807, 2984, 3230, 5040, 5654)
   age <- c(2, 15, 14, 16, 18, 22, 28)
   n <- length(age)
  
   #install.packages("nimble")
   library(nimble)
  
   model_string <- nimbleCode({
     for(i in 1:n){
       mass[i] ~ dnorm(mn[i],tau)
       mn[i] <- beta1 + beta2*age[i]
       }
     tau ~ dgamma(0.01, 0.01)
     beta1 ~ dnorm(0, 0.0000001)
     beta2 ~ dnorm(0, 0.0000001)
     })
  
   consts <- list(n=n,age=age)
   data <- list(mass=mass)
   inits <- function(){list(beta1=rnorm(1),beta2=rnorm(1),tau=10)}
   samples <- nimbleMCMC(model_string, data = data, inits = inits,
                            constants=consts,
                            monitors = c("beta1", "beta2"),
                            samplesAsCodaMCMC=TRUE,WAIC=FALSE,
                            niter = 30000, nburnin = 10000, nchains = 2)
   plot(samples)
   effectiveSize(samples)