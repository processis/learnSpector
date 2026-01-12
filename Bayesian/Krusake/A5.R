#Listing 7.10

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
 
 #Listing 7.11
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
 
  
  
  
  ##############
  
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
   
   
#Listing 7.13
   
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
    
    
    
    #Listing 7.14
    
    model_string <- textConnection("model{
 # Likelihood
 for(i in 1:n){for(j in 1:m){
 Y[i,j] ~ dnorm(alpha1[i]+alpha2[i]*age[j],tau3)
 }}

# Random effects
 for(i in 1:n){
alpha1[i] ~ dnorm(mu1,tau1)
 alpha2[i] ~ dnorm(mu2,tau2)
 }

 # Priors
 mu1 ~ dnorm(0,0.0001)
 mu2 ~ dnorm(0,0.0001)
 tau1 ~ dgamma(0.1,0.1)
 tau2 ~ dgamma(0.1,0.1)
 tau3 ~ dgamma(0.1,0.1)
 }")
    
     data <- list(Y=Y,age=age,n=n,m=m)
     params <- c("mu1","mu2","tau1","tau2","tau3")
     model <- jags.model(model_string,data = data,
                           n.chains=2,quiet=TRUE)
     update(model, 10000, progress.bar="none")
     samples <- coda.samples(model, variable.names=params,
                                n.iter=90000, progress.bar="none")
     summary(samples)
     
     
     
     
     
#Listing 7.15
     
     model_string <- function(){
        # Likelihood
        for(i in 1:n){for(j in 1:m){
          Y[i,j] ~ dnorm(mn[i,j],tau3)
          mn[i,j] <- alpha1[i]+alpha2[i]*age[j]
          }}
       
        # Random effects
        for(i in 1:n){
          alpha1[i] ~ dnorm(mu1,tau1)
          alpha2[i] ~ dnorm(mu2,tau2)
          }
       
        # Priors
        mu1 ~ dnorm(0,0.0001)
        mu2 ~ dnorm(0,0.0001)
        tau1 ~ dgamma(0.1,0.1)
        tau2 ~ dgamma(0.1,0.1)
        tau3 ~ dgamma(0.1,0.1)
        }
     
      data <- list(Y=Y,age=age,n=n,m=m)
      params <- c("mu1","mu2","tau1","tau2","tau3")
      inits <- function(){list(mu1=0,mu2=0,tau1=.1,tau2=.2,tau3=.2)}
      fit <- bugs(model.file=model_string,
                     data=data,inits=inits,
                     parameters.to.save=params,DIC=FALSE,
                     n.iter=90000,n.chains=2,n.burnin=10000)
      fit$summary
      
      
#Listing 7.16
      
      stan_model <- "

 data {
 int<lower=0> n;
 int<lower=0> m;
 vector [m] age;
 matrix [n,m] Y;
 }

 parameters {
 vector [n] alpha1;
 vector [n] alpha2;
 real mu1;
 real mu2;
 real<lower=0> sigma3;
 real<lower=0> sigma2;
 real<lower=0> sigma1;
 }

 model {
 real mu;
 alpha1 ~ normal(0,sigma1);
 alpha2 ~ normal(0,sigma2);
 sigma1 ~ cauchy(0.0,1000);
 sigma2 ~ cauchy(0.0,1000);
 sigma3 ~ cauchy(0.0,1000);
 mu1 ~ normal(0,1000);
 mu2 ~ normal(0,1000);

 for(i in 1:n){for(j in 1:m){
 mu = alpha1[i] + alpha2[i]*age[j];
 Y[i,j] ~ normal(mu,sigma3);
 }}
 }
 "
      
       data <- list(Y=Y,age=age,n=n,m=m)
       fit_stan <- stan(model_code = stan_model,
                           data = data,
                           chains=2, warmup = 10000, iter = 100000)
       summary(fit_stan)$summary
       
  #Listing 7.17
       
       library(nimble)
       
        model_string <- nimbleCode({
          # Likelihood
          for(i in 1:n){for(j in 1:m){
            Y[i,j] ~ dnorm(mn[i,j],tau3)
            mn[i,j] <- alpha1[i]+alpha2[i]*age[j]
           }}
         
          # Random effects
          for(i in 1:n){
            alpha1[i] ~ dnorm(mu1,tau1)
            alpha2[i] ~ dnorm(mu2,tau2)
            }
         
          # Priors
          mu1 ~ dnorm(0,0.0001)
          mu2 ~ dnorm(0,0.0001)
          tau1 ~ dgamma(0.1,0.1)
          tau2 ~ dgamma(0.1,0.1)
          tau3 ~ dgamma(0.1,0.1)
          })
       
        params <- c("mu1","mu2","tau1","tau2","tau3")
        consts <- list(n=n,m=m,age=age)
        data <- list(Y=Y)
        inits <- function(){
          list(mu1=rnorm(1),mu2=rnorm(1),tau1=10,tau2=10,tau3=10)
          }
        samples <- nimbleMCMC(model_string, data = data, inits = inits,
                                 constants=consts,
                                 monitors = params,
                                 samplesAsCodaMCMC=TRUE,WAIC=FALSE,
                                 niter = 100000, nburnin = 10000, nchains = 2)
        plot(samples)