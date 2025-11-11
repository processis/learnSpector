#Comparison of Bayesian software

m   <- 4
n   <- 20
age <- c(8.0, 8.5, 9.0, 9.5)
Y   <- c(47.8, 48.8, 49.0, 49.7,
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
         46.3, 47.6, 51.3, 51.8) 

Y <- matrix(Y,20,4,byrow=TRUE)

plot(NA,xlim=range(age),ylim=range(Y),xlab="Age",ylab="Bone density")
for(i in 1:n){
  lines(age,Y[i,])
  points(age,Y[i,],pch=19)
}



library(rjags)


set.seed(0820)

tick <- proc.time()[3]
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

    mu1 ~ dnorm(0,0.0001)
    mu2 ~ dnorm(0,0.0001)
   tau1 ~ dgamma(0.1,0.1)
   tau2 ~ dgamma(0.1,0.1)
   tau3 ~ dgamma(0.1,0.1)
 }")

data    <- list(Y=Y,age=age,n=n,m=m)
params  <- c("mu1","mu2","tau1","tau2","tau3")
model   <- jags.model(model_string,data = data, n.chains=2,quiet=TRUE)
update(model, 10000, progress.bar="none")
samples <- coda.samples(model, variable.names=params,
                        n.iter=90000, progress.bar="none")
tock <- proc.time()[3]
tock-tick

effectiveSize(samples)

library(R2OpenBUGS) 

set.seed(0820)

tick <- proc.time()[3]

model_string <- function(){
  # Likelihood
  for(i in 1:n){for(j in 1:m){
    Y[i,j]   ~ dnorm(mn[i,j],tau3)
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

data    <- list(Y=Y,age=age,n=n,m=m)
params  <- c("mu1","mu2","tau1","tau2","tau3")
inits <- function(){list(mu1=0,mu2=0,tau1=.1,tau2=.2,tau3=.2)}
fit   <- bugs(model.file=model_string,
              data=data,inits=inits,
              parameters.to.save=params,DIC=FALSE,
              n.iter=100000,n.chains=2,n.burnin=10000)
tock <- proc.time()[3]
tock-tick

fit$summary[,9]

library(rstan)

set.seed(0820)

tick <- proc.time()[3]
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
     mu1    ~ normal(0,10000);
     mu2    ~ normal(0,10000);

     for(i in 1:n){for(j in 1:m){
       mu      = alpha1[i] + alpha2[i]*age[j]; 
       Y[i,j]  ~ normal(mu,sigma3);
     }}
   }
 "

data    <- list(Y=Y,age=age,n=n,m=m)
fit_stan <- stan(model_code = stan_model,
                 data = data,
                 chains=2, warmup = 10000, iter = 100000)

tock <- proc.time()[3]
tock-tick

summary(fit_stan)$summary[41:45,9:10] 

library(nimble)

set.seed(0820)

tick <- proc.time()[3]

model_string <- nimbleCode({
  # Likelihood
  for(i in 1:n){for(j in 1:m){
    Y[i,j]   ~ dnorm(mn[i,j],tau3)
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

consts   <- list(n=n,m=m,age=age)
data     <- list(Y=Y)
inits    <- function(){list(mu1=rnorm(1),mu2=rnorm(1),tau1=10,tau2=10,tau3=10)}
samples  <- nimbleMCMC(model_string, data = data, inits = inits,
                       constants=consts,
                       monitors = c("mu1", "mu2","tau1","tau2","tau3"),
                       samplesAsCodaMCMC=TRUE,WAIC=FALSE,
                       niter = 100000, nburnin = 10000, nchains = 2)

tock <- proc.time()[3]
tock-tick

effectiveSize(samples)
