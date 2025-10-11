#Chapter 3.3: Introduction to JAGS

#Load T-Rex data
library(rjags)

mass <- c(29.9, 1761, 1807, 2984, 3230, 5040, 5654)
age  <- c(2, 15, 14, 16, 18, 22, 28)
n    <- length(age)

# JAGS require all the data to be packaged as a list
data <- list(mass=mass,age=age,n=n) 

#(1) Define the model as a string

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

#(2) Load the data and compile the MCMC code

inits <- list(beta1=rnorm(1),beta2=rnorm(1),tau=10)
model <- jags.model(model_string,data = data, inits=inits, n.chains=2,quiet=TRUE)

#(3) Burn-in for 10000 samples

update(model, 10000, progress.bar="none")

#(4) Generate 20000 post-burn-in samples and retain the parameters named in params

params  <- c("beta1","beta2","sigma")
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none")


#(5) Summarize the output

summary(samples)

plot(samples)

######################################

#Load concussions data

library(rjags)

# Number of concussions in 2012-2015
Y <- c(171, 152, 123, 199)
n <- 4
N <- 256


#(1) Define the model as a string

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

#(2) Load the data and compile the MCMC code

inits <- list(lambda=rgamma(n,1,1),gamma=1)
data  <- list(Y=Y,N=N,n=n,a=0.1,b=0.1)
model <- jags.model(model_string,data = data, inits=inits, n.chains=2)

#(3) Burn-in for 10000 samples

update(model, 10000, progress.bar="none")

#(4) Generate 20000 post-burn-in samples

params  <- c("lambda")
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none")

#(5) Summarize the output

summary(samples)

plot(samples)

###Understanding error messages in JAGS

# Generate data
n <- 20
p <- 2
X <- rnorm(n)
Y <- rnorm(n,X,1)

# Hyperpriors

prec_beta <- 0.01
a         <- 0.01
b         <- 0.01


#(0) Correct model

model_string <- "model{

  # Likelihood
  for(i in 1:n){
    Y[i] ~ dnorm(beta[1] + X[i]*beta[2],tau)
  }

  # Prior
  for(j in 1:p){beta[j] ~ dnorm(0, prec_beta)}
  tau ~ dgamma(a,b)

 }"

library(rjags)
data  <- list(Y=Y,X=X,p=p,n=n,prec_beta=prec_beta,a=a,b=b)
keep  <- c("beta","tau")

model <- jags.model(textConnection(model_string), data = data,quiet=TRUE)
update(model, 1000, progress.bar="none")
samp  <- coda.samples(model, variable.names=keep, n.iter=2000, progress.bar="none")
plot(samp)


rm(samp,model,data,keep)

#(1) Forget to pass a variable as data

model_string <- "model{

  # Likelihood
  for(i in 1:n){
    Y[i] ~ dnorm(beta[1] + X[i]*beta[2],tau)
  }

  # Prior
  for(j in 1:p){beta[j] ~ dnorm(0, prec_beta)}
  tau ~ dgamma(a,b)

 }"

library(rjags)
data  <- list(Y=Y,X=X,p=p,n=n,prec_beta=prec_beta,a=a) # Drop b!
keep  <- c("beta","tau")

model <- jags.model(textConnection(model_string), data = data,quiet=TRUE)


update(model, 1000, progress.bar="none")

samp  <- coda.samples(model, variable.names=keep, n.iter=2000, progress.bar="none")


plot(samp)

rm(samp,model,data,keep)

##(2) Missing values

model_string <- "model{

  # Likelihood
  for(i in 1:n){
    Y[i] ~ dnorm(beta[1] + X[i]*beta[2],tau)
  }

  # Prior
  for(j in 1:p){beta[j] ~ dnorm(0, prec_beta)}
  tau ~ dgamma(a,b)

 }"

library(rjags)
X_miss    <- X  # Add a missing X
X_miss[1] <- NA

data  <- list(Y=Y,X=X_miss,p=p,n=n,prec_beta=prec_beta,a=a,b=b)
keep  <- c("beta","tau")

model <- jags.model(textConnection(model_string), data = data,quiet=TRUE)

update(model, 1000, progress.bar="none")

samp  <- coda.samples(model, variable.names=keep, n.iter=2000, progress.bar="none")

plot(samp)

rm(samp,model,data,keep,X_miss)


#(3) Defining a variables twice

model_string <- "model{

  # Likelihood
  for(i in 1:n){
    Y[1] ~ dnorm(beta[1] + X[i]*beta[2],tau)  # It should be Y[i] not Y[1]
  }

  # Prior
  for(j in 1:p){beta[j] ~ dnorm(0, prec_beta)}
  tau ~ dgamma(a,b)

 }"

library(rjags)
data  <- list(Y=Y,X=X,p=p,n=n,prec_beta=prec_beta,a=a,b=b)
keep  <- c("beta","tau")

model <- jags.model(textConnection(model_string), data = data,quiet=TRUE)

update(model, 1000, progress.bar="none")

samp  <- coda.samples(model, variable.names=keep, n.iter=2000, progress.bar="none")

plot(samp)

rm(samp,model,data,keep,X_miss)

#(4) Priors with invalid range or intial values
model_string <- "model{

  # Likelihood
  for(i in 1:n){
    Y[i] ~ dnorm(beta[1] + X[i]*beta[2],tau)  # It should be Y[i] not Y[1]
  }

  # Prior
  for(j in 1:p){beta[j] ~ dnorm(0, prec_beta)}
  tau ~ dunif(-2,-1) # Crazy prior!

 }"

library(rjags)
data  <- list(Y=Y,X=X,p=p,n=n,prec_beta=prec_beta)
keep  <- c("beta","tau")

model <- jags.model(textConnection(model_string), data = data,quiet=TRUE)

update(model, 1000, progress.bar="none")

samp  <- coda.samples(model, variable.names=keep, n.iter=2000, progress.bar="none")

plot(samp)

rm(samp,model,data,keep,X_miss)

#(5) Passing variables that are not used
model_string <- "model{

  # Likelihood
  for(i in 1:n){
    Y[i] ~ dnorm(beta[1] + X[i]*beta[2],tau)
  }

  # Prior
  for(j in 1:p){beta[j] ~ dnorm(0, prec_beta)}
  tau ~ dgamma(.1,.1) # No a or b!

 }"

library(rjags)
data  <- list(Y=Y,X=X,p=p,n=n,prec_beta=prec_beta,a=a,b=b)
keep  <- c("beta","tau")

model <- jags.model(textConnection(model_string), data = data,quiet=TRUE)

update(model, 1000, progress.bar="none")
samp  <- coda.samples(model, variable.names=keep, n.iter=2000, progress.bar="none")
plot(samp)

rm(samp,model,data,keep)

##(6) Sending an invalid list of parameters to keep

model_string <- "model{

  # Likelihood
  for(i in 1:n){
    Y[i] ~ dnorm(beta[1] + X[i]*beta[2],tau)
  }

  # Prior
  for(j in 1:p){beta[j] ~ dnorm(0, prec_beta)}
  tau ~ dgamma(a,b)

 }"

library(rjags)
data  <- list(Y=Y,X=X,p=p,n=n,prec_beta=prec_beta,a=a,b=b)
keep  <- c("beta","tau","theta") # Ah!

model <- jags.model(textConnection(model_string), data = data,quiet=TRUE)
update(model, 1000, progress.bar="none")
samp  <- coda.samples(model, variable.names=keep, n.iter=2000, progress.bar="none")

plot(samp)

rm(samp,model,data,keep)