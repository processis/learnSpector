#Understanding error messages in JAGS

# Generate data
n <- 20
p <- 2
X <- rnorm(n)
Y <- rnorm(n,X,1)

# Hyperpriors

prec_beta <- 0.01
a         <- 0.01
b         <- 0.01

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

rm(samp,model,data,keep)

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

rm(samp,model,data,keep)

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