#Convergence diagnostics for a well-behaved model

model_string <- textConnection("model{
   Y1    ~ dpois(exp(mu[1]))
   Y2    ~ dpois(exp(mu[2]))
   mu[1] ~ dnorm(0,0.001)
   mu[2] ~ dnorm(0,0.001)
 }")

inits <- list(mu=rnorm(2,0,5))
data  <- list(Y1=1,Y2=10)
model <- jags.model(model_string,data = data, inits=inits, n.chains=3, quiet=TRUE)

update(model, 1000, progress.bar="none")
samples <- coda.samples(model, 
                        variable.names=c("mu"), 
                        n.iter=5000, progress.bar="none")



plot(samples)

autocorr.plot(samples)

# Low autocorrelation indicates convergence
autocorr(samples[[1]],lag=1)

# ESS over 1000 indicates convergence
effectiveSize(samples)

# R less than 1.1 indicates convergence
gelman.diag(samples)

# |z| less than 2 indicates convergence
geweke.diag(samples[[1]])