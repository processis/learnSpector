#Convergence diagnostics for a ill-posed model

model_string <- textConnection("model{
   Y     ~ dpois(exp(mu[1]+mu[2]))
   mu[1] ~ dnorm(0,0.001)
   mu[2] ~ dnorm(0,0.001)
 }")

inits <- list(mu=rnorm(2,0,5))
data  <- list(Y=1)
model <- jags.model(model_string,data = data, inits=inits, n.chains=3, quiet=TRUE)

update(model, 1000, progress.bar="none")
samples <- coda.samples(model, 
                        variable.names=c("mu"), 
                        n.iter=5000, progress.bar="none")

plot(samples)

autocorr.plot(samples)

# Autocorrelation near 1 indicates poor convergence
autocorr(samples[[1]],lag=1)

# Low ESS indicates poor convergence
effectiveSize(samples)

# R greater than 1.1 indicates poor convergence
gelman.diag(samples)

# |z| greater than 2 indicates poor convergence
geweke.diag(samples[[1]])