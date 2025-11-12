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