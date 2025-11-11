require(rjags)            
fileNameRoot="7dot10paleo" # For output file names.

#Reich JAGS code for simple linear regression for paleo data
mass <- c(29.9, 1761, 1807, 2984, 3230, 5040, 5654)
age <- c(2, 15, 14, 16, 18, 22, 28)
n <- length(age)

model_string <- textConnection("model{
                               for (i in 1:n){
                               mass[i]  ~ dnorm(beta1 + beta2*age[i],tau)
                              }
                               tau ~ dgamma(0.01, 0.01)
                               beta1 ~ dnorm(0,0.0000001)
                               beta2 ~ dnorm(0,0.0000001)
                               }")

data <- list(mass=mass, age=age, n=n)
inits <- list(beta=rnorm(1), beta2=rnorm(1), tau=10)
model <- jags.model(model_string, data = data, inits=inits, n.chains=2)

update(model, 10000)
samples <- coda.samples(model, n.iter=20000,
                      variable.names = c("beta1","beta2"))
summary(samples)
