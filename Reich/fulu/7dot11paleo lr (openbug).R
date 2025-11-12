

#Reich JAGS code for simple linear regression for paleo data
mass <- c(29.9, 1761, 1807, 2984, 3230, 5040, 5654)
age <- c(2, 15, 14, 16, 18, 22, 28)
n <- length(age)

library(R2OpenBUGS)

model_string <- function(){
                               for (i in 1:n){
                               mass[i]  ~ dnorm(mn[i],tau)
                                 mn[i]<-beta1+beta2*age[i]
                              }
                               tau ~ dgamma(0.01, 0.01)
                               beta1 ~ dnorm(0,0.0000001)
                               beta2 ~ dnorm(0,0.0000001)
                           }

data <- list(mass=mass, age=age, n=n)
inits <- list(beta=rnorm(1), beta2=rnorm(1), tau=10)
fit<-bugs(model.file = model_string,
          data=data,inits=inits,
          parameters.to.save = c("beta1","beta2"),
          n.iter=30000,n.burnin = 10000,n.chains = 2)
fit


