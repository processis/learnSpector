#Logistic regression for NBA clutch free throws

set.seed(0820)

Y <- c(64, 72, 55, 27, 75, 24, 28, 66, 40, 13)
N <- c(75, 95, 63, 39, 83, 26, 41, 82, 54, 16)
q <- c(0.845, 0.847, 0.880, 0.674, 0.909, 0.899, 0.770, 0.801, 0.802, 0.875)

X <- log(q)-log(1-q) # X = logit(q)

inits   <- c("RW","JH","KL","LJ","SC","IT","GA","JW","AD","KD")
plot(100*q,100*Y/N,
     xlim=100*c(0.65,0.95),ylim=100*c(0.65,0.95),
     xlab="Overall percentage",ylab="Clutch percentage")
text(100*q,100*Y/N+1,inits)
abline(0,1)


library(rjags)

data   <- list(Y=Y,N=N,X=X)
params <- c("beta")

model_string <- textConnection("model{
   # Likelihood
    for(i in 1:10){
      Y[i]        ~ dbinom(p[i],N[i])
      logit(p[i]) <- beta[1] + beta[2]*X[i]
    }
   # Priors
    beta[1] ~ dnorm(0,0.01)
    beta[2] ~ dnorm(0,0.01)
 }")

model <- jags.model(model_string,data = data, n.chains=2,quiet=TRUE)
update(model, 10000, progress.bar="none")
samples1 <- coda.samples(model, variable.names=params, thin=5, n.iter=20000, progress.bar="none")

plot(samples1)

summary(samples1)

b1 <- c(samples1[[1]][,1],samples1[[2]][,1])
b2 <- c(samples1[[1]][,2],samples1[[2]][,2])

model_string <- textConnection("model{
   # Likelihood
    for(i in 1:10){
      Y[i]        ~ dbinom(p[i],N[i])
      logit(p[i]) <- beta + X[i]
    }
   # Priors
    beta ~ dnorm(0,0.01)
 }")

model <- jags.model(model_string,data = data, n.chains=2,quiet=TRUE)
update(model, 10000, progress.bar="none")
samples2 <- coda.samples(model, variable.names=params, thin=5, n.iter=20000, progress.bar="none")
b3 <- c(samples2[[1]],samples2[[2]])

plot(samples2)

summary(samples2)

mean(b3<0) # Prob(beta_3<0|Y)

d1 <- density(b1,from=-1,to=2)
d2 <- density(b2,from=-1,to=2)
d3 <- density(b3,from=-1,to=2)

mx <- max(c(d1$y,d2$y,d3$y))

plot(d3$x,d3$y,type="l",xlim=c(-1,2),ylim=c(0,mx),xlab=expression(beta),ylab="Posterior density")
lines(d1$x,d1$y,lty=2)
lines(d2$x,d2$y,lty=3)

legend("topright",c("Model 1 - Intercept","Model 1 - Slope","Model 2 - Intercept"),
       bty="n",lty=c(2,3,1),cex=1.25)