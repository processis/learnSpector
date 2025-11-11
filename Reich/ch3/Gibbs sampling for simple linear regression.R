#Gibbs sampling for simple linear regression
### Load data and fit least squares
library(babynames)
dat <- babynames
dat <- dat[dat$name=="Sophia" & dat$sex=="F" & dat$year>1950,]
dat

yr  <- dat$year
p   <- dat$prop

X   <- dat$year - 1980
Y   <- log(p/(1-p))
n   <- length(X)

plot(yr,p,xlab="Year",ylab="Proportion Sophia")

OLS <- lm(Y~X)
summary(OLS)

plot(yr,Y,xlab="Year",ylab="Log odds Sophia")
OLS$coef

y_hat <- OLS$coef[1]+OLS$coef[2]*X
lines(yr,y_hat)

# Plot fitted values on the proportion scale
plot(yr,p,xlab="Year",ylab="Proportion Sophia")
p_hat <- exp(y_hat)/(1+exp(y_hat))
lines(yr,p_hat)

### Priors

mu0 <- 0
s20 <- 1000
a   <- 0.01
b   <- 0.01

n.iters <- 30000
keepers <- matrix(0,n.iters,3)
colnames(keepers)<-c("alpha","beta","sigma2")

# Initial values
alpha       <- OLS$coef[1]
beta        <- OLS$coef[2]
s2          <- var(OLS$residuals)
keepers[1,] <- c(alpha,beta,s2)

for(iter in 2:n.iters){
  
  # sample alpha
  
  V     <- n/s2+mu0/s20
  M     <- sum(Y-X*beta)/s2+1/s20
  alpha <- rnorm(1,M/V,1/sqrt(V))
  
  # sample beta
  
  V     <- sum(X^2)/s2+mu0/s20
  M     <- sum(X*(Y-alpha))/s2+1/s20
  beta  <- rnorm(1,M/V,1/sqrt(V))
  
  # sample s2|mu,Y,Z
  
  A  <- n/2 + a
  B  <- sum((Y-alpha-X*beta)^2)/2 + b
  s2 <- 1/rgamma(1,A,B)
  
  # keep track of the results
  keepers[iter,] <- c(alpha,beta,s2)
  
}

pairs(keepers)

output <- matrix(0,3,4)
rownames(output) <- c("Intercept","Slope","sigma2")
colnames(output) <- c("Mean","SD","Q025","Q975")

output[,1] <- apply(keepers,2,mean)
output[,2] <- apply(keepers,2,sd)
output[,3] <- apply(keepers,2,quantile,0.025)
output[,4] <- apply(keepers,2,quantile,0.975)

kable(output,digits=3)

beta <- keepers[,2]
hist(beta,main="Posterior of the slope, beta",breaks=100)

fit_bayes <- output[1:2,1]
plot(yr,Y,xlab="Year",ylab="Log odds Sophia")
lines(yr,fit_bayes[1]+fit_bayes[2]*X)

