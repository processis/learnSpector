#Metropolis + Gibbs sampling for simulated data

n          <- 100
p          <- 20
X          <- cbind(1,matrix(rnorm(n*(p-1)),n,p-1)) # first column for the intercept
true_beta  <- rnorm(p,0,.5)
prob       <- 1/(1+exp(-X%*%true_beta))
Y          <- rbinom(n,1,prob)

# Create matrix to store the samples

S                 <- 25000
samples           <- matrix(NA,S,p+1)

# Initial values

beta   <- rep(0,p)
sigma  <- 1

# priors: 

a      <- 0.1
b      <- 0.1 

# candidate standard deviation:

can_sd <- 0.1

log_post_beta <- function(Y,X,beta,sigma){
  prob  <- 1/(1+exp(-X%*%beta))
  like  <- sum(dbinom(Y,1,prob,log=TRUE))
  prior <- dnorm(beta[1],0,10,log=TRUE) +        # Intercept
    sum(dnorm(beta[-1],0,sigma,log=TRUE)) # Slopes
  return(like+prior)}

for(s in 1:S){
  
  # Metropolis for beta   
  for(j in 1:p){
    can    <- beta
    can[j] <- rnorm(1,beta[j],can_sd)
    logR   <- log_post_beta(Y,X,can,sigma)-
      log_post_beta(Y,X,beta,sigma)
    if(log(runif(1))<logR){
      beta <- can
    }
  }
  
  # Gibbs for sigma
  sigma <- 1/sqrt(rgamma(1,(p-1)/2+a,sum(beta[-1]^2)/2+b))
  
  samples[s,] <- c(beta,sigma)
}

boxplot(samples[,1:p],outline=FALSE,xlab="Index, j",ylab=expression(beta[j]))
points(true_beta,pch=19,cex=1.25)

