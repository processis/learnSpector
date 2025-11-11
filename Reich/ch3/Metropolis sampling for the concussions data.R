#Metropolis sampling for the concussions data

Y <- c(171, 152, 123, 199)
t <- 1:4
n <- 4
N <- 256

# Create an empty matrix for the MCMC samples

S                 <- 25000
samples           <- matrix(NA,S,2)
colnames(samples) <- c("beta1","beta2")
fitted            <- matrix(0,S,4)

# Initial values

beta   <- c(log(mean(Y/N)),0)

# priors: beta[j] ~ N(0,tau^2)

tau    <- 10

# candidate standard deviations

can_sd <- rep(0.1,2)

log_post <- function(Y,N,t,beta,tau){
  mn    <- N*exp(beta[1]+beta[2]*t)
  like  <- sum(dpois(Y,mn,log=TRUE))
  prior <- sum(dnorm(beta,0,tau,log=TRUE))
  post  <- like + prior
  return(post)}

for(s in 1:S){
  for(j in 1:2){
    can    <- beta
    can[j] <- rnorm(1,beta[j],can_sd[j])
    logR   <- log_post(Y,N,t,can,tau)-log_post(Y,N,t,beta,tau) 
    if(log(runif(1))<logR){
      beta <- can
    }
  }
  samples[s,] <- beta
  fitted[s,]  <- N*exp(beta[1]+beta[2]*t)
}



# Acceptance rates
colMeans(samples[1:24999,]!=samples[2:25000,])

plot(samples[,1],type="l",xlab="Iteration",ylab=expression(beta[1]))

plot(samples[,2],type="l",xlab="Iteration",ylab=expression(beta[2]))

plot(samples[1:200,2],type="b",xlab="Iteration",ylab=expression(beta[2]))

boxplot(fitted,outline=FALSE,ylim=range(Y),
        xlab="Year",ylab="Fitted values",names=2012:2015)
points(Y,pch=19,cex=2)


mean(samples[,2]>0)
