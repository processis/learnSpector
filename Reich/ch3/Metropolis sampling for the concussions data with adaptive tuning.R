#Metropolis sampling for the concussions data with adaptive tuning

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

# Initial candidate standard deviations

can_sd <- c(1,1)

log_post <- function(Y,N,t,beta,tau){
  mn    <- N*exp(beta[1]+beta[2]*t)
  like  <- sum(dpois(Y,mn,log=TRUE))
  prior <- sum(dnorm(beta,0,tau,log=TRUE))
  post  <- like + prior
  return(post)}

burn  <- 5000      # Length of burn-in period for tuning
check <- 100       # Iterations between checks of the acceptance rate     
att   <- rep(0,2)  # Keep track of the number of MH attempts
acc   <- rep(0,2)  # Keep track of the number of MH accepts

for(s in 1:S){
  for(j in 1:2){
    att[j] <- att[j] + 1  
    can    <- beta
    can[j] <- rnorm(1,beta[j],can_sd[j])
    logR   <- log_post(Y,N,t,can,tau)-log_post(Y,N,t,beta,tau) 
    if(log(runif(1))<logR){
      beta   <- can
      acc[j] <- acc[j] + 1  
    }
  }
  
  # TUNING!
  for(j in 1:length(att)){
    if(s<burn & att[j]==check){
      print(paste0("Can sd of ", round(can_sd[j],3),
                   " for beta[",j,"] gave acc rate ",acc[j]/att[j])) 
      if(acc[j]/att[j]<0.2){can_sd[j]<-can_sd[j]*0.8}
      if(acc[j]/att[j]>0.6){can_sd[j]<-can_sd[j]*1.2}
      acc[j] <- att[j] <- 0  
    }
  }
  
  samples[s,] <- beta
  fitted[s,]  <- N*exp(beta[1]+beta[2]*t)
}

colMeans(samples[burn:S,]!=samples[burn:S - 1,])

plot(samples[,1],type="l",xlab="Iteration",ylab=expression(beta[1]))

plot(samples[,2],type="l",xlab="Iteration",ylab=expression(beta[2]))

plot(1:200,samples[1:200,2],type="b",xlab="Iteration",ylab=expression(beta[2]))

plot(1:200+500,samples[1:200+500,2],type="b",xlab="Iteration",ylab=expression(beta[2]))


plot(1:200+1000,samples[1:200+1000,2],type="b",xlab="Iteration",ylab=expression(beta[2]))

plot(1:200+5000,samples[1:200+5000,2],type="b",xlab="Iteration",ylab=expression(beta[2]))

                                                           