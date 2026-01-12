# Step-by-step illustration of Metropolis sampling for a 1D posterior


# Function to compute the posterior distribution:
# Likelihood: Y[i] ~ Bern(expit(theta))
# Prior:      theta ~ Normal(pri_mn,pri_sd)
post <- function(theta,Y,pri_mn=0,pri_sd=1){
  prob  <- exp(theta)/(1+exp(theta))
  prior <- dnorm(theta,pri_mn,pri_sd)
  like  <- prod(dbinom(Y,1,prob))
  return(like*prior)}


# Generate fake data

set.seed(0820)
Y<-rbinom(20,1,0.7)

# Compute the posterior on a grid for reference

theta_grid <- seq(-3,3,length=100)
dense      <- rep(0,100)
for(i in 1:100){
  dense[i]<-post(theta_grid[i],Y)
}


# MCMC set-up

n.iters <- 10000
can_sd  <- 0.5 # Try different values of can_sd to see the effect on the acceptance rate


#initial value

theta      <- 0
keep_theta <- rep(0,n.iters)

# Go!

par(ask=TRUE,mfrow=c(1,1))
for(iter in 1:n.iters){
  
  # Draw a candidate and compute acceptance ratio:
  
  can <- rnorm(1,theta,can_sd)
  p1  <- post(can,Y)
  p2  <- post(theta,Y)
  R   <- p1/p2
  R   <- ifelse(R>1,1,R)
  
  # Plot the candidate:
  
  if(iter<50){
    
    plot(theta_grid,dense,type="l",lwd=2,
         xlab=expression(theta),ylab=expression(paste("f",theta,"|Y)")))
    lines(c(theta,theta),c(0,p2),col=2,lwd=2)
    lines(rep(can,2),c(0,p1),col=3,lwd=2)
    
    leg    <- NULL
    leg[1] <- paste("R = ",round(R,2))
    leg[2] <- paste("Old value = ",round(theta,2))
    leg[3] <- paste("Candidate = ",round(can,2))
    
    legend("topleft",leg,lty=1,col=1:3,inset=0.05)
  }
  
  # Make a decision: 
  
  keep     <- rbinom(1,1,R)==1
  if(keep){
    theta<-can
  }
  keep_theta[iter]<-theta
}

# Plot the results:

par(ask=FALSE,mfrow=c(2,2))
plot(keep_theta,type="l",xlab="MCMC Iteration",ylab=expression(theta),main="Trace plot")
acf(keep_theta)
hist(keep_theta,breaks=50,main="Posterior of theta")
keep_p <- exp(keep_theta)/(1+exp(keep_theta))
hist(keep_p,breaks=50,main="Posterior of p")