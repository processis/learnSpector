#Gibbs sampling for a one sample t-test

library(MASS)
Y <- galaxies
n <- length(Y)
hist(Y,breaks=25)

m <- 0.01
a <- 0.01
b <- 0.01

n.iters <- 30000
keep.mu <- rep(0,n.iters)
keep.s2 <- rep(0,n.iters)

# Initial values
mu         <- mean(Y)
s2         <- var(Y)
keep.mu[1] <- mu
keep.s2[1] <- s2

for(iter in 2:n.iters){
  
  # sample mu|s2,Y
  
  MN  <- sum(Y)/(n+m)
  VR  <- s2/(n+m) 
  mu <- rnorm(1,MN,sqrt(VR))
  
  # sample s2|mu,Y
  
  A  <- a + n/2
  B  <- b + sum((Y-mu)^2)/2
  s2 <- 1/rgamma(1,A,B)
  
  # keep track of the results
  keep.mu[iter] <- mu
  keep.s2[iter] <- s2
  
  # Plot the samples every 10000 iterations
  if(iter%%10000==0){
    par(mfrow=c(1,2))
    plot(keep.mu[1:iter],type="l",ylab="mu")
    plot(keep.s2[1:iter],type="l",ylab="s2")
  }
}

plot(keep.s2,keep.mu,xlab="Sigma^2",ylab="mu",main="Joint posterior")
abline(mean(Y),0)
abline(v=var(Y))

hist(keep.mu,xlab="mu",main="Marginal posterior")

keep.s <- sqrt(keep.s2)
hist(keep.s,xlab="sigma",main="Marginal posterior")

mean(keep.mu)
quantile(keep.mu,c(0.025,0.975))
mean(keep.s2)
quantile(keep.s2,c(0.025,0.975))
mean(keep.s)
quantile(keep.s,c(0.025,0.975))


mu_hat  <- mean(keep.mu)
sig_hat <- mean(keep.s)
h       <- hist(Y,breaks=25)

y       <- seq(4000,40000,100)
d       <- dnorm(y,mu_hat,sig_hat)
d       <- max(h$count)*d/max(d)
lines(y,d,lwd=2,col=2)







