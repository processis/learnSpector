#Gibbs sampling for the concussions data
Y <- c(171, 152, 123, 199)
n <- 4
N <- 256

# Create an empty matrix for the S MCMC samples

S                 <- 25000
samples           <- matrix(NA,S,5)
colnames(samples) <- c("lam1","lam2","lam3","lam4","gamma")

# Initial values

lambda <- log(Y/N)
gamma  <- 1/mean(lambda)

# priors: lambda|gamma ~ Gamma(1,gamma), gamma ~ InvG(a,b)

a      <- 0.1
b      <- 0.1

# Gibbs sampling

for(s in 1:S){
  for(i in 1:n){
    lambda[i] <- rgamma(1,Y[i]+1,N+gamma)
  }
  gamma       <- rgamma(1,a+4,b+sum(lambda)) 
  samples[s,] <- c(lambda,gamma)
}


boxplot(samples[,1:4],outline=FALSE,ylab=expression(lambda),names=2012:2015)

plot(samples[,5],type="l",xlab="Iteration",ylab=expression(gamma))

# Posterior mean, median and 95% credible intervals
round(apply(samples,2,mean),2)

round(apply(samples,2,quantile,c(0.500,0.025,0.975)),2)

# Is the rate higher in 2015 than 2012?
mean(samples[,4]>samples[,1])

# Is the rate higher in 2014 than 2012?
mean(samples[,3]>samples[,1])

# Is the rate higher in 2013 than 2012?
mean(samples[,2]>samples[,1])

# Is the rate higher in 2015 than 2013?
mean(samples[,4]>samples[,2])

# Is the rate higher in 2014 than 2013?
mean(samples[,3]>samples[,2])

# Is the rate higher in 2015 than 2014?
mean(samples[,4]>samples[,3])
