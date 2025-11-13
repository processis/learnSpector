#Multiple linear regression prediction

# Load the data

load("election_2008_2016.RData")

junk <- is.na(Y+rowSums(X))
Y    <- Y[!junk]
X    <- X[!junk,]
n    <- length(Y)
p    <- ncol(X)

n

p

X    <- scale(X)

# Fit the model to a training set of size 100 and make prediction for the remaining observations

set.seed(0820)
test  <- order(runif(n))>100
table(test)


Yo    <- Y[!test]    # Observed data
Xo    <- X[!test,]

Yp    <- Y[test]     # Counties set aside for prediction
Xp    <- X[test,]

no    <- length(Yo)
np    <- length(Yp)
p     <- ncol(Xo)

library(rjags)


model_string <- "model{

  # Likelihood
  for(i in 1:no){
    Yo[i]   ~ dnorm(muo[i],inv.var)
    muo[i] <- alpha + inprod(Xo[i,],beta[])
  }

  # Prediction
  for(i in 1:np){
    Yp[i]  ~ dnorm(mup[i],inv.var)
    mup[i] <- alpha + inprod(Xp[i,],beta[])
  }

  # Priors
  for(j in 1:p){
    beta[j] ~ dnorm(0,0.0001)
  }
  alpha     ~ dnorm(0, 0.01)
  inv.var   ~ dgamma(0.01, 0.01)
  sigma     <- 1/sqrt(inv.var)
}"


model <- jags.model(textConnection(model_string), 
                    data = list(Yo=Yo,no=no,np=np,p=p,Xo=Xo,Xp=Xp))

update(model, 10000, progress.bar="none")

samp <- coda.samples(model, 
                     variable.names=c("beta","sigma","Yp","alpha"), 
                     n.iter=20000, progress.bar="none")

summary(samp[,-c(1:np)])

#Extract the samples for each parameter

samps       <- samp[[1]]
Yp.samps    <- samps[,1:np] 
alpha.samps <- samps[,np+1]
beta.samps  <- samps[,np+1+1:p]
sigma.samps <- samps[,ncol(samps)]

# Compute the posterior mean for the plug-in predictions  

beta.mn  <- colMeans(beta.samps)
sigma.mn <- mean(sigma.samps)
alpha.mn <- mean(alpha.samps) 


# Plot the PPD and plug-in

for(j in 1:5){
  
  # Plug-in
  mu <- alpha.mn+sum(Xp[j,]*beta.mn)
  y  <- rnorm(20000,mu,sigma.mn)
  plot(density(y),col=2,xlab="Y",main="PPD")
  
  # PPD
  lines(density(Yp.samps[,j]))
  
  # Truth
  abline(v=Yp[j],col=3,lwd=2)
  
  legend("topright",c("PPD","Plug-in","Truth"),col=1:3,lty=1,inset=0.05)
  
  # plug-in 95% intervals
  low1   <- alpha.mn+Xp%*%beta.mn - 1.96*sigma.mn
  high1  <- alpha.mn+Xp%*%beta.mn + 1.96*sigma.mn
  cover1 <- mean(Yp>low1 & Yp<high1)
  mean(cover1)
  
  # PPD 95% intervals
  low2   <- apply(Yp.samps,2,quantile,0.025)
  high2  <- apply(Yp.samps,2,quantile,0.975)
  cover2 <- mean(Yp>low2 & Yp<high2)
  mean(cover2)
}