#Multiple linear regression for the HOMES data
#Chapter 4.2: Linear Regression

library(rjags)

# Load data

load("/media/user/娱乐/learnSpector/Bayesian/Krusake/BSMdata/homes.RData")

lat      <- homes[,4]
long     <- homes[,5]
temp     <- homes[,6]
precip   <- homes[,7]
NPP      <- homes[,8]
elev     <- homes[,9]
house    <- ifelse(homes[,10]=="One-family house detached from any other house",1,0)
bedrooms <- as.numeric(homes[,11])



city     <- homes[,2]
state    <- homes[,3]

OTU      <- as.matrix(OTU)
nspecies <- rowSums(OTU>0)
Y        <- log(nspecies)
X        <- cbind(long,lat,temp,precip,NPP,elev,house,bedrooms)
names    <- c("Longitude","Latitude",
              "Temperature","Precipitation","NPP",
              "Elevation","Single-family home",
              "Number of bedrooms")

# Remove observations with missing data

junk     <- is.na(rowSums(X))
Y        <- Y[!junk]
X        <- X[!junk,]
city     <- city[!junk]
state    <- state[!junk]

# Standardize the covariates

X        <- as.matrix(scale(X))

# Plot the sample locations

library(maps)
map("state")
points(homes[,5],homes[,4],pch=19,cex=.5)
title("Sample locations")


n        <- length(Y)
p        <- ncol(X)

data   <- list(Y=Y,X=X,n=n,p=p)
params <- c("beta")

burn     <- 10000
n.iter   <- 20000
thin     <- 10
n.chains <- 2


model_string <- textConnection("model{
   # Likelihood
    for(i in 1:n){
      Y[i] ~ dnorm(alpha+inprod(X[i,],beta[]),taue)
    }
   # Priors
    for(j in 1:p){
      beta[j] ~ dnorm(0,0.001)
    }
    alpha ~ dnorm(0,0.001)
    taue  ~ dgamma(0.1, 0.1)
 }")

model <- jags.model(model_string,data = data, n.chains=n.chains,quiet=TRUE)
update(model, burn, progress.bar="none")
samples1 <- coda.samples(model, variable.names=params, thin=thin, n.iter=n.iter, progress.bar="none")

#add

par(mar=c(2,2,1,1))

plot(samples1)



round(effectiveSize(samples1),1)

sum                      <- summary(samples1)
rownames(sum$statistics) <- names
rownames(sum$quantiles)  <- names
sum$statistics           <- round(sum$statistics,3)
sum$quantiles            <- round(sum$quantiles,3)
sum

model_string <- textConnection("model{
   # Likelihood
    for(i in 1:n){
      Y[i] ~ dnorm(alpha+inprod(X[i,],beta[]),taue)
    }
   # Priors
    for(j in 1:p){
      beta[j] ~ dnorm(0,taue*taub)
    }
    alpha ~ dnorm(0,0.001)
    taue  ~ dgamma(0.1, 0.1)
    taub  ~ dgamma(0.1, 0.1)
 }")

model <- jags.model(model_string,data = data, n.chains=n.chains,quiet=TRUE)
update(model, burn, progress.bar="none")
samples2 <- coda.samples(model, variable.names=params, thin=thin, n.iter=n.iter, progress.bar="none")

plot(samples2)

round(effectiveSize(samples2),1)

sum                      <- summary(samples2)
rownames(sum$statistics) <- names
rownames(sum$quantiles)  <- names
sum$statistics           <- round(sum$statistics,3)
sum$quantiles            <- round(sum$quantiles,3)
sum

model_string <- textConnection("model{
   # Likelihood
    for(i in 1:n){
      Y[i] ~ dnorm(alpha+inprod(X[i,],beta[]),taue)
    }
   # Priors
    for(j in 1:p){
      beta[j] ~ ddexp(0,taue*taub)
    }
    alpha ~ dnorm(0,0.001)
    taue  ~ dgamma(0.1, 0.1)
    taub  ~ dgamma(0.1, 0.1)
 }")

model <- jags.model(model_string,data = data, n.chains=n.chains,quiet=TRUE)
update(model, burn, progress.bar="none")
samples3 <- coda.samples(model, variable.names=params, thin=thin, n.iter=n.iter, progress.bar="none")

plot(samples3)



round(effectiveSize(samples3),1)

sum                      <- summary(samples3)
rownames(sum$statistics) <- names
rownames(sum$quantiles)  <- names
sum$statistics           <- round(sum$statistics,3)
sum$quantiles            <- round(sum$quantiles,3)
sum

for(j in 1:p){
  
  # Collect the MCMC iteration from both chains for the three priors
  
  s1 <- c(samples1[[1]][,j],samples1[[2]][,j])
  s2 <- c(samples2[[1]][,j],samples2[[2]][,j])
  s3 <- c(samples3[[1]][,j],samples3[[2]][,j])
  
  # Get smooth density estimate for each prior
  
  d1 <- density(s1)
  d2 <- density(s2)
  d3 <- density(s3)
  
  # Plot the density estimates
  
  mx <- max(c(d1$y,d2$y,d3$y))
  
  plot(d1$x,d1$y,type="l",ylim=c(0,mx),xlab=expression(beta),ylab="Posterior density",main=names[j])
  lines(d2$x,d2$y,lty=2)
  lines(d3$x,d3$y,lty=3)
  abline(v=0)
}

##Multiple linear regression prediction

# Load the data

load("/media/user/娱乐/learnSpector/Bayesian/Krusake/BSMdata/election_2008_2016.RData")

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

# NOTE: Yp is not sent to JAGS!
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
}


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

##Beta regression for the microbiome data
#Chapter 4.3.3: Generalized linear models

set.seed(0820)

load("/media/user/娱乐/learnSpector/Bayesian/Krusake/BSMdata/homes.RData") 
ls()

city     <- homes[,2]
state    <- homes[,3]
lat      <- homes[,4]
long     <- homes[,5]
temp     <- homes[,6]
precip   <- homes[,7]
NPP      <- homes[,8]
elev     <- homes[,9]
house    <- ifelse(homes[,10]=="One-family house detached from any other house",1,0)
bedrooms <- as.numeric(homes[,11])


OTU      <- as.matrix(OTU)
Y        <- apply(OTU,1,max)/rowSums(OTU)
X        <- cbind(long,lat,temp,precip,NPP,elev,house,bedrooms)
names    <- c("Intercept","Longitude","Latitude",
              "Temperature","Precipitation","NPP",
              "Elevation","Single-family home",
              "Number of bedrooms")

# Remove observations with missing values
junk        <- is.na(rowSums(X))
Y           <- Y[!junk]
X           <- X[!junk,]
city        <- city[!junk]
state       <- state[!junk]

# Standardize the covariates
X           <- as.matrix(scale(X))

X           <- cbind(1,X) # add the intercept
colnames(X) <- names
n           <- length(Y)
p           <- ncol(X)


hist(Y,breaks=50,ylab="Maximum proportion")

for(j in 2:p){
  plot(X[,j],Y,xlab=names[j],ylab="Maximum proportion")
}


library(rjags)

data   <- list(Y=Y,X=X,n=n,p=p)
params <- c("beta","r")

model_string <- textConnection("model{
    for(i in 1:n){
      Y[i]       ~ dbeta(r*mu[i],r*(1-mu[i]))
      logit(mu[i]) <- inprod(X[i,],beta[])
    }
    for(j in 1:p){beta[j] ~ dnorm(0,0.01)}
    r ~ dgamma(0.1,0.1)
 }")

model <- jags.model(model_string,data = data, n.chains=2,quiet=TRUE)
update(model, 10000, progress.bar="none")
samples <- coda.samples(model, variable.names=params, thin=5, n.iter=20000, progress.bar="none")

plot(samples)




sum <- summary(samples)

rownames(sum$statistics) <- c(names,"r")
rownames(sum$quantiles)  <- c(names,"r")
sum$statistics           <- round(sum$statistics,3)
sum$quantiles            <- round(sum$quantiles,3)
sum



#One-way random effects model for the jaw data
#Chapter 4.4: Random effects


library(rjags)

m   <- 4
n   <- 20
age <- c(8.0, 8.5, 9.0, 9.5)
Y   <- c(47.8, 48.8, 49.0, 49.7,
         46.4, 47.3, 47.7, 48.4,
         46.3, 46.8, 47.8, 48.5,
         45.1, 45.3, 46.1, 47.2,
         47.6, 48.5, 48.9, 49.3,
         52.5, 53.2, 53.3, 53.7,
         51.2, 53.0, 54.3, 54.5,
         49.8, 50.0, 50.3, 52.7,
         48.1, 50.8, 52.3, 54.4,
         45.0, 47.0, 47.3, 48.3,
         51.2, 51.4, 51.6, 51.9,
         48.5, 49.2, 53.0, 55.5,
         52.1, 52.8, 53.7, 55.0,
         48.2, 48.9, 49.3, 49.8,
         49.6, 50.4, 51.2, 51.8,  
         50.7, 51.7, 52.7, 53.3,
         47.2, 47.7, 48.4, 49.5,
         53.3, 54.6, 55.1, 55.3,
         46.2, 47.5, 48.1, 48.4,
         46.3, 47.6, 51.3, 51.8) 

Y <- matrix(Y,20,4,byrow=TRUE)

plot(row(Y),Y,xlab="Patient",ylab="Bone density",pch=19)
lines(rowMeans(Y))
legend("topleft",c("Observations","Sample mean"),lty=c(NA,1),pch=c(19,NA),bty="n")

data     <- list(Y=Y,n=n,m=m)
burn     <- 10000
n.iter   <- 20000
thin     <- 20
n.chains <- 2

model_string <- textConnection("model{

   # Likelihood
    for(i in 1:n){for(j in 1:m){
      Y[i,j] ~ dnorm(alpha[i],taue)
    }}

   # Random effects
    for(i in 1:n){alpha[i] ~ dnorm(mu,taua)}

   # Priors
    mu   ~ dnorm(0,0.0001)
    taue ~ dgamma(0.1,0.1)
    taua ~ dgamma(0.1,0.1)
 }")

params   <- c("mu","alpha","taue","taua")
model    <- jags.model(model_string, data = data, 
                       n.chains=n.chains, quiet=TRUE)
update(model, burn, progress.bar="none")
samples1 <- coda.samples(model, variable.names=params, thin=thin,
                         n.iter=n.iter, progress.bar="none")

samples1 <- rbind(samples1[[1]],samples1[[2]])
alpha    <- samples1[,1:n]
mu       <- samples1[,n+1]
sigma2   <- 1/samples1[,n+2:3]
r        <- sigma2[,1]/rowSums(sigma2)
hist(r,breaks=50,prob=TRUE,main="",xlab="Proportion of variance explained by the random effect")

boxplot(alpha~col(alpha),ylim=range(Y),xlab="Patient",ylab="Bone density",outline=FALSE)
points(row(Y),Y,pch=19)

model_string_HC <- textConnection("model{

   # Likelihood
    for(i in 1:n){for(j in 1:m){
      Y[i,j] ~ dnorm(alpha[i],taue)
    }}

   # Random effects
    for(i in 1:n){alpha[i] ~ dnorm(mu,taua)}

   # Priors
    mu      ~ dnorm(0,0.0001)
   taue    <- pow(sigma1,-2)

   taua    <- pow(sigma2,-2)

   sigma1   ~ dt(0, 1, 1)T(0,)
   sigma2   ~ dt(0, 1, 1)T(0,)

 }")

model    <- jags.model(model_string_HC, data = data, 
                       n.chains=n.chains, quiet=TRUE)
update(model, burn, progress.bar="none")
samplesHC <- coda.samples(model, variable.names=params, thin=thin,
                          n.iter=n.iter, progress.bar="none")

samplesHC <- rbind(samplesHC[[1]],samplesHC[[2]])
sigma2HC <- 1/samplesHC[,n+2:3]

apply(sqrt(sigma2),2,quantile,c(0.5,0.025,0.975))   # InvGamma prior

apply(sqrt(sigma2HC),2,quantile,c(0.5,0.025,0.975)) # Half-Cauchy prior

plot(density(sigma2[,1]),xlab="Sigma",ylab="Posterior",main="Error SD")
lines(density(sigma2HC[,1]),col=2)
legend("topright",c("InvGamma","Half-Cauchy"),lty=1,col=1:2,bty="n")

plot(density(sigma2[,2]),xlab="Sigma",ylab="Posterior",main="Random effect SD")
lines(density(sigma2HC[,2]),col=2)
legend("topright",c("InvGamma","Half-Cauchy"),lty=1,col=1:2,bty="n")

model_string0 <- textConnection("model{
   # Likelihood
    for(i in 1:n){for(j in 1:m){
      Y[i,j] ~ dnorm(mu,taue)
    }}

   # Priors
    mu ~ dnorm(0,0.0001)
    taue ~ dgamma(0.1,0.1)
 }")

model0   <- jags.model(model_string0,data = data, 
                       n.chains=2, quiet=TRUE)
update(model0, burn, progress.bar="none")
samples0 <- coda.samples(model0, variable.names=c("mu"),
                         n.iter=n.iter, thin=thin, progress.bar="none")
mu_naive <- c(samples0[[1]],samples0[[2]])

d1 <- density(mu,from=47,to=52)
d0 <- density(mu_naive,from=47,to=52)
quantile(mu,c(0.025,0.975))

quantile(mu_naive,c(0.025,0.975))

var(mu)/var(mu_naive)

plot(d0,type="l",lty=2,xlab=expression(mu),ylab="Posterior density",main="")
lines(d1,lty=1)

legend("topleft",c("Random effects","IID"),lty=1:2,bty="n",cex=1.25)

#Linear mixed model for the jaw data
#Chapter 4.4: Random effects

library(rjags)

m   <- 4
n   <- 20
age <- c(8.0, 8.5, 9.0, 9.5)
Y   <- c(47.8, 48.8, 49.0, 49.7,
         46.4, 47.3, 47.7, 48.4,
         46.3, 46.8, 47.8, 48.5,
         45.1, 45.3, 46.1, 47.2,
         47.6, 48.5, 48.9, 49.3,
         52.5, 53.2, 53.3, 53.7,
         51.2, 53.0, 54.3, 54.5,
         49.8, 50.0, 50.3, 52.7,
         48.1, 50.8, 52.3, 54.4,
         45.0, 47.0, 47.3, 48.3,
         51.2, 51.4, 51.6, 51.9,
         48.5, 49.2, 53.0, 55.5,
         52.1, 52.8, 53.7, 55.0,
         48.2, 48.9, 49.3, 49.8,
         49.6, 50.4, 51.2, 51.8,  
         50.7, 51.7, 52.7, 53.3,
         47.2, 47.7, 48.4, 49.5,
         53.3, 54.6, 55.1, 55.3,
         46.2, 47.5, 48.1, 48.4,
         46.3, 47.6, 51.3, 51.8) 

Y <- matrix(Y,20,4,byrow=TRUE)

plot(NA,xlim=range(age),ylim=range(Y),xlab="Age",ylab="Bone density")
for(i in 1:n){
  lines(age,Y[i,])
  points(age,Y[i,],pch=19)
}

data     <- list(Y=Y,age=age,n=n,m=m)
burn     <- 10000
n.iter   <- 20000
thin     <- 10
n.chains <- 2

model_string <- textConnection("model{
   # Likelihood
    for(i in 1:n){for(j in 1:m){
      Y[i,j] ~ dnorm(alpha[i,1]+alpha[i,2]*age[j],taue)
    }}

   # Random effects
    for(i in 1:n){alpha[i,1:2] ~ dmnorm(mu[1:2],Omega[1:2,1:2])}

   # Priors
    for(j in 1:2){mu[j] ~ dnorm(0,0.0001)}
    taue ~ dgamma(0.1,0.1)
    Omega[1:2,1:2] ~ dwish(R[,],2.1)

    R[1,1]<-1/2.1
    R[1,2]<-0
    R[2,1]<-0
    R[2,2]<-1/2.1
 }")

params  <- c("mu","alpha","taue","Omega")
model   <- jags.model(model_string,data = data, n.chains=n.chains,quiet=TRUE)
update(model, burn, progress.bar="none")
samples <- coda.samples(model, variable.names=params,
                        n.iter=n.iter, thin=thin, progress.bar="none")
samples <- rbind(samples[[1]],samples[[2]])
Omega   <- samples[,1:4]
a1      <- samples[,5:24]
a2      <- samples[,25:44]
mu      <- samples[,45:46]
sig     <- 1/sqrt(samples[,47])
S       <- Omega
for(i in 1:nrow(S)){
  S[i,]<-as.vector(solve(matrix(Omega[i,],2,2)))
} 

r <- S[,2]/sqrt(S[,1]*S[,4])
hist(r,breaks=50,prob=TRUE,main="",xlab="Correlation between random slopes and intercepts")

these  <- c(1,11,12) # pick three subjects
na     <- 10
ages   <- seq(8,10,length=na) # Estimate the line for these ages

plot(NA,xlim=range(ages),ylim=c(45,60),xlab="Age",ylab="Bone density")
for(sub in 1:length(these)){
  
  # Plot the posterior of the mean alpha1+age[j]*alpha2
  
  i   <- these[sub]
  fit <- NULL
  for(j in 1:na){fit <- cbind(fit,a1[,i]+ages[j]*a2[,i])}
  q  <- apply(fit,2,quantile,c(0.025,0.5,0.975))
  points(age,Y[i,],pch=sub)
  lines(ages,q[1,],lty=2)
  lines(ages,q[2,],lty=1)
  lines(ages,q[3,],lty=2)
  
  # Plot the posterior predictive distribution at age 10
  
  Y10 <- a1[,i]+a2[,i]*10+rnorm(length(sig),0,sig)
  q   <- quantile(Y10,c(0.025,0.975))
  lines(c(10,10),q,lty=sub)
  lines(10+0.05*c(-1,1),rep(q[1],2),lty=sub)
  lines(10+0.05*c(-1,1),rep(q[2],2),lty=sub)
}

legend("topleft",paste("Patient",1:3),pch=1:3,cex=1.5,bty="n")


