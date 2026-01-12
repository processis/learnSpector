#Spatial modeling of gun-related homicide rates

set.seed(0820)

load("guns.RData")
Y     <- log(10000*Y/N)
Z[,1] <- log(Z[,1])
X     <- cbind(1,Z,rowSums(X))

# Remove AK and HI
Y <- Y[-c(2,11)]
X <- X[-c(2,11),]

n <- length(Y)
p <- ncol(X)

ns_model <- "model{

   # Likelihood
   for(i in 1:n){
      Y[i]   ~ dnorm(mu[i],taue)
      mu[i] <- inprod(X[i,],beta[])
   }
   # Priors
   for(j in 1:p){beta[j] ~ dnorm(0,0.01)}
   taue ~ dgamma(0.1,0.1)
   sig <- 1/sqrt(taue)

 }"

library(rjags)
dat    <- list(Y=Y,n=n,X=X,p=p)
init   <- list(beta=rep(0,p))
model1 <- jags.model(textConnection(ns_model),
                     inits=init,data = dat,quiet=TRUE)
update(model1, 10000, progress.bar="none")
samp1   <- coda.samples(model1, 
                        variable.names=c("beta","sig"), 
                        n.iter=20000, progress.bar="none")
summary(samp1)

library(maps)
library(spdep)
library(maptools)
usa.state = map(database="state", fill=TRUE, plot=FALSE)
state.ID <- sapply(strsplit(usa.state$names, ":"), function(x) x[1])
usa.poly = map2SpatialPolygons(usa.state, IDs=state.ID)
usa.nb = poly2nb(usa.poly)
A = nb2mat(usa.nb, style="B")
A <- A[-8,] # Take out DC
A <- A[,-8]
M <- diag(rowSums(A))

sp_model <- "model{

   # Likelihood
   for(i in 1:n){
      Y[i]  ~ dnorm(mu[i]+S[i],taue)
   }
   S[1:n] ~ dmnorm(zero[1:n],taus*Omega[1:n,1:n])
   for(i in 1:n){
      mu[i]   <- inprod(X[i,],beta[])
      zero[i] <- 0
   }
   Omega[1:n,1:n]<-M[1:n,1:n]-rho*A[1:n,1:n]

   # Priors
   for(j in 1:p){beta[j] ~ dnorm(0,0.01)}
   taue ~ dgamma(0.1,0.1)
   taus ~ dgamma(0.1,0.1)
   rho  ~ dunif(0,1)
   sig[1] <- 1/sqrt(taue)
   sig[2] <- 1/sqrt(taus)
  }"


library(rjags)
dat    <- list(Y=Y,n=n,X=X,A=A,M=M,p=p)
init   <- list(rho=0.99,beta=lm(Y~X-1)$coef)
model2 <- jags.model(textConnection(sp_model),
                     inits=init,data = dat,quiet=TRUE)
update(model2, 10000, progress.bar="none")
samp2  <- coda.samples(model2, 
                       variable.names=c("beta","rho","sig"), 
                       n.iter=20000, progress.bar="none")

summary(samp2)

rho <- samp2[[1]][,8]
hist(rho,breaks=100)

b1  <- samp1[[1]][,7]
b2  <- samp2[[1]][,7]
r   <- c(-0.035,0.015)
d1  <- density(b1,from=r[1],to=r[2])
d2  <- density(b2,from=r[1],to=r[2])

plot(NA,xlim=r,ylim=c(0,max(d1$y)),
     xlab="Beta",ylab="Posterior density")
lines(d1$x,d1$y)
lines(d2$x,d2$y,lty=2)
legend("topright",c("Non-spatial","Spatial"),lty=1:2,bty="n",cex=1.5)

mean(b1<0)

mean(b2<0)