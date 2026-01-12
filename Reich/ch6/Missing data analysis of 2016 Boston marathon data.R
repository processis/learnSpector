#Missing data analysis of 2016 Boston marathon data
load("BostonMarathon2016.RData")

Y  <- Marathon_female$SPEED_mile[,26]
X  <- Marathon_female$SPEED_mile[,-26]
X  <- scale(X)
n  <- length(Y)
p  <- ncol(X)

image(1:p,1:n,is.na(t(X)),col=0:1,
      xlab="Mile",ylab="Runner",
      main="Missing data pattern")
for(j in 0:p){
  abline(v=j+0.5,col=gray(0.5))
}
for(j in 0:n){
  abline(j+0.5,0,col=gray(0.5))
}

matplot(t(X),type="l",lty=1,xlab="Mile",ylab="Standardized speed")

library(rjags)

model_string <- textConnection("model{

   # Likelihood
    for(i in 1:n){
      Y[i] ~ dnorm(alpha + inprod(X[i,],beta[]),taue)
    }

   # Missing data model
    for(i in 1:n){
      X[i,1]  ~ dnorm(0,tau1)
      for(j in 2:p){
        X[i,j] ~ dnorm(rho*X[i,j-1],tau2)
      }
    }

   # Priors
    alpha ~ dnorm(0,0.01)
    for(j in 1:p){
      beta[j] ~ dnorm(0,0.01)
    }
    taue  ~  dgamma(0.1, 0.1)
    tau1  ~  dgamma(0.1, 0.1)
    tau2  ~  dgamma(0.1, 0.1)
    rho   ~  dnorm(0, 0.01)
 }")


inits <- list(rho=rnorm(1))
data  <- list(Y=Y,X=X,n=n,p=p)
model <- jags.model(model_string,data = data, quiet=TRUE,inits=inits, n.chains=2)

update(model, 10000, progress.bar="none")
params  <- c("alpha","beta","X[3,6]","X[3,7]",
             "X[149,19]","X[149,20]","X[149,21]","rho")
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=20000, progress.bar="none")

plot(samples)

X_miss <- rbind(samples[[1]],samples[[2]])[,1:5]
r      <- range(na.omit(as.vector(X)))

# Runner 3
plot(X[3,],ylim=r,
     xlab="Mile",ylab="Speed (minute/mile)",
     main="Imputed X for two runners")
boxplot(X_miss[,4],at=6,outline=FALSE,add=TRUE)
boxplot(X_miss[,5],at=7,outline=FALSE,add=TRUE)

# Runner 149
points(X[149,],pch=19)
boxplot(X_miss[,1],at=19,col=1,outline=FALSE,add=TRUE)
boxplot(X_miss[,2],at=20,col=1,outline=FALSE,add=TRUE)
boxplot(X_miss[,3],at=21,col=1,outline=FALSE,add=TRUE)

legend("topleft",c("Runner 3","Runner 149"),pch=c(1,19),bty="n",cex=1.5)

beta <- rbind(samples[[1]],samples[[2]])[,1:p+6]

boxplot(beta,xlab="Mile",ylab="Posterior",
        main="Posterior of beta",
        ylim=1.5*c(-1,1),outline=FALSE,axes=FALSE)
axis(1)
axis(2)

abline(0,0)

