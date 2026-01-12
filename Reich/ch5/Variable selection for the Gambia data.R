#Variable selection for the Gambia data

library(geoR)

data(gambia)

write.csv(gambia,"gambia.csv")

Y <- gambia[,3]
X <- gambia[,4:8]

Y[1:5]

X[1:5,]

names <- c("Age","Netuse","Treated","Green","PCH")
for(j in 1:5){
  boxplot(X[,j]~Y,main=names[j])
}

# Standardize X
X <- scale(X)
X[1:5,]

n <- length(Y)
p <- ncol(X)

library(rjags)

m <- textConnection("model{
     for(i in 1:n){
       Y[i]          ~ dbern(pi[i])
       logit(pi[i]) <- alpha          + X[i,1]*beta[1] + X[i,2]*beta[2] + 
                       X[i,3]*beta[3] + X[i,4]*beta[4] + X[i,5]*beta[5]
     }
     for(j in 1:5){
        beta[j] <- gamma[j]*delta[j]
        gamma[j] ~ dbern(0.5)
        delta[j] ~ dnorm(0,tau)
     }
     alpha ~ dnorm(0,0.01)
     tau   ~ dgamma(0.1,0.1)
   }")

data   <- list(Y=Y,X=X,n=n)
burn   <- 10000
iters  <- 50000
chains <- 3 
model  <- jags.model(m,data = data, n.chains=chains,quiet=TRUE)
update(model, burn, progress.bar="none")
samps  <- coda.samples(model, variable.names=c("beta"), 
                       thin=5, n.iter=iters, progress.bar="none")
plot(samps)

beta    <- NULL
for(l in 1:chains){
  beta <- rbind(beta,samps[[l]])
}
colnames(beta) <- names

for(j in 1:5){
  hist(beta[,j],xlab=expression(beta[j]),ylab="Posterior density",
       breaks=100,main=names[j])
}

Inc_Prob <- apply(beta!=0,2,mean)
Q        <- t(apply(beta,2,quantile,c(0.5,0.05,0.95)))
out      <- cbind(Inc_Prob,Q)

kable(round(out,2))

model <- "Intercept" 
for(j in 1:5){
  model <- paste(model,ifelse(beta[,j]==0,"","+"))
  model <- paste(model,ifelse(beta[,j]==0,"",names[j]))
}
model[1:5]

beta[1:5,]

model_probs <- table(model)/length(model)
model_probs <- sort(model_probs,dec=T)
round(model_probs,3)

