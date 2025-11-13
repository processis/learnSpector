#Beta regression for the microbiome data

set.seed(0820)

load("homes.RData") 
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

