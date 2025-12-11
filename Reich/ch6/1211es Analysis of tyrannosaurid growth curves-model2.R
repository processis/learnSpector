dat         <- read.csv("!Erickson.csv")
dat[15:16,] <- dat[16:15,] #order by age 
dat

taxon       <- dat[,1]
age         <- dat[,3]
mass        <- dat[,4]
taxon1      <- dat[,5]
# Plot the data

plot(NA,xlim=range(age),ylim=range(mass),
     main="", xlab="Age (years)", ylab="Body Mass (kg)") 
points(age[taxon=="Albertosaurus"], mass[taxon=="Albertosaurus"], pch=1,cex=1.5)
points(age[taxon=="Daspletosaurus"],mass[taxon=="Daspletosaurus"],pch=2,cex=1.5)
points(age[taxon=="Gorgosaurus"],   mass[taxon=="Gorgosaurus"],   pch=3,cex=1.5)
points(age[taxon=="Tyrannosaurus"], mass[taxon=="Tyrannosaurus"], pch=4,cex=1.5)

# Fitted growth curves (from the original paper)

x <- seq(0,30,.1)
lines(x,1218/(1+exp(-0.43*(x-14.1)))+5, lty=1)  
lines(x,1728/(1+exp(-0.44*(x-12.1)))+5, lty=2)  
lines(x,1234/(1+exp(-0.38*(x-12.4)))+5, lty=3) 
lines(x,5551/(1+exp(-0.57*(x-16.1)))+5, lty=4) 

legend("topleft",c("Albertosaurus","Daspletosaurus","Gorgosaurus","Tyrannosaurus"), 
       lty=1:4, pch=1:4,bty="n",cex=1.5)


library(rjags)
set.seed(0820)

y       <- log(mass)
x       <- log(age)
n       <- length(y)
sp      <- as.numeric(taxon1)
names   <- c("Albertosaurus","Daspletosaurus","Gorgosaurus","Tyrannosaurus")
data    <- list(y=y,x=x,sp=sp,n=n,N=4)
data



burn    <- 10000
iters   <- 100000
thin    <- 10


plot(x,y,pch=sp,cex=1.5,xlab="Log Age (Years)",ylab="Log Body Mass (kg)")
for(j in 1:4){
  b <- lm(y[sp==j]~x[sp==j])
  abline(b[1],b[2],lty=j)
}
legend("topleft",names,lty=1:4, pch=1:4,bty="n",cex=1.5)



model_string2 <- textConnection("model{
  for(i in 1:n){
    y[i]    ~ dnorm(muY[i],tau[1])
    muY[i] <- a[sp[i]] + b[sp[i]]*x[i]  - 0.5/tau[1]
  }

  for(j in 1:N){
    a[j]   ~ dnorm(mu[1],tau[2])
    b[j]   ~ dnorm(mu[2],tau[3])
  }

  for(k in 1:2){
    mu[k]  ~ dnorm(0.0,0.1)
  }
  for(k in 1:3){
    tau[k]  ~ dgamma(0.1,0.1)
  }

  for(age in 1:30){for(j in 1:4){
    fitted[age,j] <- exp(a[j] + b[j]*log(age))
  }}
}")

model2   <- jags.model(model_string2,data = data,quiet=TRUE, n.chains=2)
update(model2, burn, progress.bar="none")
samples2 <- coda.samples(model2, variable.names=c("a","b","fitted","tau"), n.iter=iters, thin=thin, progress.bar="none")

ESS <- effectiveSize(samples2) 
ESS[which.min(ESS)]

ESS[which.max(ESS)]

fit2 <- summary(samples2)$quantiles[1:120+8,3]
lo2  <- summary(samples2)$quantiles[1:120+8,1]
hi2  <- summary(samples2)$quantiles[1:120+8,5]
id   <- rep(1:4,each=30)

par(mfrow=c(2,2))
for(j in 1:4){
  plot(NA,xlim=range(age),ylim=range(mass),
       xlab="Age (years)", ylab="Body Mass (kg)",
       main=names[j]) 
  points(age[sp==j],mass[sp==j],pch=19,cex=1.5)
  lines(1:30,fit2[id==j],lty=1)
  lines(1:30,lo2[id==j],lty=2)
  lines(1:30,hi2[id==j],lty=2)
  
  if(j==1){
    legend("topleft",c("Data","Post mean","95% interval"),
           pch=c(19,NA,NA),lty=c(NA,1,2),cex=1.5,bty="n")
  }
}
