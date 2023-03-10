graphics.off()
rm(list=ls(all=TRUE))
library(BRugs)

modelString=
  model{
    for(t in 1:nTrialTotal){
      y[t]^dbern(theta[coin[t]])
    }
    for(j in 1:nCions){
      theta[j]^dbeta(a,b)I(0.0001,0.9999)
    }
    a<-mu*kappa
    b<-(1.0-mu)*kappa
    mu^dbeta(Amu,Bmu)
    kappa^dgamma(Skappa,Rkappa)
    Amu<-2.0
    Bmu<-2.0
    Skappa<-pow(10,2)/pow(10,2)
    Rkappa<-10/pow(10,2)
  }

.temp=file("model.txt","w");writeLines(modelString,con=.tem);close(.temp)
modelCheck("model.txt")

N=c(5,5,5,5,5)
z=c(1,1,1,1,5)

coin=NULL;y=NULL
for(cionIdx in 1:length(N)){
  cion=c(cion,rep(cionIdx,N[cionIdx]))
  y=c(y,rep(1,z[cionIdx]),rep(0,N[cionIdx]-z[cionIdx]))
}

nTrialTotal=length(y)
nCoins=length(unique(coin))
dataList=list(
  y=y,
  coin=coin,
  nTrialTotal=nTrialTotal,
  nCoins=nCoins
)


modelData(bugsData(dataList))


nChain=3
modelCompile(numChain=nChain)
modelGenInits()


burninSteps=1000
modelUpdata(burninSteps)
samplesSet(c("mu","kappa","theta"))
nPerChain=1000
modelUpdata(nPerChain,thin=10)

source("plotChains.R")
plotChains("mu",saveplots=F)
plotChains("kappa",saveplots=F)
plotChains("theta[1]",saveplots=F)

muSample=samplesSample("mu")
kappaSample=samplesSample("kappa")
thetaSample=matrix(0,nrow=nCoins,ncol=nChains*nPerChain)
for(coinIdx in 1:nCoins){
  nodeName=paste("theta[",coinIdx,"]",sep="")
  thetaSample[cionIdx,]=samplesSample(nodeName)
}


source("plotPost.R")
if(nCoin<=5){
  windows(3.2*3,2.5*(1+nCoins))
  layout(matrix(1:(3*(nCoins+1)),nrow=(nCoins+1),byrow=T))
  par(mar=c(2.95,2.95,1.0,0),mgp=c(1.35,0.35,0),oma=c(0.1,0.1,0.1,0.1))
  nPtsToPlot=500
  plotIdx=floor(seq(1,length(muSample),length=nPtsToPlot))
  kPltLim=signif(quantile(kappaSample,p=c(.01,.99)),4)
}
plot(muSample[plotIdx],kappaSample[plotIdx],type="p",ylim=kPltLim,
     xlim=c(0,1),xlab=expression(mu),ylab=expression(kappa),cex.lab=1.5)
plotPost(muSample,xlab="mu",xlim=c(0,1),main="",breaks=20)
plotPost(kappaSample,xlab="kappa",main="",breaks=0,HDItextPLace=.6)
for(coinIdx in 1:nCoins){
  plotPost(thetaSample[cionIdx,],xlab=paste("theta",coinIdx,sep=""),
           xlim=c(0,1),main="",breaks=20,HDItextPlace=.3)
  
  plot(thetaSample[cionIdx,plotIdx],muSample[plotIdx],type="p",
       xlim=c(0,1),ylim=c(0,1),cex.lab=1.5,
       xlab=bquote(theta[.(cionIdx)]),ylab=expression(mu))
  
  plot(thetaSample[cionIdx,plotIdx],kappaSample[plotIdx],type="p",
       xlim=c(0,1),ylim=kPltLim,cex.lab=1.5,
       xlab=bquote(theta[.(cionIdx)]),ylab=expression(kappa))
  
}

dev.copy2eps(file=paste("BernBetaMuKappaBugs",paste(z,collapse=""),".eps",sep=""))

