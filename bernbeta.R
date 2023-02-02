#ch5

BernBeta = function(priorShape,dataVec,credMass=0.95,saveGraph=F)
{
  if(length(priorShape)!=2){
    stop("priorShape must have two components.")
  }
  if(any(priorShape<=0)){
    stop("priorShape components must be positive.")
  }
  
  if(length(dataVec!=1&dataVec!=0)){
    stop("dataVec must be a vector of isand 0s.")
  }
  if(any(credMass<=0|credMass>=1.0)){
    stop("credMass must be between 0 and 1.")
  }
  
  
  a=priorShape[1]
  b=priorShape[1]
  
  z=sum(dataVec==1)
  N=length(dataVec)
  
  
  postShape=c(a+z,b+N-z)
  
  pData=beta(z+a,N-z+b)/beta(a,b)
  
  source("HDIofICDF.R")
  hpdLim=HDIOfICDF(qbeta,shape1=postShape[1],shape2=postShape[2])
  
  
  binwidth=0.005
  Theta=seq(from=binwidth/2,to=1-(binwidth/2),by=binwidth)
  
  pTheta=dbeta(Theta,a,b)
  pDataGivenTheta = Theta^z*(1-Theta)^(N-z)
  
  pDataGivenTheta = dbeta(Theta,a+z,b+N-z)
  
  pThetaGivenTheta = dbeta(Theta,a+z,b+N-z)
  
  
  window(7,10)
  layout(matrix(c(1,2,3),nrow=3,ncol=1,byrow=FALSE))
  par(mar=c(3,3,1,0),mgp=c(2,1,0),mai=c(0.5,0.5,0.3,0.1))
  maxY=max(c(pTheta,pThetaGivenData))
  
  plot(Theta,pTheta,type="1",lwd=3,
       xlim=c(0,1),ylim=c(0,maxY),cex.axis=1.2,
       xlab=bquote(theta),ylab=bquote(p(theta)),cex.lab=1.5,
       main="Prior",cex.main=1.5)
  if(a>b){textx=0;textadj=c(0,1)}
  else {textx=1;textadj=c(1,1)}
  
  text(text,1.0*max(pThetaGivenData),
       bquote("beta("*theta*"|"*.(a)*","*.(b)*")"),
       cex=2.0,adj=textadj)
  
  plot(theta,pThetaGivenData,type="1",lwd=3,
       xlim=c(0,1),ylim=c(0,maxY),cex.axis=1.2,
       xlab=bquote(theta),ylab=bquote("p("+theta+"jd3"),
       cex.lab=1.5,main="Posterior",cexmain=1.5)
  if(a+z>b+N-z){textx=0:textadj=c(0,1)}
  else{textx=1;textadj=c(1,1)}
  text(textx,1.00+max(pThetaGivenData),cex=2.0,
       bquote("beta("+theta+"|"+(a+z)+","+(b+N-z)+")"),
       adj=textadj)
  
  plot(theta,pThetaGivenData,type="1",lwd=3,
       xlim=c(0,1),ylim=c(0,maxY),cex.axis=1.2,
       xlab=bquote(theta),ylab=bquote("p("+theta+"|D"),
       cex.lab=1.5,main="Posterior",cexmain=1.5)
  
  if(a+z>b+N-z){textx=0;textadj=c(0,1)}
  else{textx=1;textadj=c(1,1)}
  
  text(textx,0.75*max(pTtetaGivenData),cex=2.0,
       bquote("p(D)="*.(signif(pData,3))),adj=textadj)
  
  hpdHt=mean(c(dbeta(hptlim[1],a+z,b+N-z),dbeta(hpdLim[2],a+z,b+N-z)))
  lines(c(hpdLim[1],hpdLim[1]),c(-0.5,hpdHp),type="1",lty=2,lwd=1.5)
  lines(c(hpdLim[2],hpdLim[2]),c(-0.5,hpdHp),type="1",lty=2,lwd=1.5)
  lines(hpdLim,c(hpdHt,hpdHt),type="1",led=2)
  
  text(mean(hpdLim),hpdHt,bquote(.(100+credMass)*"%HDI"),adj=c(0.5,-1.0),cex=2.0)
  text(hpdLim[1],hpdHt,bquote(.(round(hpdLim[1],3))),adj=c(-1.0,-1.0),cex=1.2)
  text(hpdLim[2],hpdHt,bquote(.(round(hpdLim[2],3))),adj=c(-1.0,-1.0),cex=1.2)
  
  if(saveGraph){
    filename=paste("BernBeta_",a,"_",b,"_",z,"_",N,".eps",sep="")
    dev.copy2eps(file=filename)
    
  }
  return(postShape)
  s#top("priorShape component must be positive.")
}




priorA=100
priorB=1
actualDataZ=8
actualDataN=12
postA=priorA+actualDataZ
postB=priorB+actualDataN-actualDataZ

simSampleSize=actualDataN
nSimSamples=10000

simSampleZrecord=vector(length=nSimSamples)

for(sampleIDx in 1:nSimSamples){
  sampleTheta = rbeta(1,postA,postB)
  sampleData=sample(x=c(0,1),prob=c(1-sampleTheta,sampleTheta),
                    size=simSampleSize,replace=TRUE)
  simSampleZrecord[sampleIDx]=sum(sampleData)
  
}

hist(simSampleZrecord)



#ch8

model{
  for (i in 1:N1){y1[i]^dbern(theta1)}
  for (i in 1:N2){y2[i]^dbern(theta2)}
  
  theta1^dbeta(3,3)
  theta2^dbeta(3,3)
  
}

datalist = list(
  N1=7,
  y1=c(1,1,1,1,1,0,0),
  N2=7,
  y2=c(1,1,0,0,0,0,0)
)
modelData(bugsData(datalist))

datalist = list(
  N1=7,
  #y1=c(1,1,1,1,1,0,0),
  N2=7 ,
 #y2=c(1,1,0,0,0,0,0)
)

thetaDiff = theta1Sample = theta2Sample
source("plotPost.R")
window(7,4)
plotPost(thetaDiff , xlab=expression(theta[1]-theta[2]),compVal=0.0,
         breaks=30)


#8.8.1
nInt = 501
theta1=seq(from=((1/nInt)/2),to=(1-((1/nInt)/2)),by=(1/nInt))
theta2=theta1

priorName=c("Beta","Ripples","Null","Alt")[1]
if(priorName=="Beta"){
  a1=3;b1=3;a2=3;b2=3;
  prior1=dbeta(theta1,a1,b1)
  prior2=dbeta(theta2,a2,b2)
  prior=outer(prior1,prior2)
  prior=prior/sum(prior)
}

if(priorName=="Ripples"){
  rippleAtPoint = function(theta1,theta2){
    m1=0;m2=1;k=0.75*pi
    sin((k*(theta1-m1))^2+(k*(theta2-m2))^2)^2
  }
  prior=outer(theta1,theta2,rippleAtPoint)
  prior=prior/sum(prior)
}

if(priorName=="Null"){
  prior=diag(1,nrow=length(theta1),ncol=length(theta1))
  prior=prior/sum(prior)
}

if(priorName=="Alt"){
  prior=matrix(1,nrow=length(theta1),ncol=length(theta2))
  prior=prior/sum(prior)
}

z1=5;N1=7;z2=2;N2=7
likeAtPoint=function(t1,t2){
  p=t1^z1*(1-t1)^(N1-z1)*t2^z2*(1-t2)^(N2-z2)
  return(p)
}
likelihood=outer(theta1,theta2,likeAtPoint)

pData=sum(prior*likelihood)
posterior=(prior*likelihood)/pData


plotFIleName=paste("BernTwoGrid",priorName,".eps",sep="")
credib=.95

rotate=(-25)
tilt=25
parallelness=5.0
shadeval=0.05
perspcex=0.7
ncontour=9
zmax=max(c(max(posterior),max(prior)))


nteeth1=length(theta1)
thindex1=seq(1,nteeth1,by=round(nteeth1/30))
thindex1=c(thindex1,nteeth1)
thindex2=thindex1

window(7,10)
layout(matrix(c(1,2,3,4,5,6),nrow=3,ncol=2,byrow=TRUE))
par(mar=c(3,3,1,0))
par(mgp=c(2,0,1))
par(mai=c(0.4,0.4,0.2,0.05))
par(pty="s")


persp(theta1[thindex1],theta2[thindex2],prior[thindex1,thindex2],
      xlab="theta1",ylab = "theta2",main = "Prior",cex=perspcex,lwd=0.1,
      xlim=c(0,1),ylim=c(0,1),zlim=c(0,zmax),zlab="p(ti,t2)",
      theta=rotate,phi=tilt,d=parallelness,shade=shadeval)
contour(theta1[thindex1],theta2[thindex2],prior[thindex1,thindex2],
        main=bquote(""),levels=signif(seq(0,zmax,length=ncontours),3),
        drawlabels=FALSE,xlab=bquote(theta[1],ylab=bquote(theta[2])))

persp(theta1[thindex1],theta2[thindex2],likelihood[thindex1,thindex2],
      xlab="theta1",ylab="theta2",main="Likelihood",ld=0.1,
      xlim=c(0,1),ylin=c(0,1),zlab="p(D|ti,t2)",cex=perspcex,
      theta=rotate,phi=tilt,d=parallelness,shade=shadeval)

contour(theta1[thindex1],theta2[thindex2],likelihood[thindex1,thindex2],
        main=bquote(""),nlevels=(ncontours-1),
        drawlabels=FALSE,xlab=bquote(theta[1],ylab=bquote(theta[2])))


maxlike=which(likelihood==max(likelihood),arr.ind = TRUE)
if(theta1[maxlike[1]]>0.5){textxpos=0;xadj=0
}else{textxpos=1;xadj=1}

if(theta2[maxlike[2]]>0.5){textxpos=0;xadj=0
}else{textxpos=1;xadj=1}

text(textxpos,textypos,ces=1.5,
     bquote("z1="*.(z1)*".N1=".(N1)*".z2="*.(z2)*".N2="*.(N2))
     adj=c(xadj,yadj))

persp(theta1[thindex1],theta2[thindex2],posterior[thindex1,thindex2],
      xlab="theta1",ylab="theta2",main = "Posteior",cex=perspcex,lwd=0.1,
      xlim=c(0,1),ylim=c(0,1),zlim=c(0,zmax),zlab="p(ti,t2|D)",
      theta=rotate,phi=tilt,d=parallelness,shade=shadeval)

contour(theta1[thindex1],theta2[thindex2],posterior[thindex1,thindex2],
        main=bquote(""),levels=signif(seq(0,zmax,length=ncontours),3),
        drawlabels=FALSE,xlab=bquote(theta[1],ylab=bquote(theta[2])))

maxpost=which(posterior==max(posterior),arr.ind=TRUE)

if(theta1[maxpost[1]]>0.5){textxpos=0;xadj=0
}else{textxpos=1;xadj=1}

if(theta2[maxpost[2]]>0.5){textypos=0;yadj=0
}else{textypos=1;yadj=1}

text(textxpos,textypos,ces=1.5,
     bquote("p(D)"*.(signif(pData,3))),
     adj=c(xadj,yadj))

source("HDIofGrid.R")
HDIheight=HDIofGrid(posterior)Sheight
par(new=TRUE)
contour(theta1[thindex1],theta2[thindex2],posterior[thindex1,thindex2],
        main=bquote(.100*credib)"% HD region"),
        levels=signif(HDIheight,3),lwd=3,
        drawlabels=FALSE,xlab=bquote(theta[1],ylab=bquote(theta[2]))

wantSaveGraph=T
if(wantSaveGraph){dev.copy2eps(file=plotFileName)}



#9.5.1

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




#8.8.4 plotPost.R

plotPost=function(paramSampleVec,credMass=0.95,compVal=NULL,
                  HDItextPlace=0.7,ROPE=NULL,yaxt=NULL,ylab=NULL,
                  xlab=NULL,cex.lab=NULL,cex=NULL,xlim=NULL,main=NULL,
                  showMode=F,...){
  if(is.null(xlab))xlab="Parameter"
  if(is.null(cex.lab))cex.lab=1.5
  if(is.null(cex))cex=1.4
  if(is.null(xlim))xlim=range(c(copmVal,paramSampleVec))
  if(is.null(main))main=""
  if(is.null(yaxt))yaxt="n"
  if(is.null(ylab))ylab=""
  
  par(xpd=NA)
  histinfo=hist(paramSampleVec,xlab=xlab,yaxt=yaxt,ylab=ylab,
                freq=F,col="lightgrey",border="white",
                xlim=xlim,main=main,cex=cex,cex.lab=cex.lab,
                ...)
  
  if(showMode==F){
    meanParam=mean(paramSampleVec)
    text(meanParam,.9*max(histinfo$density),
         bquote(mean==.(signif(meanParam,3))),adj=c(.5,0),cex=cex)
  }else{
    dres=density(paramSampleVec)
    modeParam=dres$x[which.max(dres$y)]
    text(modeParam,.9*max(histinfo$density),
         bquote(model==.(signif(modeParam,3))),adj=c(.5,0),cex=cex)
  }
  
  if(!is.null(compVal)){
    pcgtCompVal=round(100*sum(paramSampleVec>compVal)
                      /length(paramSampleVec),1)
    pcltCompVal=100-pcgtCOmpVal
    lines(c(compVal,compVal),c(.5*max(histinfo$density),0),
                                lty="dashed",lwd=2)
    text(copmVal,.5*max(histinfo$density),
         bquote(.(pcltCompVal)*"%<="*
                  .(signif(compVal,3))*"<"*.(pcgtCompVal)*"%"),
         adj=c(pcltCompVal/100,-0.2),cex=cex)
    
  }
  
  if(!is.null(ROPE)){
    pcInPORE=(sum(paramSampleVec>ROPE[1]&paramSampleVec<ROPE[2])
              /length(paramSampleVec))
    ROPEtextHt=.35*max(histinfo$density)
    lines(c(ROPE[1],ROPE[1]),c(ROPEtextHt,0),lty="dotted",lwd=2)
    lines(c(ROPE[2],ROPE[2]),c(ROPEtextHt,0),lty="dotted",lwd=2)
    text(mean(ROPE),ROPEtextHt,
         bquote(.(round(100*pcInPORE))*"% in ROPE"),
         adj=c(.5,-0.2),cex=1)
  }
  
  source("HDIofMCMC.R")
  HDI=HDIofMCMC(paramSampleVec,credMass)
  lines(HDI,c(0,0),lwd=4)
  text(mean(HDI),0,bquote(.(100*credMass)*"%HDI"),
       adj=c(.5,-1.9),cex=cex)
  text(HDI[1],0,bquote(.(signif(HDI[1],3))),
       adj=c(HDItextPalce,-0.5),cex=cex)
  
  text(HDI[2],0,bquote(.(signif(HDI[2],3))),
       adj=c(1.0-HDItestPlace,-0.5),cex=cex)
  par(xpd=F)
  return(histinfo)
}


#23.3.2 HDIofMCMC.R

HDIofMCMC=function(sampleVec,credMass=0.95){
  sortedPts=sort(sampleVec)
  ciIdxInc=floor(credMass*length(sortedPts))
  nCIs=length(sortedPts)-ciIdxInc
  ciwidth=rep(0,nCIs){
    for(i in 1:nCIs){
      ciwidth[i]=sortedPts[i+ciIdxInc]-sortedPts[i]
    }
    HDImin=sortedPts[which.min(ciwidth)]
    HDImax=sortedPts[which.min(ciwidth)+ciIdxInc]
    HDIlim=c(HDImin,HDImax)
    return(HDIlim)
  }
}

#chapter 7 bernmetropolistemplate.r

myData=c(1,1,1,1,1,1,1,1,1,0,0,0)

likelihood = function(theta,data){
  z=sum(data==1)
  N=length(data)
  pDataGivenTheta=theta^z*(1-theta)^(N-z)
  
  pDataGivenTheta[theta>1|theta<0]=0
  return(pDataGivenTheta)
}

prior=function(theta){
  prior = rep(1,length(theta))
  
  prior[theta>1|theta<0]=0
  return(prior)
}

targetRelProb=function(theta,data){
  targetRelProb=likelihood(theta,data)*prior(theta)
  return(targetRelProb)
}

trajLength=11112
trajectory=rep(0,trajLength)

trajectory[1]=0.50

burnIn=ceiling(.1*trajLength)

nAccepted=0
nRejected=0

set.seed(47405)

for(t in 1:(trajLength-1)){
  currentPosition=trajectory[t]
  
  proposedJump=rnorm(1,mean=0,sd=0.1)
  
  probAccept=min(1,
                 targetRelProb(currentPosition+proposedJump,myData)
                 /targetRelProb(currentPosition,myData))
  
  if(runif(1)<probAccept){
    trajectory[t+1]=currentPosition+proposedJump
    
    if(t>burnIn){nAccepted=nAccepted+1}
  }else{
    trajectory[t+1]=currentPosition
    if(t>burnIn){nRejected+nRejected+1}
  }
}

acceptedTraj=trajectory[(burnIn+1):length(trajectory)]

source("plotPost.R")
histInfo=plotPost(acceptedTraj,xlim=c(0,1),breaks=30)

densMax=max(histInfo$density)
meanTraj=mean(acceptedTraj)
adTraj=sd(acceptedTraj)
if(meanTraj>.5){
  xpos=0.0;xadj=0.0
}else{
  xpos=1.0;xadj=1.0
}
text(xpos,0.75*densMax,
     bquote(N[pro]*"="*.(length(acceptedTraj))*""*
              frac(N[acc],N[pro])*"="*.(signif(nAccep /length(acceptedTraj),3))
            ),adj=c(xadj,0))

a=meanTraj*((meanTraj*(1-meanTraj)/adTraj^2)-1)
b=(1-meanTraj)*((meanTraj*(1-meanTraj)/adTraj^2)-1)

wtdEvid=dbeta(acceptedTraj,a,b)/(
  likeli (acceptedTraj,myData)*prior(acceptedTraj)
)

pData=1/mean(wtdEvid)

if(meanTraj>.5){xpos=0.0;xadj=0.0
}else{xpos=1.0;xadj=1.0}

text(xpos,0.9*densMax,bquote(p(D)==.(signif(pData,3))),
     adj=c(xadj,0),cex=1.5)



#ch4.4.1 BayesUpdate.R

nThetaVals=3
Theta=seq(from=1/(nThetaVals+1),to=nThetaVals/(nThetaVals+1),
          by=1/(nThetaVals+1))

pTheta=pmin(Theta,1-Theta)
pTheta=pTheta/sum(pTheta)

Data=c(1,1,1,0,0,0,0,0,0,0,0,0) 
nHeads=sum(Data==1)
nTails=sum(Data==0)

pDataGivenTheta=Theta^nHeads*(1-Theta)^nTails

pData=sum(pDataGivenTheta*pTheta)
pThetaGivenData=pDataGivenTheta*pTheta/pData

windows(7,10)
layout(matrix(c(1,2,3),nrow=3,ncol=1,byow=FALSE))
par(mar=c(3,3,1,0))
par(mgp=c(2,1,0))
par(mai=c(0.5,0.5,0.3,0.1))

plot(Theta,pTheta,type="h",lwd=3,main="Prior",
     xlim=c(0,1),xlab=bquote(theta),
     ylim=c(0.1,1*max(pThetaGivenData)),ylab=bquote(p(theta)),
     cex.axis=1.2,cex.lab=1.5,cex.main=1.5)


plot(Theta,pDataGivenTheta,type="h",lwd=3,main="Likelihood",
     xlim=c(0,1),xlab=bquote(theta),
     ylim=c(0,1.1*max(pDataGivenTheta)),ylab=bquote(paste("p(D|",theta,")")),
     cex.axis=1.2,cex.lab=1.5,cex.main=1.5)
text(.55,.85*max(pDataGivenTheta),cex=2.0,
     bquote("D="*.(nHeads)*"H."*.(nTails)*"T"),adj=c(0,.5))

plot(Theta,pThetaGivenData,type="h",lwd=3,main="Posterior",
     xlim=c(0,1),xlab=bauote(theta),
     ylim=c(0,1.1*max(pThetaGivenData)),ylab=bquote(paste("p(",theta,"|D)")),
     cex.axos=1.2,cex.lab=1.5,cex.main=1.5)
text(.55,.85*max(pThetaGivenData),cex=2.0,
     bquote("p(D)="*.(signif(pData,3))),adj=c(0,.5))



#3.5.2  IntegralOfDensity.R

meanval=0.0
sdval=0.2
xlow=meanval-3*sdval
xhigh=meanval+3*sdval
dx=0.02

x=seq(from=xlow,to=xhigh,by=dx)
y=(1/(sdval*sqrt(2*pi)))*exp(-.5*((x-meanval)/sdval)^2)
plot(x,y,type="h",lwd=1,cex.axis=1.5,
     xlab="x",ylab="p(x)",cex.lab=1.5,
     main="Normal Probablity Density",cex.main=1.5)

lines(x,y)

area=sum(dx*y)

text(-sdval,.9*max(y),bquote(paste(mu,"=",.(meanval)))
     ,adj=c(1,.5))

text(-sdval,.8*max(y),bquote(paste(mu,"=",.(sdval)))
     ,adj=c(1,.5))

text(sdval,.9*max(y),bquote(paste(mu,"=",.(dx)))
     ,adj=c(0,.5))

text(sdval,.8*max(y),
     bquote(
       paste(sum(,x,),"",Delta,"x p(x)=",.(signif(area,3)))
     ),adj=c(0,.5))

dev.copy2eps(file="IntegralofDensity.eps")