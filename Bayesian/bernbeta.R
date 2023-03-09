#ch5 BernBeta.R

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


#8.8.1 BernTwoGrid.R
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



#9.5.1 BernBetaMuKappaBugs.R

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

a=meanTraj*((meanTraj*(1-meanTraj)/sdTraj^2)-1)
b=(1-meanTraj)*((meanTraj*(1-meanTraj)/adTraj^2)-1)

wtdEvid=dbeta(acceptedTraj,a,b)/(
  likeli (acceptedTraj,myData)*prior(acceptedTraj)
)

pData=1/mean(wtdEvid)

if(meanTraj>.5){xpos=0.0;xadj=0.0
}else{xpos=1.0;xadj=1.0}

text(xpos,0.9*densMax,bquote(p(D)==.(signif(pData,3))),
     adj=c(xadj,0),cex=1.5)



#chapter8.8.1 BernTwoGrid.R

nInt=501
theta1=seq(from=((1/nInt)/2),to=(1-((1/nInt)/2)),by=(1/nINt))
theta2=theta2

priorName=c("Beta","Ripples","NUll","Alt")[1]
if(priorName=="Beta"){
  a1=3;b1=3;a2=3;b2=3
  prior1=dbeta(theta1,a1,b1)
  prior2=dbeta(theta2,a2,b2)
  prior=outer(prior1,prior2)
  prior=prior/sum(prior)
  
}

if(priorName=="Ripples"){
  rippleAtPoint=function(theta1,theta2){
    m1=0;m2=1;k=0.75*pi
    sin((k*(theta1=m1))^2+(k*(theta2-m2))^2)^2
  }
  prior=outer(theta1,theta2,rippleAtPoint)
  prior=prior/sum(prior)
}

if(priorName=="NULL"){
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
ncontours=9
zmax=max(c(max(posterior),max(prior)))



nteeth1=length(theta1)
thindex1=seq(1,nteeth1,by=round(nteeth1/30))
thindex1=c(thindex1,nteeth1)
thindex2=thindex1

window(7,10)
layout(matrix(c(1,2,3,4,5,6),nrow=3,ncol=2,byrow=TRUE))
par(mar=c(3,3,1,0))
par(mgp=c(2,1,0))
par(mai=c(0.4,0.4,0.2,0.05))
par(pty="s")


persp(theta1[thindex1],theta2[thindex2],prior[thindex1,thindex2],
      xlab="theta1",ylab="theta2",main="Prior",cex=perspcex,lwd=0.1,
      xlim=c(0,1),ylim=c(0,1),zlim=c(0,zmax),zlab="p(t1,t2",
      theta=rotate,phi=tilt,d=parallelness,shade=shadeval)

contour(theta1[thindex1],theta2[thindex2],prior[thindex1,thindex2],
        main=bquote(""),levels=signif(seq(0,zmax,length=ncontours),3),
        drawlabels = FALSE,xlab=bquote(theta[1]),ylab=bquote(theta[2]))

persp(theta1[thindex1],theta2[thindex2],likelihood[thindex1,thindex2],
      xlab="theta1",ylab="theta2",main="Likelihood",lwd=0.1,
      xlim=c(0,1),ylim=c(0,1),zlab="p(D|t1,t2)",cex=perspcex,
      theta=rotate,phi=tilt,d=parallelness,shade=shadeval)

contour(theta1[thindex1],theta2[thindex2],likelihood[thindex1,thindex2],
        main=bquote(""),nlevels = (ncontours-1),
        xlab=bquote(theta[1]),ylab=bquote(theta[2]),drawlabels = FALSE)

maxlike=which(likelihood==max(likelihood),arr.ind=TRUE)

if(theta [maxlike[1]]>0.5){
  textxpos=0;xadj=0
}else{textxpos=1;xadj=1}

if(theta2[maxlike[2]]>0.5){textxpos=0;xadj=0
}else{textypos=1;yadj=1}

text(textxpos,textypos,cex=1.5,
     bquote("z1="*.(z1)*".N1="*.(N1)*".z2="*.(z2)*".N2="*.(N2)),
     adj=c(xadj,yadj))

persp(theta1[thindex1],theta2[thindex2],posterior[thindex1,thindex2],
      xlab="theta1",ylab="theta2",main="Posterior",cex=perspcex,
      lwd=0.1,xlim=c(0,1),ylim=c(0,1),zlim=c(0,zmax),
      zlab="p(t1,t2|D)",theta=rotate,phi=tilt,d=parallelness,
      shade=shadeval)

coutour(theta1[thindex1],theta2[thindex2],posterior[thindex1,thindex2],
        main=bquote(""),levels=signif(seq(0,zmax,length=ncontours),3),
        drawlabels=FALSE,xlab=bquote(theta[1],ylab=bquote(theta[2])))

maxpost=which(posterior==max(posterior),arr.ind=TURE)

if(theta1[maxpost[1]]>0.5){textxpos=0;xadj=0
}else{textxpos=1;xadj=1}

if(theta2[maxpost[2]]>0.5){textypos=0;yadj=0
}else{textypos=1;yadj=1}

text(textxpos,textypos,cex=1.5,
     bquote("p(D)="*.(signif(pData,3))),adj=c(xadj,yadj))


source(HDIofGrid.R)
HDIheight=HDIofGrid(posterior)$height
par(new=TURE)

contour(theta1[thindex1],theta2[thindex2],posterior[thindex1,thindex2],
        main=bqoute(.(100*credib)*"% HD region"),
        levels=signif(HDIheight,3),lwd=3,drawlabels = FALSE,
        xlab=bquote(theta[1],ylab=bquote(theta[2])))

wantSavedGraph=T
if(wantSavedGraph){dev.copy2eps(file=plotFIleName)}
        

#chapter 10.2.2 
#FilconModelCompBrugs.R

model{
  for(i in 1:nSubj){
    nCorr0fSubj[i]~dbin(theta[i],nTr10fSubj[i])
  theta[i]~dbeta(aBeta[Cond0fSubj[i]],
                 bBeta[Cond0fSubj[i]])I(0.0001,0.9999)
}
kappa0~dgamma(shapeGamma,rateGamma)
for(j in 1:nCond){
  mu[j]~dbeta(aHyperbeta,bHyperbeta)
  kappa[j]~dgamma(shapeGamma,rateGamma)
}

for(j in 1:nCond){
  aBeta[j]<-mu[j]*((kappa[j]*(2-mdlIdx))+(kappa0*(mdlIdx-1)))
  bBeta[j]<-(1-mu[j])*((kappa[j]*(2-mdlIdx))+(kappa0*(mdlIdx-1)))
}

aHyperbeta<-1
bHyperbeta<-1
shapeGamma<-1.0
rateGamma<-0.1
mdlIdx~dcat(modelProb[])
modelProb[1]<-.5
modelProb[2]<-.5
}




model{
  for(i in 1:nSubj){
    nCorr0fSubj[i]~dbin(theta[i],nTr10fSubj[i])
    theta[i]~dbeta(aBeta[Cond0fSubj[i]],
                   bBeta[Cond0fSubj[i]])I(0.0001,0.9999)
  }
  kappa0~dgamma(shk[mdlIdx],rak[malIdx])
  for(j in 1:nCond){
    mu[j]~dbeta(aHyperbeta,bHyperbeta)
    kappa[j]~dgamma(shk[j,mdlIdx],rak[j,mdlIdx])
  }
  for(j in 1:nCond){
    aBeta[j]<-mu[j]*((kappa[j]*(2-mdlIdx))+(kappa0*(mdlIdx-1)))
    bBeta[j]<-(1-mu[j])*((kappa[j]*(2-mdlIdx))+(kappa0*(mdlIdx-1)))
  } 
  aHyperbeta<-1
  bHyperbeta<-1
  
  shP<-1.0
  raP<-0.1
  
  shk0[2]<-shP
  rak0[2]<-raP
  
  shk[1,1]<-shP
  shk[2,1]<-shP
  shk[3,1]<-shP
  shk[4,1]<-shP
  
  
  rak[1,1]<-raP
  rak[2,1]<-raP
  rak[3,1]<-raP
  rak[4,1]<-raP
  
  
  shk0[1]<-54.0
  rhk0[1]<-4.35
  
  shk[1,2]<-11.8
  shk[2,2]<-11.9
  shk[3,2]<-13.6
  shk[4,2]<-12.6
    
  rak[1,2]<-1.34
  rak[2,2]<-1.11
  rak[3,2]<-0.903
  rak[4,2]<-0.748
  
  mdlIdx~dcat(modelProb[])
  modelProb[1]<-.5
  modelProb[2]<-.5
}


#chapter12 OneOddGroupModelComp.R

#modelstring=""

model{
  for(i in 1:nSubj){
    nCorr0fSubj[i]~dbin(theta[i],nTr10fSubj[i])
    theta[i]~dbeta(aBeta[Cond0fSubj[i]],
                   bBeta[Cond0fSubj[i]])I(0.0001,0.9999)
    
  }
  for(j in 1:nCond){
    aBeta[j]<-mu[j]*((kappa[j]*(2-mdlIdx))+(kappa0*(mdlIdx-1)))
    bBeta[j]<-(1-mu[j])*((kappa[j]*(2-mdlIdx))+(kappa0*(mdlIdx-1)))
  } 
  for(j in 1:nCond){
    mu[j]~dbeta(a[j,mdlIdx],b[j,mdlIdx])
  }
  for(j in 1:nCond){
    kappa[j]~dgamma(shk,rak)
  }
  
  nu0~dbeta(a0[mdlIdx],b0[mdlIdx])
  
  shP<-1.0
  raP<-0.1
  aP<-1
  bP<-1
  
  a0[1]<-.53*400
  b0[1]<-(1-.53)*400
  
  a0[2]<-aP
  b0[2]<-bP
  
  a[1,1]<-aP
  a[2,1]<-aP
  a[3,1]<-aP
  a[4,1]<-aP
  b[1,1]<-bP
  b[2,1]<-bP
  b[3,1]<-bP
  b[4,1]<-bP
  
  
  a[1,2]<-.61*100
  a[2,2]<-.50*100
  a[3,2]<-.49*100
  a[4,1]<-.51*100
  b[1,2]<-(1-.61)*100
  b[2,2]<-(1-.50)*100
  b[3,2]<-(1-.49)*100
  b[4,2]<-(1-.51)*100}

  mdlIdx~dcat(modelProb[])
  modelProb[1]<-.5
  modelProb[2]<-.5


writeLines(text=modelstring,con="model.txt")

modelCheck("model.txt")

npg=20
ntrl=20
COnd0fSubj=c(rep(1,npg),rep(2,npg),rep(3,npg),rep(4,npg))
nTrl0fSubj=rep(ntrl,4*npg)
set.seed(47401)
nCorr0fSubj=c(rbinom(npg,ntrl,.61),rbinom(npg,ntrl,.50),
              rbinom(npg,ntrl,.49),rbinom(npg,ntrl,.51))
nSubj=length(Cond0fSubj)
nCOnd=length(unique(COnd0fSubj))

for(condIdx in 1:nCond){
  show(mean(nCorr0fSubj[COnd0fSubj==condIdx]))
}

datalist=list(
  nCOnd=nCond,
  nSubj=nSubj,
  COnd0fSubj=COnd0fSubj,
  nTrl0fSubj=nTrl0fSubj,
  nCorr0fSubj=nCorr0fSubj
)

modelData(bugsData(datalist))

nchain=3
modelCompile(numChains=nchain)
modelGenInits()

burninSteps=5000
modelUpdate(burninSteps)
samplesSet(c("mu","kappa","mu0","theta","mdlIdx"))
nPerChain=5000;nThin=10
modelUpdate(nPerChain,thin=nThin)

###
for(j in 1:2){
  mu[j]~dbeta(a[j,mdlIdx],b[j,mdlIdx])
  
}
mu[3]<-mu[2]
mu[4]<-mu[2]

genInitList<-function(){
  sqzData=.01+.98*datalist$nCorr0fSubj/datalist$nTrlofSubj
  mu=aggregata(sqzData,list(datalist$Cond0fSubj),"mean")[,"x"]
  sd=aggregata(sqzData,list(datalist$Cond0fSubj),"sd")[,"x"]
  kappa=mu*(1-mu)/sd^2-1
  return(
    list(
      theta=sqzData,
      mu=c(mu[1],mean(mu[2:4],NA,NA),
           mu0=mean(mu),
           kappa=kappa,
           mdlIdx=1)
    )
  )
}
for(chainIdx in 1:nchain){
  modelInits(bugsInits(genInitList))
}



theta1<-mu+deflect
theta2<-mu-deflect
mu~dbeta(16,6)
delta~dbeta(1,1)
deflext<-(delta-.5)*2*min(mu,1-mu)



#ANOVAonewayBRugs.R   18.4.1

graphics.off()
rm(list=ls(all=TRUE))
fnroot="ANOVAonewayBRugs"

modelstring="
"
model{
  for(i in 1:Ntotal){
    y[i]~dnorm(mu[i],tau)
    mu[i]<-a0+a[x[i]]
  }
  tau<-pow(sigma,-2)
  sigma~dunif(0,10)
  a0~dnorm(0,0.001)
  for(j in 1:NxLv1){a[j]~dnorm(0.0,atau)}
  atau<-1/pow(aSD,2)
  aSD<-abs(aSDunabs)+.1
  aSDunabs~dt(0,0.001,2)
}

writeLines(modelstring,con="model.txt")
modelCheck("model.txt")


dataSource=c("McDonaldSK1991","SolariLS2008","Random")[1]

if(dataSource=="McDonaldSK1991"){
  fnroot=paste(fnroot,dataSource,sep="")
  datarecord=read.table("McDonaldSK1991data.txt",header=T,
                        colCLasses=c("factor","numeric"))
  
  y=as.numeric(datarecord$Size)
  Ntotal=length(datarecord$Size)
  x=as.numeric(datarecord$Group)
  xnames=levels(datarecord$Group)
  NxLvl=length(unique(datarecord$Group))
  
  contrastList=list(BIGvSMALL=c(-1/3,-1/3,1/2,-1/3,1/2),
                    ORE1vORE2=c(1,-1,0,0,0),
                    ALAvORE=c(-1/2,-1/2,1/2,1/2,0),
                    NPACvORE=c(-1/2,-1/2,1/2,1/2,0),
                    USAvRUS=c(1/3,1/3,1/3,-1,0),
                    FINvPAC=c(-1/4,-1/4,-1/4,-1/4,1),
                    ENGvOTH=c(1/3,1/3,1/3,-1/2,-1/2),
                    FINvRUS=c(0,0,0,-1,1))
}


if(dataSOurce=="SolariLS2008"){
  fnroot=paste(fnroot,dataSource,sep="")
  datarecord=read.table("SolariLS2008data.txt",header=T,
                        colClasses=c("factor","numeric"))
  
  y=as.numeric(datarecord$Acid)
  Ntotal=length(datarecord$Acid)
  x=as.numeric(datarecord$Type)
  xnames=levels(datarecord$Type)
  NxLvl=length(unique(datarecord$Type))
  contrastList=list(G3vOTHER=c(-1/8,-1/8,1,-1/8,-1/8,-1/8,-1/8,-1/8,-1/8))
}



if(dataSource=="Random"){
  fnroot=paste(fnroot,dataSource,sep="")
  ysdtrue=4.0
  a0true=100
  atrue=c(2,-2)
  npercell=8
  datarecord=mareix(0,ncol=2,nrow=length(atrue)*npercell)
  colnames(datarecord)=c("y","x")
  rowidx=0
  for(xidx in 1:length(atrue)){
    for(subjidx in 1:npercell){
      rowidx=rowidx+1
      datarecord[rowidx,"x"]=xidx
      datarecord[rowidx,"y"]=(a0true+atrue[xidx]+rnorm(1,0,ysdtrue))
    }
  }
  
  datarecord=data.frame(y=datarecord[,"y"],x=as.factor(datarecord[,"x"]))
  y=as.numeric(datarecord$y)
  Ntotal=length(y)
  x=as.numeric(datarecord$x)
  xnames=levels(datarecord$x)
  NxLvl=length(unique(x))
  
  
  contrastList=NULL
  for(glidx in 1:(NxLvl-1)){
    for(g2idx in (glidx+1):NxLvl){
      cmpVec=rep(0,NxLvl)
      cmpVec[g1idx]=-1
      cmpVec[g2idx]=1
      contrastList=c(contrastList,list(cmpVec))
    }
  }
}


ySDorig=sd(y)
yMorig=mean(y)
z=(y-yMorig)/ySDorig
datalist=list(
  y=z,
  x=x,
  Ntotal=Ntotal,
  NxLc1=NxLc1
)

modelData(bugsData(datalist))

nchain=5
modelCompile=(numChains=nchain)

if(F){
  modelGenInits()
}else{
  theData=data.frame(y=datalist$y,x=factor(x,labels=xnames))
  a0=mean(theData$y)
  a=aggregate(theData$y,list(theData$x),mean)[,2]-a0
  ssw=aggregate(theData$y,list(theData$x),
                function(x){var(x)*(length(x)-1)})[,2]
  sp=sqrt(sum(ssw)/length(theData$y))
  genInitList<-function(){
    return(
      list(
        a0=a0,
        a=a,
        sigma=sp,
        aSDunabs=sd(a)
      )
    )
  }
  
  for(chainIdx in 1:nchain){
    modelInits(bugsInits(genInitList))
  }
}


BurninSteps=10000
modelUpdate(BurninSteps)
samplesSet(c("a0","a","sigma","aSD"))
stepsPerChain=ceiling(5000/nchain)
thinStep=750
modelUpdate(stepsPerChain,thin=thinStep)


source("plotChains.R")
source("plotPost.R")

checkConvergence=T
if(checkConvergence){
  sumInfo=plotChains("a0",saveplot=T,filenameroot=fnroot)
  sumInfo=plotChains("a",saveplot=T,filenameroot=fnroot)
  sumInfo=plotChains("sigma",saveplot=T,filenameroot=fnroot)
  sumInfo=plotChains("aSD",saveplot=T,filenameroot=fnroot)
}

sigmaSample=samplesSample("sigma")
aSDSample=samplesSample("aSD")
windows()
layout(matrix(1:2,nrow=2))
par(mar=c(3,1,2,5,0),mgp=c(2,0.7,0))
plotPost(sigmaSample,xlab="sigma",main="Cell SD",breaks=30)
plotPost(aSDSample,xlab="aSD",main="a SD",breaks=30)
dev.copy2eps(file=paste(fnroot,"SD.eps",sep=""))

a0Sample=samplesSample("a0")
chainLength=length(a0Sample)
aSample=array(0,dim=c(datalist$NxLvl,chainLength))

for(xidx in 1:datalist$NxLv1){
  aSample[xidx,]=samplesSample(paste("a[",xidx,"]",sep=""))
}


nSample=array(0,dim=c(datalist$NxLv1,chainLength))
for(stepIdx in i:chainLength){
  mSample[,stepIdx]=(a0Sample[stepIdx]+aSample[,stepIdx])
}

b0Sample=apply(mSample,2,mean)
bSample=mSample-matrix(rep(b0Sample,NxLv1),nrow=NxLvl,byrow=T)

b0Sample=b0Sample*ySDorig+yMorig
bSample=bSample*ySDorig

windows(datalist$NxLv1*2.75,2.5)
layout(matrix(1:datalist$NxLv1,nrow=1))
par(mar=c(3,1,2.5,0),mgp=c(2,0.7,0))
for(xidx in 1:datalist$NxLv1){
  plotPOst(bSample[xidx,],breaks=30,
           xlab=bquote(beta*1[.(xidx)]),
           main=paste("x:",xnames[xidx]))
}

dev.copy2eps(file=paste(fnroot,"b.eps",sep=""))

nContrasts=length(contrastList)
if(nContrasts>0){
  nPlotPerROW=5
  nPlotRow=ceiling(nContrasts/nPlotPerROW)
  nPlotCol=ceiling(nContrasts/nPlotRow)
  windows(3.75*nPlotCol,2.5*nPlotRow)
  
  layout(matrix(1:(nPlotRow*nPlotCol),nrow=nPlotRow,ncol=nPlotCol,byrow=T))
  par(mar=c(4,0.5,2.5,0.5),mgp=c(2.0,7.0))
  
  for(mar=c(4,0.5,2.5,0.5),mgp=c(2,0.7,0)){
    contrast=matrix(contrastList[[cIdx]],nrow=1)
    incIdx=contrast!=0
    histInfo=plotPost(contrast %*% bSample,compVal=0,breaks=30,
                      xlab=paste(round(contrast[incIdx],2),xnames[incIdx],
                                 c(rep("+",sum(incIdx)-1),""),collapse=""),
                      cex.lab=1.0,
                      main=paste("X Contrast:",names(contrastList[cIdx])))
  }
  dev.copy2eps(file=paste(fnroot,"xContrasts.eps",sep=""))
}



theData=data.frame(y=y,x=factor(x,labels=xnames))
aovresult=aov(y~x,data=theData)

cat("\n--------------------------------------------------------\n\n")
print(summary(aovresult))
cat("\n--------------------------------------------------------\n\n")
print(model.tables(aovresult,"means"),digits=4)
windows()
boxplot(y~x,data=theData)
cat("\n--------------------------------------------------------\n\n")

print(TukeyHSD(aovresult,"x",ordered=FALSE))
windows()

plot(TukeyHSD(aovresult,"x"))

if(T){
  for(xIdx1 in 1:(NxLv1~1)){
    for(xIdx2 in (xIdx1+1):NxLv1){
      cat("\n--------------------------------------------------------\n\n")
      cat("xIdx1=",xIdx1,",xIdx2=",xIdx2,
          ",M2-M1=",mean(y[x==xIdx2])-mean(y[x==xIdx1]),"\n")
      print(t.test(y[x==xIdx2],y[x==xIdx1],var.equal=T))
    }
  }
}

cat("\n--------------------------------------------------------\n\n")




#plotPost.R

plotPost=function(paramSampleVec,credMass=0.95,compVal=NULL,
                  HDItextPLace=0.7,ROPE=NULL,yaxt=NULL,ylab=NULL,
                  xlab=NULL,cex.lab=NULL,cex=NULL,xlim=NULL,main=NULL,
                  showMOde=F,...){
  if(is.null(xlab))xlab="Parameter"
  if(is.null(cex.lab))cex.lab=1.5
  if(is.null(cex))cex=1.4
  if(is.null(xlim))xlim=range(c(compVal,paramSampleVec))
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
    modeParam=dres$x[which.max(dres$y),
                     bquote(mode==.(signif(modeParam,3))),adj=c(.5,0),cex=cex)
  }
  
  if(!is.null(compVal)){
    pcgtCompVal=round(100*sum(paramSampleVec>compVal)
                      /length(paramSampleVec),1)
    pcltCompVal=100-pcgtCompVal
    lines(c(compVal,compVal),c(.5*max(histinfo$density),0),
          lty="dashed",lwd=2)
    text(compVal,.5*max(histinfo$density),
         bquote(.(pcltCompVal)*"%<="*
                  .(signif(compVal,3))*"<"*.(pcgtCompVal)*"%"),
         adj=c(pcltCompVal/100,-0.2),cex=cex)
  }
  
  if(!is.null(ROPE)){
    pcInROPE=(sum(paramSampleVec>ROPE[1]&paramSampleVec<ROPE[2])
              /length(paramSampleVec))
    
    ROPEtextHt=.35*max(histinfo$density)
    lines(c(ROPE[1],ROPE[1]),c(ROPEtextHt,0),lty="dotted",lwd=2)
    lines(c(ROPE[2],ROPE[2]),c(ROPEtextHt,0),lty="dotted",lwd=2)
    text(mean(ROPE),ROPEtextHt,
         bquote(.(round(100*pcInROPE))*"% in ROPE"),
         adj=c(.5,-0.2),cex=1)
  }
  
  source("HDIofMCMC.R")
  HDI=HDIofMCMC(paramSampleVec,credMass)
  lines(HDI,c(0,0),lwd=4)
  text(mean(HDI),0,bquote(.(100*credMass)*"% hdi"),
       adj=c(.5,-1.9),cex=cex)
  text(HDI[1],0,bquote(.(signif(HDI[1],3))),
       adj=c(HDItextPLace,-0.5),cex=cex)
  text(HDI[2],0,bquote(.(signif(HDI[2],3))),
       adj=c(1.0-HDItextPLace,-0.5),cex=cex)
  par(xpd=F)
  return(histinfo)
}

##19.3.1 ANOVAtwowayBRugs.R

graphics.off()
rm(list=ls(all=TRUE))
fnroot="ANOVAtwowayBrugs"
library(BRugs)

modelstring=""

model{
  for(i in 1:Ntotal){
    y[i]~dnorm(mu[i],tau)
    mu[i]<-a0+a1[x1[i]]+a2[x2[i]]+a1a2[x1[i],x2[i]]
  }
  tau<-pow(sigma,-2)
  sigma~dunnif(0,10)
  
  a0~dnorm(0,0.001)
  
  for(j1 in 1:Nx1Lvl){a1[j1]~dnorm(0.0,altua)}
  
  a1tau<-1/pow(a1SD,2)
  a1SD<-abs(a1SDunabs)+.1
  alSDunabs~dt(0,0.001,2)
  
  for(j1 in 1:Nx1Lvl){for(j1 in 1:Nx2Lvl){
    a1a2[j1,j2]~dnorm(0.0,a1a2tua)
  }}
  a1a2tua<-1/pow(a1a2SD,2)
  a1a2SD<-abs(a1a2SDunabs)+.1
  a1a2SDunabs~dt(0,0.001,2)
}
writeLines(modelstring,con="model.txt")
modelCheck("model.txt")


dataSource=c("QianS2007","Salary","Random","Ex19.3")[4]

if(dataSource=="QianS2007"){
  fnroot=paste(fnroot,dataSource,sep="")
  datarecord=read.table("QianS2007SeaweedData,txt",header=TRUE,sep=",")
  
  datarecord$COVER=-log((100/datarecord$COVER)-1)
  y=as.numeric(datarecord$COVER)
  x1=as.numeric(datarecord$TREAT)
  x1names=levels(datarecord$TREAT)
  x2=as.numeric(datarecord$BLOCK)
  x2names=levels(datarecord$BLOCK)
  Ntotal=length(y)
  Nx1Lvl=length(unique(x1))
  Nx2Lvl=length(unique(x2))
  
  x1contrastList=list(f_Effect=c(1/2,-1/2,0,1/2,-1/2,0),
                      f_Effect=c(0,1/2,-1/2,0,1/2,-1/2),
                      L_Effect=c(1/3,1/3,1/3,-1/3,-1/3,-1/3))
  x2contrastList=NULL
  x1x2contrastList=NULL
}

if(dataSOurce=="Salary"){
  fnroot=paste(fnroot,dataSource,sep="")
  
  datarecord=read.table("Salary.csv",header = TRUE,sep=",")
  y=as.numeric(datarecord$Salary)
  if(F){
    y=log10(y)
    fnroot=paste(fnroot,"Log10",sep="")
  }
  x1=as.numeric(datarecord$Org)
  x1names=levels(datarecord$Org)
  x2=as.numeric(datarecord$Post)
  x2names=levels(datarecord$Post)
  Ntotal=length(y)
  Nx1Lvl=length(unique(x1))
  Nx2Lvl=length(unique(x2))
  
  x1contrastList=list(BFINvCEDP=c(1,-1,0,0),
                      CEDPvTHTR=c(0,1,0,-1))
  x2contrastList=list(FT1vFT2=c(1,-1,0),FT2vFT3=c(0,1,-1))
  x1x2contrastList=list(
    CHEMvTHTRxFT1vFT3=outer(c(0,0,+1,-1),c(+1,0,-1)),
    BFINvOTHxFT1vOTH=outer(c(+1,-1/3,-1/3,-1/3),c(+1,-1/2,-1/2))
  )
}

if(dataSource=="Random"){
  fnroot=paste(fnroot,dataSource,sep="")
  set.seed(47405)
  ysdtrue=3.0
  a0true=100
  a1true=c(2,0,-2)
  a2true=c(3,1,-1,-3)
  a1a2true=matrix(c(1,-1,0,-1,1,0,0,0,0,0,0,0),
                  nrow=length(a1true),ncol=length(a2true),byrow=F)
  
  npercell=8
  datarecord=matrix(0,ncol=3,nrow=length(a1true)*length(a2true)*npercell)
  colnames(datarecord)=c("y","x1","x2")
  rowidx=0
  for(x1idx in 1:length(a1true)){
    for(x2idx in 1:length(a2true)){
      for(subjidx in 1:npercell){
        rowidx=rowidx+1
        datarecord[rowidx,"x1"]=x1idx
        datarecord[rowidx,"x2"]=x2idx
        datarecord[rowidx,"y"]=(a0true+a1true[x1idx]+a2true[x2idx]
                                +a1a2true[x1idx,x2idx]+rnorm(1,0,ysdtrue))
      }
    }
  }
  
  datarecord=data.frame(y=datarecord[,"y"],
                        x1=as.factor(datarecord[,"x1"]),
                        x2=as.factor(datarecord[,"x2"]))
  
  y=as.numeric(datarecord$y)
  x1=as.numeric(datarecord$x1)
  x1names=levels(datarecord$x1)
  x2=as.numeric(datarecord$x2)
  x2names=levels(datarecord$x2)
  Ntotal=length(y)
  Nx1Lvl=length(unique(x1))
  Nx2Lvl=length(unique(x2))
  x1contrastList=list(X1_1v3=c(1,0,-1))
  x2contrastList=list(X2_11v34=c(1/2,1/2,-1/2,-1/2))
  x1x2contrastList=list(
    IC_11v22=outer(c(1,-1,0),c(1,-1,0,0)),
    IC_23v34=outer(c(0,1,-1),c(0,0,1,-1))
  )
                        
}

if(dataSource=="Ex19.3"){
  fnroot=paste(fnroot,dataSource,sep="")
  y=c(101,102,103,105,104, 104,105,107,106,108, 105,107,106,108,109, 109,108,110,111,112)
  x1=c(1,1,1,1,1, 1,1,1,1,1, 2,2,2,2,2, 2,2,2,2,2)
  x2=c(1,1,1,1,1, 2,2,2,2,2, 1,1,1,1,1, 2,2,2,2,2)
  
  x1names=c("x1.1","x1.2")
  x2names=c("x2.1","x2.2")
  
  Ntotal=length(y)
  Nx1Lvl=length(unique(x1))
  Nx2Lvl=length(unique(x2))
  
  x1contrastList=list(X1.2vX1.1=c(-1,1))
  x2contrastList=list(X2.2vX2.1=c(-1,1))
  x1x2contrastList=NULL
}

ySDorig=sd(y)
yMorig=mean(y)
z=(y-yMorig)/ySDorig
datalist=list(
  y=z,
  x1=x1,
  x2=x2,
  Ntotal=Ntotal,
  Nx1Lvl=Nx1Lvl,
  Nx2Lvl=Nx2Lvl
)

modelData(bugsData(datalist))

nchain=10
modelCompile(numChains=nchain)

if(F){
  modelGenInits()
}else{
  theData=data.frame(y=datalist$y,x1=factor(x1,labels=x1names),
                     x2=factor(x2,labels=x2names))
  a0=mean(theData$y)
  a1=aggregate(theData$y,list(theData$x1),mean)[,2]-a0
  a2=aggregate(theData$y,list(theData$x2),mean)[,2]-a0
  linpred=as.vector(outer(a1,a2,"+")+a0)
  a1a2=aggregate(theData$y,list(theData$x1,theData$x2),mean)[,3]-linpred
  
  genInitList<-function(){
    return(
      list(
        a0=a0,
        a1=a1,
        a2=a2,
        a1a2=matrix(a1a2,nrow = Nx1Lv1,ncol = Nx2Lv1),
        sigma=sd(theData$y)/2,
        a1SDunads=sd(a1),
        a1a2SDunabs=sd(a1a2)
      )
    )
  }
  for(chainIdx in 1:nchain){
    modelInits(bugsInits(genInitList))
  }
}


BurninSteps=10000
modelUpdate(BurninSteps)

samplesSet(c("a0","a1","a2","a1a2",
             "sigma","a1SD","a2SD","a1a2SD"))
stepsPerChain=ceiling(2000/nchain)
thinStep=500
modelUpdate(stepsPerChain,thin=thinStep)


source("plotChains.R")
source("plotPost.R")

checkConvergence=F
if(checkConvergence){
  sumInfo=plotChains("a0",saveplots=F,filenameroot=fnroot)
  sumInfo=plotChains("a1",saveplots=F,filenameroot=fnroot)
  sumInfo=plotChains("a2",saveplots=F,filenameroot=fnroot)
  sumInfo=plotChains("a1a2",saveplots=F,filenameroot=fnroot)
  readline("Press any key to clear graphics and continue")
  
  graphics.off()
  sumInfo=plotChains("sigma",saveplots=F,filenameroot=fnroot)
  sumInfo=plotChains("a1SD",saveplots=F,filenameroot=fnroot)
  sumInfo=plotChains("a2SD",saveplots=F,filenameroot=fnroot)
  sumInfo=plotChains("a1a2SD",saveplots=F,filenameroot=fnroot)
  readline("Press any key to clear graphics and continue")
  
  graphics.off()
}

sigmaSample=samplesSample("sigma")
a1SDSample=samplesSample("a1SD")
a2SDSample=samplesSample("a2SD")
a1a2SDSample=samplesSample("a1a2SD")

windows()
layout(matrix(1:4,nrow=2))
par(mar=c(3,1,2.5,0),mgp=c(2,0.7,0))
plotPost(sigmaSample,xlab="sigma",main="Cell SD",breaks=30)
plotPost(a1SDSample,xlab="a1SD",main="a1 SD",breaks=30)
plotPost(a2SDSample,xlab="a2SD",main="a2 SD",breaks=30)
plotPost(a1a2SDSample,xlab="a1a2SD",main="Interaction SD",breaks=30)
dev.copy2eps(file=paste(fnroot,"SD.eps",sep=""))

a0Sample=samplesSample("a0")
chainLength=length(a0Sample)
a1Sample=array(0,dim=c(datalist$Nx1Lv1,chainLength))
for(x1idx in 1:datalist$Nx1Lv1){
  a1Sample[xlidx,]=samplesSample(paste("a1[",xlidx,"]",sep=""))
}
a2Sample=array(0,dim=c(datalist$Nx2Lv1,chainLength))

for(x2idx in 1:datalist$Nx2Lv1){
  a2Sample[x2idx,]=samplesSample(paste("a2[",x2idx,"]",sep=""))
  
}
a1a2Sample=array(0,dim=c(datalist$Nx1Lv1,datalist$Nx2Lv1,chainLength))
for(x1idx in 1:datalist$Nx1Lv1){
  for(x2idx in 1:datalist$Nx2Lv1){
    a1a2Sample[x1idx,x2idx]=samplesSample(paste("a1a2[",x2idx,",",x2idx,"]",
                                                sep=""))
  }
}


m12Sample=array(0,dim=c(datalist$Nx1Lv1,datalist$Nx2Lv1,chainLength))
for(stepIdx in 1:chainLength){
  m12Sample[.,stepIdx]=(a0Sample[stepIdx]
                        +outer(a1Sample[,stepIdx],
                               a2Sample[,stepIdx],"+")
                        +a1a2Sample[.,stepIdx])
}
b0Sample=apply(m12Sample,3,mean)
b1Sample=(apply(m12Sample,c(1,3),mean)
          -matrix(rep(b0Sample,Nx1Lv1),nrow=Nx1Lv1,byrow=T))
b1Sample=(apply(m12Sample,c(2,3),mean)
          -matrix(rep(b0Sample,Nx2Lv1),nrow=Nx2Lv1,byrow=T))

linpredSample=array(0,dim=c(datalist$Nx1Lv1,datalist$Nx2Lv1,chainLength))

for(stepIdx in 1:chainLength){
  linpredSample[.,stepIdx]=(b0Sample[stepIdx]
                            +outer(b1Sample[,stepIdx],
                                   b2Sample[,stepIdx],"+"))
}

b1b2Sample=m12Sample-linpredSample
b0Sample=b0Sample*ySDorig+yMorig
b1Sample=b1Sample*ySDorig
b2Sample=b2Sample*ySDorig
b1b2Sample=b1b2Sample*ySDorig

windows((datalist$Nx1Lv1+1)*2.75,(datalist$Nx2Lv1+1)*2.0)
layoutMat=matrix(0,nrow=(datalist$Nx2Lv1+1),ncol=(datalist$Nx1Lv1+1))

layoutMat[1,1]=1
layoutMat[1,2:(datalist$Nx1Lv1+1)]=1:datalist$Nx1Lv1+1

layoutMat[2:(datalist$Nx2Lv1+1),1]=1:datalist$Nx2Lv1+(datalist$Nx1Lv1+1)
layoutMat[2:(datalist$Nx2Lv1+1),2:(datalist$Nx1Lv1+1)]=matrix(
  1:(datalist$Nx1Lv1*datalist$Nx2Lv1)+(datalist$Nx2Lv1+datalist$Nx1Lv1+1),
  ncol=datalist$Nx1Lv1,byrow=T)

layout(layoutMat)
par(mar=c(4,0.5,2.5,0.5),mgp=c(2,0.7,0))

histinfo=plotPost(b0Sample,xlab=expression(beta*0),main="Baseline",
                  breaks=30)

for(x1idx in 1:datalist$Nx1Lv1){
  histinfo=plotPost(b1Sample[x1idx,],xlab=bquote(beta*1[.(x1idx)]),
                    main=paste("x1:",x1names[x1idx]),breaks=30)
}
for(x2idx in 1:datalist$Nx2Lv1){
  histinfo=plotPost(b2Sample[x2idx,],xlab=bquote(beta*2[.(x2idx)]),
                    main=paste("x2:",x2names[x2idx]),breaks=30)
}

for(x2idx in 1:datalist$Nx2Lv1){
  for(x1idx in 1:datalist$Nx1Lv1){
    histinfo=plotPost(b1b2Sample[x1idx,x2idx,],breaks=30,
                      xlab=bquote(beta*12[.(x1idx)*","*.(x2idx)]),
                      main=paste("x1:",x1names[x1idx],",x2:",x2names[x2idx]))
  }
}

dev.copy2eps(file=paste(fnroot,"b.eps",sep=""))

nContrasts=length(x1contrastList)
if(nCOntrasts>0){
  nPlotPerRow=5
  nPlotRow=ceiling(nContrasts/nPlotPerRow)
  nPlotCol=ceiling(nContrasts/nPlotPerRow)
  windows(3.75*nPlotCol,2.5*nPlotRow)
  layout(matrix(1:(nPlotRow*nPlotCol),nrow=nPlotRow,ncol=nPlotCol,byrow=T))
  par(mar=c(4,0.5,2.5,0.5),mgp=c(2,0.7,0))
  
  for(cIdx in 1:nContrasts){
    contrast = matrix(x1contrastList[[cIdx]],nrow=1)
    incIdx=contrast!=0
    histInfo=plotPost(contrast %*% b1Sample,compVal = 0,breaks=30,
                      xlab=paste(round(contrast[incIdx],2),x1names[incIdx],
                                 c(rep("+",sum(incIdx)-1),""),collapse=""),
                      cex.lab=1.0,
                      main=paste("X1 Contrast:",names(x1contrastList)[cIdx]))
  }
  
  dev.copy2eps(file=paste(fnroot,"x1Contrasts.eps",sep=""))
}

nContrasts=length(x2contrastList)
if(nContrasts>0){
  nPLotPerRow=5
  nPlotRow=ceiling(nContrasts/nPLotPerRow)
  nPLotCol=ceiling(nContrasts/nPlotRow)
  
  windows(3.75*nPLotCol,2.5*nPlotRow)
  layout(matrix(1:(nPlotRow*nPLotCol),nrow=nPlotRow,ncol=nPLotCol,byrow = T))
  par(mar=c(4,0.5,2.5,0.5),mgp=c(2,0.7,0))
  
  for(cIdx in 1:nContrasts){
    contrast=matrix(x2contrastList[[cIdx]],nrow=1)
    incIdx=contrast!=0
    histInfo=plotPost(contrast %*% b2Sample,compVal=0,breaks=30,
                      xlab=paste(round(constrast[incIdx],2),x2names[incIdx],
                                 c(rep("+",sum(incIdx)-1),""),collapse=""),
                      cex.lab=1.0,
                      main=paste("X2 Contrast:",names(x2contrastList)[cIdx]))
  }
  dev.copy2eps(file=paste(fnroot,"x2Contrasts.eps",sep=""))
}

theData=data.frame(y=y,x1=factor(x1,labels=x1names),
                   x2=factor(x2,labels=x2names))

windows()
interaction.plot(theData$x1,theData$x2,theData$y,type="b")
dev.copy2eps(file=paste(fnroot,"DataPlot.eps",sep=""))
aobresuit=aov(y~x1*x2,data=theData)

cat("\n-------------------------------------------------------\n\n")
print(summary(aobresuit))
cat("\n-------------------------------------------------------\n\n")
print(model.tables(aovresult,type="effects",se=TRUE),digits = 3)
cat("\n-------------------------------------------------------\n\n")



##20.5.1  MultipleLogisticRegressionBrugs.R

graphics.off()
rm(list=ls(all=TRUE))
fname="MultipleLogisticRegressionBrugs"
library(Brugs)

model{
  for(i in 1:nData){
    y[i]~dbern(mu[i])
    mu[i]<-1/(1+exp(-(b0+inprod(b[],x[1,]))))
    
  }
  b0~dnorm(0,1.0E-12)
  for(j in i:nPredictors){
    b[j]~dnorm(0,1.0E-12)
  }
}

writeLines(modelstring.con="model.txt")
modelCheck("model.txt")

dataSource=c("HtWt","Cars","HeartAttack")[3]

if(dataSource=="HtWt"){
  fname=paste(fname,dataSource,sep="")
  
  source("HtWtDataGenerator.R")
  
  dataMat=HtWtDataGenerator(nSubj=70,rndsd=474)
  predictedName="male"
  predictorNames=c("height","weight")
  nData=NROW(dataMat)
  y=as.matrix(dataMat[,predictedName])
  x=as.matrix(dataMat[,predictorNames])
  nPredictors=NCOL(x)
}

if(dataSource=="Cars"){
  fname=paste(fname,dataSource,sep="")
  dataMat=read.table(file="Lock1993data.txt",header=T,sep="")
  predictedName="AirBag"
  predictorNames=c("MidPrice","RPM","Uturn")
  nData=NROW(dataMat)
  y=as.matrix(as.numeric(dataMat[,predictedName]>0))
  x=as.matrix(dataMat[,predictorNames])
  nPredictors=NCOL(x)
  
}

if(dataSource=="HeartAttack"){
  fname=paste(fname,dataSource,sep="")
  dataMat=read.table(file="BloodDataGeneratorOutput.txt",header=T,sep="")
  predictedName="HeartAttack"
  predictorNames=c("Systolic","Diastolic","Weight","Cholesterol",
                   "Height","Age")
  nData=NROW(dataMat)
  y=as.matrix(as.numeric(dataMat[,predictedName]>0))
  x=as.matrix(dataMat[,predictorNames])
  nPredictors=NCOL(x)
  
}

standardizeCols=function(dataMat){
  zDataMat=dataMat
  for(colIdx in 1:NCOL(dataMat)){
    mCol=mean(dataMat[,colIdx])
    sdCol=sd(dataMat[,colIdx])
    zDataMat[,colIdx]=(dataMat[,colIdx]-mCol)/sdCol
  }
  return(zDataMat)
}
zx=standardizeCols(x)
zy=y

datalist=list(
  x=zx
  y=as.vector(zy)
  nPredictors=nPredictors,
  nData=nData
)
modelData(bugsData(datalist))


nchain=3
modelCompile(numChain=nchain)

genInitList<-function(){
  glmInfo=glm(datalist$y~datalist$x,family=binomial(logit))
  show(glmInfo)
  b0Init=glmInfo$coef[1]
  bInit=glmInfo$coef[-1]
  return(list(
    b0=b0Init,
    b=bInit
  ))
}

for(chainIdx in 1:nchain){
  modelInit(bugsInits(genInitList))
}

BurninSteps=1000
modelUpdate(BurninSteps)
samplesSet(c("b0","b"))
stepsPerChain=ceiling(5000/nchain)
thinStep=50
modelUpdate(stepsPerChain,thin=thinStep)


source("plotChains.R")
source("plotPost.R")

checkConvergence=T
if(checkConvergence){
  b0Sum=plotChains("b0",saveplots=F,filenameroot=fname)
  bSum=plotChains("b",saveplots=F,filenameroot=fname)
}

zb0Sample=matrix(samplesSample("b0"))
chainLength=length(zb0Sample)
zbSample=NULL
for(j in 1:nPredictors){
  zbSample=cbind(zbSample,samplesSample(paste("b[".j."]",sep="")))
}

x=dataMat[,predictorNames]
y=dataMat[,predictedName]
My=mean(y)
SDy=sd(y)
Mx=apply(x,2,mean)
SDx=apply(x,2,sd)
b0Sample=0*zb0Sample
bSample=0*zbSample
for(stepIdx in 1:chainLength){
  b0Sample[stepIdx]=(zb0Sample[stepIdx]
                     -sum(Mx/SDx*zbSample[stepIdx,]))
  for(j in 1:nPredictors){
    bSample[stepIdx,j]=zbSample[stepIdx,j]/SDx[j]
  }
}

windows()
pairs(cbind(b0Sample[thinIdx],bSample[thinIdx,]),
      labels=c("b0",paste("b_",predictorNames,sep="")))
dev.copy2eps(file=paste(fname,"PostPairs.eps",sep=""))

windows(3.5*(1+nPredictors),2.75)
layout(matrix(1:(1+npredictors),nrow=1))

histInfo=plotPost(b0Sample,xlab = "b0 Value",compVal = NULL,breaks=30,
                  main=paste("logit(p(",predictedName,
                             "=1))when predictors=zero",sep=""))
                             

for(bIdx in 1:nPredictors){
  histInfo=plotPost(bSample[,bIdx],xlab=paste("b",bIdx,"Value",sep=""),
                    compVal = 0.0,breaks=30,
                    main=paste(predictorNames[bIdx]))
}

dev.copy2eps(file=paste(fname,"PostHist.eps",sep=""))

for(p1idx in 1:(nPredictors-1)){
  for(p2idx in (p1idx+1):nPredictors){
    windows()
    xRange=range(x[,p1idx])
    yRange=range(x[,p2idx])
    
    plot(NULL,NULL,main=predictedName,xlim=xRange,ylim=yRange,
         xlab=predictorName[p1idx],ylab=predictorNames[p2idx])
    
    for(chainIdx in ceiling(seq(1,chainLength,length=20))){
      abline(-(b0Sample[chainIdx]
               +if(nPredictor>2){
                 bSample[chainIdx,c(-p1idx,-p2idx)]*Mx[c(-p1idx,-p2idx)]
               }else{0})
             /bSample[chainIdx,p2idx],
             -bSample[chainIdx,p1idx]/bSample[chainIdx,p2idx],
             col="grey",lwd=2)
    }
    
    for(yVal in 0:1){
      rowIdx=(y==yVal)
      points(x[rowIdx,p1odx],x[rowIdx,p2idx],pch=as.character(yVal),
             cex=1.75)
    }
    
    dev.copy2eps(file=paste(fname,"PostContours",p1idx,p2idx,".eps",sep=""))
  }
}

glmRes=glm(datalist$y~as.matrix(x),family=binomial(logit))
show(glmRes)
                             
                             
  