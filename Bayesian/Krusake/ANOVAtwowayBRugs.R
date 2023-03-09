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


