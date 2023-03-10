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

