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

