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

