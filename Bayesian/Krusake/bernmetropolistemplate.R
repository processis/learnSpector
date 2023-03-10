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

