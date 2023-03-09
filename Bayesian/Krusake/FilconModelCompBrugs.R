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

