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

