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