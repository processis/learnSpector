#HDIofICDF.R

HDIofICDF = function(ICDFname,credMass=0.95,tol=le-8,...){
  incredMass=1.0-credMass
  intervalWidth=function(lowTailPr,ICDFname,credMass,...){
    ICDFname(credMass+lowTailPr,...)-ICDFname(lowTailPr,...)
  }
  optInfo=optimize(intervalWidth,c(0,incredMass),ICDFname=ICDFname,
                   credMass=credMass,tol=tol,...)
  HDIlowTailPr=optInfo$minimum
  return(c(ICDFname(HDIlowTailPr,...),
           ICDFname(credMass+HDIlowTailPr,...)))
}