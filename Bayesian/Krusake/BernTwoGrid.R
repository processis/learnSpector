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

