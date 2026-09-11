model <- function(time, stocks, auxs) {
  with(as.list(c(stocks, auxs)),{
    aBeta <- aEffective.Contact.Rate / aTotalPopulation
    aLambda <- aBeta * sInfected
    fIR <- sSusceptible*aLambda
    fRR <-sInfected / aDelay
    ds_dt <- -fIR
    dI_dt <- fIR - fRR
    dR_dt <- fRR
    return (list(c(dS_dt,dI_dt,dR_dt),
                 IR=fIR, RR=fRR,Beta=aBeta,Lambda=aLambda,
                 DEL=aDelay,CE=aEffective.Contact.Rate,
                 InitI=initInfected))
  })
}



CE.MIN<-0; CE.MAX<-7.0
DEL.MIN<-1.0; DEL.MAX<-10.0
INIT.INF.MIN<-1.0; 
INIT.INF.MAX<-25.0;


parRange<-data.frame(
  min=c(CE.MIN, DEL.MIN, INIT.INF.MIN),
  max=c(CE.MAX, DEL.MAX, INIT.INF.MAX)
)

rownames (parRange)<-c("aEffective.Contact.Rate", "aDelay",
                       "initInfected")


parRange

p

g.simRuns<-list()


sensRun<-function(p){
  g.simRuns<<-list(length=nrow(p))
  for(i in 1:nrow(p)){
    init <-p[i,"initInfected"]
    auxs <-c(aTotalPopulation=10000,p[i,1:3])
    stocks <- c(sSusceptible=10000-init, sInfected=init,
                sRecovered=0)
    o<-data.frame(ode(y=stocks, simtime, func = model,
                      parms=auxs, method="euler"))
    o$run <-i
    g.simRuns[[i]] <<-0
    }
}


p<-data.frame(Latinhyper(parRange,200))
sensRun(p)


library(plyr)
df<-rbind.fill(g.simRuns)

p1<-ggplot (df,aes (x=time,y=sInfected, color=run,group=run)) +
  geom_line() +
  ylab("Infected") +
  xlab("Time (Days)") + guides (color=FALSE)

p1

runs<-split(df,df$time)

cor.CE<-sapply(runs,function(l){cor(l$sInfected,
                                    l$CE)})

length(cor.CE)

round (head(cor.CE),2)

cor.DEL<-sapply(runs, function(l){cor(1$sInfected, 1$DEL )})
cor.initInf<-sapply(runs,function(l){cor(1$sInfected,
                                          1$InitI)})
                    
                    av.Infected<-sapply(runs,function(l)
                    {mean(l$sInfected)})
                    
                    summary(cor.CE[1:41])
                    
                    summary(cor.DEL[1:41])
                    
                    summary(cor.initInf[1:41])
                    

















