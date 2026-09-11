library(deSolve)
library(ggplot2)
library(gridExtra)

START<-0;FINISH<-100;STEP<-0.25
simtime<- seq(START, FINISH,by=STEP)
stocks<-c(sStock=100)
auxs<-c(aCapacity=10000,aRef.Availability=1,aRef.GrowthRate=0.10)

model<-function(time,stocks,auxs){
  with(as.list(c(stocks,auxs)),{
    aAvailability<-1-sStock /aCapacity
    aEffect <-aAvailability /aRef.Availability
    aGrowth.Rate <-aRef.GrowthRate *aEffectf
    fNet.Flow<-sStock *aGrowth.Rate
    ds_dt <- fNet.Flow
    return(list(c(dS_dt),NetFlow=fNet.Flow,
                GrowthRate=aGrowth.Rate,
                Effect=aEffect,
                Availability=aAvailability))
  })
}
