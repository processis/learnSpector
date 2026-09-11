library(deSolve)
library(tibble)
library(purrr)
library(glue)
library(dplyr)
library(ggplot2)

model <- function(time,stocks,auxs){
  with(as.list(c(stocks,auxs)),{
    aAvailability<-1-sStock /aCapacity
    aEffect <-aAvailability /aRef.Availability
    aGrowth.Rate <-aRef.GrowthRate *aEffect
    fNet.Flow<-sStock *aGrowth.Rate
    dS_dt <- fNet.Flow
    return(list(c(dS_dt),NetFlow=fNet.Flow,
                GrowthRate=aGrowth.Rate,
                Effect=aEffect,
                Availability=aAvailability))
  })
}


run_model <- function(start=0, 
                    finish=100,
                    step=0.25,
                    contacts=10,
                    infectivity=0.1,
                    
                    N=100000,
                    
                    inits=c(10000,1,0.1),
                    VF=0.0){
  
  simtime <- seq(start, finish, step)
  # initialise vector of stocks
  stocks  <- c(aCapacity=inits[1],
               aRef.Availability=inits[2],
               aRef.GrowthRate=inits[3])
  
  
  # initialise vector of auxiliaries
  auxs<-c(aCapacity=10000,aRef.Availability=1,aRef.GrowthRate=0.10)
  
  sim <-data.frame(ode(y=stocks, 
                       times  = simtime, 
                       func   = model, 
                       parms  = auxs, 
                       method = "euler"))
  
  as_tibble(sim)
}

sStock=100

aCapacity

o <- data.frame(ode(y = stocks, times = simtime, func = model,
                    parms = auxs, method = "euler"))







as_tibble(o)


