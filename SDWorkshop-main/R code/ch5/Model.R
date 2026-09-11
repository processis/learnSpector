library(deSolve)
library(tibble)
library(purrr)
library(glue)
library(dplyr)
library(ggplot2)

sir <- function(time, stocks, auxs) {
  with(as.list(c(stocks, auxs)), {
    aBeta <- aEffective.Contact.Rate / aTotalPopulation
    aLambda <- aBeta * sInfected
    
    fIR <- sSusceptible * aLambda
    fRR <- sInfected / aDelay
    
    dS_dt <- -fIR
    dI_dt <- fIR - fRR
    dR_dt <- fRR
    
    CheckSum<-sSusceptible+sInfected+sRecovered
    
    return(list(c(dS_dt, dI_dt, dR_dt),
                IR = fIR, RR = fRR, Beta = aBeta, Lambda = aLambda,
                CE = aEffective.Contact.Rate))
  })
}

run_sir <- function(start=0, 
                                          finish=2.0,
                                          step=0.125,
                                          contacts=10,
                                          #infectivity=0.1,
                                         
                                            N=100000,
                                          
                                            inits=c(N-1,1,0),
                                          VF=0.0){
     
       simtime <- seq(start, finish, step)
       # initialise vector of stocks
         stocks  <- c(sSusceptible=inits[1],
                                       sInfected=inits[2],
                                       sRecovered=inits[3])
                      
           
           # initialise vector of auxiliaries
           auxs    <-c(aTotalPopulation = 100000, aEffective.Contact.Rate = 2,
                                       aDelay = 2)
           
             sim <-data.frame(ode(y=stocks, 
                                                           times  = simtime, 
                                                           func   = sir, 
                                                           parms  = auxs, 
                                                           method = "euler"))
             
               as_tibble(sim)
           }


# One single run, default values
sim <- run_sir()







