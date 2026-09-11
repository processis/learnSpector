# the model function.
START <- 0
FINISH <- 20
STEP <- 0.125

simtime <- seq(START, FINISH, by = STEP)
stocks <- c(sSusceptible = 99999, sInfected = 1, sRecovered = 0)
auxs <- c(aTotalPopulation = 100000, aEffective.Contact.Rate = 2,
          aDelay = 2)

model <- function(time, stocks, auxs) {
  with(as.list(c(stocks, auxs)), {
    aBeta <- aEffective.Contact.Rate / aTotalPopulation
    aLambda <- aBeta * sInfected
    
    fIR <- sSusceptible * aLambda
    fRR <- sInfected / aDelay
    
    dS_dt <- -fIR
    dI_dt <- fIR - fRR
    dR_dt <- fRR
    
    return(list(c(dS_dt, dI_dt, dR_dt),
                IR = fIR, RR = fRR, Beta = aBeta, Lambda = aLambda,
                CE = aEffective.Contact.Rate))
  })
}

o <- data.frame(ode(y = stocks, times = simtime, func = model,
                    parms = auxs, method = "euler"))



START <- 0
FINISH <- 20
STEP <- 0.125
NUM_COHORTS <- 3
NUM_STATES <- 3


simtime <- seq(START, FINISH, by = STEP)

CE <- matrix(c(3.0, 2.0, 1.0,
               2.0, 2.0, 1.0,
               1.0, 1.0, 0.5), nrow = 3, ncol = 3, byrow = TRUE)

CE


CohortPopulations <- c(Young = 25000, Adult = 50000, Elderly = 25000)

CohortPopulations

beta <- CE / CohortPopulations

beta


stocks <- c(SusceptibleY = 24999, SusceptibleA = 50000,
            SusceptibleE = 25000, InfectedY = 1,
            InfectedA = 0, InfectedE = 0,
            RecoveredY = 0, RecoveredA = 0,
            RecoveredE = 0)

delays <- c(DY = 2.0, DA = 2.0, DE = 2.0)

auxs <- NULL

model <- function(time, stocks, auxs) {
  with(as.list(c(stocks, auxs)), {
    states <- matrix(stocks, nrow = NUM_COHORTS, ncol = NUM_STATES)
    
    Susceptible <- states[, 1]
    Infected <- states[, 2]
    Recovered <- states[, 3]
    
    Lambda <- beta %*% Infected
    
    IR <- Lambda * Susceptible
    RR <- Infected / delays
    
    dS_dt <- -IR
    dI_dt <- IR - RR
    dR_dt <- RR
    
    return(list(c(dS_dt, dI_dt, dR_dt)))
  })
}



states <- matrix(stocks, nrow = NUM_COHORTS, ncol = NUM_STATES)

states 


Susceptible <- states[, 1]
Susceptible 

Infected <- states[, 2]
Infected 

Recovered <- states[, 3]
Recovered 


Lambda <- beta %*% Infected
Lambda 

IR <- Lambda * Susceptible
as.vector (IR)

RR <- Infected / delays
as.vector (RR)

dS_dt
as.vector (dS_dt)

dI_dt <- IR - RR
as.vector (dI_dt)

dR_dt <- RR
as.vector (dR_dt)


stocks.vy <- c(SusceptibleY=4999, SusceptibleA=50000,
               SusceptibleE-25000, InfectedY=1, InfectedA-0,
               InfectedE=0, RecoveredY=20000,
               RecoveredA=0, RecoveredE=0)

o.vy<-data.frame(ode(y=stocks.vy, times=simtime, func = model,
                     parms=auxs, method="euler"))

stocks.va <- c(SusceptibleY=24999, SusceptibleA=30000,
SusceptibleE=25000, InfectedY=1,InfectedA=0,
InfectedE=0, RecoveredY=0,
RecoveredA=20000, RecoveredE=0)

o.va<-data.frame(ode(y=stocks.va, times=simtime,func = model,
parms=auxs, method="euler"))

stocks.ve <- c(SusceptibleY=24999, SusceptibleA=50000,
               SusceptibleE=5000, InfectedY=1, InfectedA=0,
               InfectedE=0, RecoveredY=0,
               RecoveredA=0, RecoveredE=20000)

o.ve<-data.frame(ode(y=stocks.ve, times=simtime, func = model,
                     parms=auxs, method="euler"))
                 



