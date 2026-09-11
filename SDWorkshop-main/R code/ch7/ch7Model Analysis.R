library(deSolve)
library(tibble)
library(purrr)
library(glue)
library(dplyr)
library(ggplot2)

START<-0; FINISH<-100; STEP<-0.25
simtime <- seq(START, FINISH, by=STEP)
stocks <- c(sStock=100)
auxs <- c(aCapacity=10000, aRef.Availability=1,
          aRef.GrowthRate=0.10)

model <- function (time, stocks, auxs) {
  with(as.list(c(stocks, auxs)),{
    aAvailability <- 1 - sStock / aCapacity
    aEffect <- aAvailability / aRef.Availability
    aGrowth.Rate <- aRef.GrowthRate * aEffect
    fNet.Flow <-sStock* aGrowth.Rate
    dS_dt <- fNet.Flow
    return (list (c(dS_dt), NetFlow=fNet.Flow,
                  GrowthRate=aGrowth.Rate,
                  Effect=aEffect,
                  Availability=aAvailability))
  })
}

o<-data.frame (ode (y=stocks, times=simtime, func = model,
                    parms=auxs, method="euler"))

as_tibble(o)


# One single run, default values
#sim <- run_model()

p1 <- ggplot(o,aes(x=time,y=GrowthRate)) + 
  geom_point()+
  geom_line()

p1

p2 <- ggplot(o,aes(x=time,y=Effect)) + 
  geom_point()+
  geom_line()

p2