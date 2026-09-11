library(deSolve)

model <- function(time, stocks, auxs) {
  with(as.list(c(stocks, auxs)),{
    aBeta <- aCE / aTotalPopulation
    aLambda <- aBeta * sInfected
    fIR <- sSusceptible * aLambda
    fRR <- sInfected / aDelay
    dS_dt <- -fIR
    dI_dt <- fIR - fRR
    dR_dt <- fRR
    return (list(c(dS_dt,dI_dt,dR_dt),IR=fIR,RR=fRR,
                 Beta=aBeta,Lambda=aLambda,CE=aCE))
  })
}

T01.S.Eq.0.IMP.IR.Eq.0<-function()
{
  START<-0; FINISH<-20; STEP<-0.01;
  simtime <- seq(START, FINISH, by=STEP)
  stocks <- c(sSusceptible=0,sInfected=10000,sRecovered=0)
  auxs <- c(aTotalPopulation=10000, aCE=2, aDelay=2)
  t<-data.frame(ode(y=stocks, times=simtime, func = model,
                    parms=auxs, method="euler"))
  t$Expected<-0
  checkEquals(t$Expected,t$IR)
}


T02.I.Eq.0.IMP.IR.Eq.0<-function()
{
  START<-0; FINISH<-20; STEP<-0.01;
  simtime <- seq(START, FINISH, by=STEP)
  stocks<- c(sSusceptible=10000,sInfected=0,sRecovered=0)
  auxs <- c(aTotalPopulation=10000, aCE=0, aDelay=2)
  t<-data.frame(ode(y=stocks, times=simtime, func = model,
                    parms=auxs, method="euler"))
  t$Expected<-0
  checkEquals(t$Expected,t$IR)
}


T03.CE.Eq.0.IMP.IR.Eq.0<-function()
{
  START<-0; FINISH<-20; STEP<-0.01;
  simtime <- seq(START, FINISH, by=STEP)
  stocks <- c(sSusceptible=9999,sInfected=1,sRecovered=0)
  auxs <- c(aTotalPopulation=10000, aCE=0, aDelay=2)
  t<-data.frame(ode(y=stocks, times=simtime, func = model,
                    parms=auxs, method="euler"))
  t$Expected<-0
  checkEquals (t$Expected, t$IR)
}

T04.D.Eq.INF.IMP.RR.Eq.0<-function()
{
  START<-0; FINISH<-20; STEP<-0.01;
  simtime <- seq(START, FINISH, by=STEP)
  stocks <- c(sSusceptible=0,sInfected=10000,sRecovered=0)
  auxs <- c(aTotalPopulation=10000, aCE=2, aDelay=Inf)
  t<-data.frame(ode(y=stocks, times=simtime, func = model,
                    parms=auxs, method="euler"))
  t$Expected<-0
  checkEquals(t$Expected,t$RR)
}

T05.IMP.All.Vars.GTE.0<-function()
{
  START<-0; FINISH<-20; STEP<-0.01;
  simtime <- seq(START, FINISH, by=STEP)
  stocks <- c(sSusceptible=9999,sInfected=1,sRecovered=0)
  auxs <- c(aTotalPopulation=10000, aCE=20, aDelay=2)
  t<-data.frame(ode(y=stocks, times=simtime, func = model,
                    parms=auxs, method="euler"))
  checkTrue(all(t$sSusceptible>=0))
  checkTrue(all(t$sInfected >= 0))
  checkTrue(all(t$sRecovered>=0)); checkTrue(all(t$IR>=0))
  checkTrue(all(t$RR>=0)); checkTrue(al1(t$Lambda>=0))
}

T06.IMP.Mode.BellShaped<-function()
{
  START<-0; FINISH<-20; STEP<-0.01;
  simtime <- seq(START, FINISH, by=STEP)
  stocks <- c(sSusceptible=9999,sInfected=1,sRecovered=0)
  auxs <- c(aTotalPopulation=10000, aCE=2, aDelay=2)
  t<-data.frame(ode(y=stocks, times=simtime, func = model,
                    parms=auxs, method="euler"))
  expected<-c("EXP", "LOG", "EXP","LOG")
  actual<-bpattern(bmode(t$IR-t$RR,t$time))
  checkEquals(expected,actual)
}

library(RUnit)


testsuite.SIR<-defineTestSuite("SIR Model Tests",
                               dirs=file.path("models/06 chapter/R/tests/"),
                               testFileRegexp = "^TestSuite.+\\.R",
                               testFuncRegexp = "^T.+")
if(isValidTestSuite(testsuite.SIR))
{
  test.result<-runTestSuite(testsuite.SIR,verbose=0)
  summary(test.result)
}

t<-c("TestSuitel.R","TestSuite.C","TestSuite2.R")
t

p <- "^TestSuite.+\\.R"
p

t
grep(p,t)

t[grep(p,t)]


testsuite.SIR <- defineTestSuite("SIR Model Tests",
                                 dirs = file.path("models/06 chapter/R/tests/"),
                                 testFileRegexp = "^TestSuite.+\\.R",
                                 testFuncRegexp = "^T.+")

str(isValidTestSuite(testsuite.SIR))

summary(test.result)








