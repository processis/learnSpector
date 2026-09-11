library(deSolve)

START<-0;FINISH<-200;STEP<-0.25
simtime <- seq(START,FINISH,by=STEP)
stocks<-c(sCapital=5,sResource=1000)
auxs<-c(aDesired.Growth
    =0.07,
    aDepreciationa=0.05,aCost.Per.Investment=2.00,aFraction.Reinvested=0.12,aRevenue.Per.Unit= 3.00)


x.Resource<-seq(0,1000,by=100)
y.Efficiency<-c(0,0.25,0.45,0.63,0.75,0.85,0.92,0.96,0.98,0.99,1.0)
func.Efficiency<-approxfun(x=x.Resource,y=y.Efficiency,method="linear",yleft=0,yright=1.0)


func.Efficiency(-1)

func.Efficiency(500)

func.Efficiency(1000)


model<-function(time,stocks,auxs){
  with(as.list(c(stocks,auxs)),
       {
         aExtr.Efficiency<-func.Efficiency(sResource)
         fExtraction<-aExtr.Efficiency *sCapital
         aTotal.Revenue<-aRevenue.Per.Unit *fExtraction
         aCapital.Costs<-sCapital*0.10
         
         aProfit<-aTotal.Revenue-aCapital.Costs
         aCapital.Funds<=aFraction.Reinvested*aProfit
         aMaximum.Investmenta<-aCapital.Funds/aCost.Per.Investment
         aDesired.Investment<-sCapital*aDesired.Growth
         
         fInvestment<- min(aMaximum.Investment,
                           aDesired.Investment)
         fDepreciation<-sCapital *aDepreciation
         dS_dt<-fInvestment -fDepreciation
         dR-dt<--fExtraction
         
         
         return(list(c(dS_dt,dR_dt),
                     DesiredInvestment=aDesired.Investment,
                     MaximumInvestment=aMaximum.Investment,
                     Investment=fInvestment,
                     Depreciation=fDepreciation,
                     Extraction=fExtraction))
       }
  )
}
       

o<-data.frame(ode(y=stocks,times=simtime,func=model,
                  parms=auxs,method = "euler"))




o[which.max(o$sCapital),"time"]

o[which.max(o$Extraction),"time"]

auxs["aDesired.Growth"] <- 0.05
ol <- data.frame(ode(y = stocks, times = simtime, func = model,
                     parms = auxs, method = "euler"))
ol$GR <- "GR=5%"
base <- ol

auxs["aDesired.Growth"] <- 0.06
o2 <- data.frame(ode(y = stocks, times = simtime, func = model,
                     parms = auxs, method = "euler"))
o2$GR <- "GR=6%"
base <- rbind(base, o2)

auxs["aDesired.Growth"] <- 0.07
o3 <- data.frame(ode(y = stocks, times = simtime, func = model,
                     parms = auxs, method = "euler"))
o3$GR <- "GR=7%"
base <- rbind(base, o3)

o4 <- data.frame(ode(y = stocks, times = simtime, func = model,
                     parms = auxs, method = "euler"))
o4$GR <- "GR=10%"
base <- rbind(base, o4)

auxs["aDesired.Growth"] <- 0.12
o5 <- data.frame(ode(y = stocks, times = simtime, func = model,
                     parms = auxsa, method = "euler"))
o5$GR <- "GR=12%"
base <- rbind(base, o5)

ggplot(data = base, aes(x = time, y = Extraction, color = base$GR)) +
  geom_line() + xlab("Year") + ylab("Extraction Rate") +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(title = NULL))
