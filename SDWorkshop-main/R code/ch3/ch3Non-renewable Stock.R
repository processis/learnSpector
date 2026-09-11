library(deSolve)
library(tibble)
library(purrr)
library(glue)
library(dplyr)
library(ggplot2)

START<-0;FINISH<-200;STEP<-0.25
simtime <- seq(START,FINISH,by=STEP)
stocks<-c(sCapital=5,sResource=1000)
auxs<-c(aDesired.Growth
        =0.07,
        aDepreciation=0.05,aCost.Per.Investment=2.00,aFraction.Reinvested=0.12,aRevenue.Per.Unit= 3.00)

x.Resource<-seq(0,1000,by=100)
y.Efficiency<-c(0,0.25,0.45,0.63,0.75,0.85,0.92,0.96,0.98,0.99,1.0)
func.Efficiency<-approxfun(x=x.Resource,y=y.Efficiency,method="linear",yleft=0,yright=1.0)



model<-function(time,stocks,auxs){
  with(as.list(c(stocks,auxs)),
       {
         aExtr.Efficiency<-func.Efficiency(sResource)
         fExtraction<-aExtr.Efficiency *sCapital
         aTotal.Revenue<-aRevenue.Per.Unit *fExtraction
         aCapital.Costs<-sCapital*0.10
         
         aProfit<-aTotal.Revenue-aCapital.Costs
         aCapital.Funds<-aFraction.Reinvested*aProfit
         aMaximum.Investment<-aCapital.Funds/aCost.Per.Investment
         aDesired.Investment<-sCapital*aDesired.Growth
         
         fInvestment<- min(aMaximum.Investment,
                           aDesired.Investment)
         fDepreciation<-sCapital *aDepreciation
         dS_dt<-fInvestment -fDepreciation
         dR_dt<--fExtraction
         
         
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

as_tibble(o)



# 创建数据框用于绘图
plot_data <- data.frame(
  Resource = x.Resource,
  Efficiency = y.Efficiency
)
# 绘制图形
ggplot(plot_data, aes(x = Resource, y = Efficiency)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "red", size = 3) +
  scale_x_continuous(limits = c(0, 1000), 
                     breaks = seq(0, 1000, by = 250)) +
  scale_y_continuous(limits = c(0, 1), 
                     breaks = seq(0, 1, by = 0.25)) +
  labs(title = "Extraction Efficiency Per Unit Capital",
       x = "Resource",
       y = "Efficiency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))