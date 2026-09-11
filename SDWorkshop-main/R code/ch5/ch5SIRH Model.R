# This R script implements the SIRH model and also performs
# a sensitivity sweep

library(deSolve)
library(tibble)
library(purrr)
library(glue)
library(dplyr)
library(ggplot2)

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





as_tibble(o)


# One single run, default values
#sim <- run_model()

p1 <- ggplot(o,aes(x=time,y=IR)) + 
        geom_point()+
       geom_line()

p1

p5 <- ggplot(o,aes(x=time,y=RR)) + 
  geom_point()+
  geom_line()


p5

ggplot(o, aes(x = time)) + 
  geom_point(aes(y = IR, color = "IR")) +
  geom_line(aes(y = IR, color = "IR")) +
  geom_point(aes(y = RR, color = "RR")) +
  geom_line(aes(y = RR, color = "RR")) +
  labs(color = "Variable",y="system flow") +
  scale_color_manual(values = c("IR" = "red", "RR" = "blue")) +
  theme_minimal()


ggplot(o, aes(x = time)) + 
  geom_point(aes(y = sSusceptible, color = "sSusceptible")) +
  geom_line(aes(y = sSusceptible, color = "sSusceptible")) +
  geom_point(aes(y = sInfected, color = "sInfected")) +
  geom_line(aes(y = sInfected, color = "sInfected")) +
  geom_point(aes(y = sRecovered, color = "sRecovered")) +
  geom_line(aes(y = sRecovered, color = "sRecovered")) +
  labs(color = "Variable",y="system flow") +
  scale_color_manual(values = c("sSusceptible" = "red", "sInfected" = "blue", "sRecovered" = "green")) +
  theme_minimal()









o <- data.frame(ode(y = stocks, times = simtime, func = model,
                    parms = auxs, method = "euler"))




# Sensitivity sweep, modify 2 params
NSIMS <- 500
s_contacts <- sample(3:20,NSIMS,replace = T)
s_vacc     <- runif(NSIMS,min=0,max = 0.10)

count <- 1
# map2 is an iterator over two vectors
sens <- map2(s_contacts,s_vacc,~{
  message(glue("Sim {count} contacts {.x} vacc Fr {.y}"))
  out_sim <- run_sirh(contacts = .x,VF = .y) %>%
             mutate(Run=count) %>%
             select(Run,everything())
  count <<- count+1
  out_sim
})

full_sims <- bind_rows(sens)

p2 <- ggplot(full_sims,aes(x=time,y=H,colour=Run,group=Run))+
  geom_line()+scale_colour_gradientn(colours=rainbow(10))


summ <- full_sims %>%
  group_by(Run) %>%
  summarise(PeakH=max(H),
            Contacts=first(Contacts),
            VF=first(VaccFrac)) %>%
  ungroup()


arrange(summ,desc(PeakH))

# Plot the results
p3 <- ggplot(summ,aes(x=Contacts,y=VF,size=PeakH,colour=PeakH))+
  geom_point()+
  scale_color_gradient(low="blue", high="red")+geom_jitter()




