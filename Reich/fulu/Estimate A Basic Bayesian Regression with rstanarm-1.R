library(tidyverse)
library(performance)
library(see)

library(bayesplot)
library(bayestestR)

theme_set(theme_bw())

library(rstanarm)
library(ggeffects)

library(BayesPostEst)
library(wooldridge)


d_f<-wooldridge::alcohol
attach(d_f)

View(d_f)

m1<-stan_glm(unemrate~age+educ+famsize+
               married,
             prior=normal(),
             prior_intercept=normal(),
             data=d_f)

mcmc_dens(m1)

posterior_vs_prior(
  m1,
  pars=c("age","educ","famsize","married"),
  group_by_parameter = TRUE)+
  theme_bw()+
  guides(color=FALSE)


pp_check(m1,"dens_overlay")

plot(p_direction(m1))+
  theme(legend.position = "none",
        title = element_blank())

p_direction(m1)








