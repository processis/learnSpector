rm(list=ls())

setwd("/media/user/娱乐/learnSpector/Reich/fulu")
dat<-read.csv("pain.csv")

library(rstanarm)

options(mc.cores=parallel::datectCores())

lmer1<-stan_lmer(paffect ~ pain+active+empstat+ (1|personid),dat)

lmer2<-stan_lmer(paffect ~ pain+active+empstat+ (1|personid),dat,
                 prior = normal(location = 0,scale = 2))


lmer3<-stan_lmer(paffect ~ pain+active+empstat+ (1|personid),dat,
                 prior = normal(location = c(-1,1,0),scale = c(4,1,0.5)))


prior_summary(lmer1)
