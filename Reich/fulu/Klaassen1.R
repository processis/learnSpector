library(rstan)

N<-40
x<-rnorm(N,100,20)
res<-rnorm(N,0,10)
y<-10+x*5+res
ex1data<-list("N"~N,"x"~x,"y"~y)

fit1<-stan(
  file="Klaassen1.stan",
  data=ex1data,
  chain=4,
  warmup = 1000,
  iter = 2000,
  cores = 1,
  verbose = TRUE,
  refresh=10,
  pars = "theta",
  include = FALSE
  
)