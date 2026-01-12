library(rstan)
mass <- c(29.9, 1761, 1807, 2984, 3230, 5040, 5654)
age <- c(2, 15, 14, 16, 18, 22, 28)
n <- length(age)

data <- list(mass=mass, age=age, n=n)

fit1<-stan(
  file="paleo.stan",
  data=data,
  chain=2,
  warmup = 1000,
  iter = 2000,
  cores = 1,
  verbose = TRUE,
  pars = "beta1",
  refresh=10,
  include = FALSE
  
)
fit1