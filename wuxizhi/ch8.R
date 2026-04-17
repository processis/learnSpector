set.seed(1010)
N <- 100
theta <- 0.6
y <- rbinom(N, 1, theta)
coin.data <- list(N=N, y=y)

Coin="
data{
int<lower=0>N;
int<lower=0,upper=1>y[N];
}
parameters{
real<lower=0,upper=1>theta;
}

model{
theta~beta(1,1);
for(n in 1:N)
y[n]~bernoulli(theta);
}
"

library(rstan)

fit_c<-stan(model_code=Coin,data=coin.data,chains=2,warmup=1000,
            iter=2000,refresh=1000)

print(fit_c)

ce=extract(fit_c)

library(lattice)

require(gridExtra)

densityplot(ce$theta)

layout(t(1:2))

library(hdrcde)
hdr.den(ce$theta)

hdr.den(ce$lp_)


set.seed(1010)
N<-100
y<-rnorm(N,10,2)
NMS_data=list(y=y,x=x,N=N)





NMS = "
data {
    int<lower=0> N;
    real y[N];
}
parameters {
    real<lower=0> mu;
    real<lower=0> sigma;
}
model {
    mu ~ uniform(0, 100);
    sigma ~ uniform(0, 100);
    for (n in 1:N)
    y[n] ~ normal(mu,sigma);
}
"

fit_NMS <- stan(
  model_code = NMS, data = NMS_data, chains = 2,
  warmup = 1000, iter = 2000, refresh = 1000
)



print(fit_NMS)


nms=e=extract(fit_NMS)
layout(t(1:2))
library(hdrcdc)
hdr.den(nmse$mu)
hdr.den(nmse$sigma)





set.seed(1010)
N=30
x=seq(0,1,length=N)
y=6-2*x+rnorm(N)
lms_data=list(y=y,x=x,N=N)



lms = "
data {
    int<lower=0> N;
    vector[N] x;
    vector[N] y;
}
parameters {
    real a;
    real b;
    real<lower=0> tau;
}
transformed parameters {
    real sigma = 1 / tau;
}
model {
    a ~ normal(0, 100);
    b ~ normal(0, 100);
    tau ~ gamma(0.1, 0.1);
    y ~ normal(a + b * x, sigma);
}
"

fit_NMS <- stan(
  model_code = lms, data = lms_data, chains = 2,
  warmup = 1000, iter = 2000, refresh = 1000
)


print(fit_NMS)

pairs(fit_NMS,pars="sigma",include=F)

traceplot(fit_NMS,inc_warmup=TRUE,nrow=2)



#8.1.4


w = read.csv("HTWI.csv")
data = list(N = nrow(w), X = cbind(1, w[,-1]), K = 3,
            y = w$male, beta_loc = rep(0, 3), beta_scale = rep(100, 3))

logit = "
data {
  int N;
  int y[N];
  int K;
  matrix[N, K] X;
  vector[K] beta_loc;
  vector[K] beta_scale;
}
parameters {
  vector[K] beta;
}
transformed parameters {
  vector[N] eta;
  eta = X * beta;
}
model {
  beta ~ normal(beta_loc, beta_scale);
  y ~ bernoulli_logit(eta);
}
generated quantities {
  vector[N] log_lik;
  vector[N] mu;
  for (i in 1:N) {
    mu[i]=inv_logit(eta[i]);
    log_lik[i]=bernoulli_logit_lpmf(y[i]|eta[i]);
  }
}
"


library(rstan)

fit_log = stan(
  model_code = logit, data = w_data, chains = 2, warmup = 1000,
  iter = 2000, cores = 2, refresh = 1000
)

print(fit_log, pars = c('beta'))

pairs(fit_log, pars = "beta")

traceplot(fit_log, pars = "beta", inc_warmup = TRUE, nrow = 1)


#8.2.1

logitb2 = "data {
    int N;
    int y[N];
    int n[N];
}
parameters {
    real theta;
}
transformed parameters {
    real odds;
    odds = theta / (1 - theta);
}
model {
    theta ~ beta(l, 1);
    y ~ binomial(n, theta);
}
"

fit_log2 = stan(
  model_code = logitb2, data = data_list2, chains = 2, warmup = 1000,
  iter = 5000, cores = 2, refresh = 1000
)

rat = read.csv("rat.csv")

fit_log2 = stan(
  model_code = logitb2, data = data_list2, chains = 2, warmup = 1000,
  iter = 5000, cores = 2, refresh = 1000,
  iter=5000,cores=2,refresh=1000
)

fit_log2


library(lattice)
require(gridExtra)
dev.off()
pl = plot(fit_log2)
p2 = stan_trace(fit_log2)
grid.arrange(pl, p2, ncol = 2)

ef = extract(fit_log2)
g1 = densityplot(ef$theta)
g2 = densityplot(ef$odds)
grid.arrange(g1, g2, ncol = 2)



#8.3.1

library(rstan)

model_p <- "
data {
    int n;
    int y[n];
}
parameters {
    real theta;
}
model {
    theta ~ gamma(1, 1);
    y ~ poisson(theta);
}
"

y = rep(0, 9)

data_list <- list(y = y, n = length(y))

stan_samples <- stan(model_code = model_p, data = data_list)

stan_samples

library(lattice)
require(gridExtra)
p1 = plot(stan_samples)
p2 = stan_trace(stan_samples)
grid.arrange(p1, p2, ncol = 2)

ef = extract(stan_samples)
densityplot(ef$theta)
library(hdrcdc)
hdr.den(ef$theta)


#8.4.1

stanmodelcode = "
data {
    int<lower=1> N;
    vector[N] y;
}
parameters {
    real mu;
    real sigma;
}
model {
    mu ~ normal(0, 1000);
    sigma ~ uniform(0, 40);
    y[N] ~ normal(mu, sigma);
}
"

w = read.csv("THM.csv")
N = nrow(w)
y = w[, 2]
dat = list(N = N, y = y)


library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

fit = stan(model_code = stanmodelcode,
           data = dat,
           iter = 10000,
           control = list(max_treedepth = 15,
                          adapt_delta = 0.95),
           chains = 2)

summary(fit)


plot(fit, plotFun = "hist")

library(lattice)
require(gridExtra)
ef = extract(fit)
g1 = densityplot(ef$mu)
g2 = densityplot(ef$sigma)
grid.arrange(g1, g2, ncol = 2)
