SMC_model = "
data {
  int<lower=0> J;
  real y[J];
  real<lower=0> sigma[J];
}

parameters {
  real mu;
  real<lower=0> tau;
  real theta_tilde[J];
}

transformed parameters {
  real theta[J];
  for (j in 1:J)
    theta[j] = mu + tau * theta_tilde[j];
}


model {
    mu ~ normal(0, 5);
    tau ~ cauchy(0, 5);
    theta_tilde ~ normal(0, 1);
    y ~ normal(theta, sigma);
}"






schools.data <- list(
  J = 8,
  y = c(28.39, 7.94, -2.75, 6.82, -0.64, 0.63, 18.01, 12.16),
  sigma = c(14.9, 10.2, 16.3, 11.0, 9.4, 11.4, 10.4, 17.6)
)

library(rstan)

fit <- stan(
  model_code = SMC_model,
  data = schools.data,
  chains = 2,
  warmup = 1000,
  iter = 2000,
  refresh = 1000
)


print(fit)

print(fit,pars=c("mu","tau"))

plot(fit,plotfun="trace",pars=c("mu","tau","theta"),int_warmup=TRUE,nrow=2)

traceplot(fit,pars=c("mu","tau","theta"),inc_warmup=TRUE,nrow=2)

pairs(fit,pars=c("theta","theta_tilde"),log=TRUE,las=1,include=F)



samples <- extract(fit, permuted = TRUE)

layout(t(1:3))
library(hdrcode)
hdr.den(samples$tau)
hdr.den(samples$mu)
hdr.den(samples$lp)

sampler_params <- get_sampler_params(fit, inc_warmup = FALSE)
sampler_params_chain1 <- sampler_params[[1]]
colnames(sampler_params_chain1)


library(shinystan)
launch_shinystan(fit)


