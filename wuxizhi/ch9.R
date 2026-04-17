MM = "
data { 
    int<lower=1> N; // total number of observations
    vector[N] Y; // response variable
    int<lower=1> K; // number of population-level effects
    matrix[N, K] X; // population-level design matrix
}
transformed data {
    real mean_Y = mean(Y);
    real sd_Y = sd(Y);
}
parameters {
    real m;
    real lam;
    real lambda;
    vector[K] b; // population-level effects
    real Intercept; // intercept
}
model {
    vector[N] mu = Intercept + X * b;
    m ~ normal(0, 10);
    lam ~ uniform(0, 100);
    target += gamma_lpdf(lambda | 1, 0.1);
    for(i in 1:K){
        target += student_t_lpdf(b[i] | 1, m, lambda);
    };
    Intercept ~ normal(mean_Y, sd_Y);
    target += student_t_lpdf(Y | 1.5, mu, lam);
}
"

library(rstan)
w = read.csv("Anscombe.csv")
data = list(N = nrow(w), Y = w[,1], K = ncol(w) - 1, X = w[,-1])
fit1 <- stan(
  model_code = MM,
  data = data,
  chains = 4,
  warmup=1000,
  iter=20000,
  cores=4,
  refresh=1000
)

print(fit1)



#9.3.1

ML="
data {
  // 输入的数据
  int N;
  int y[N];
  int K;
  matrix[N, K] X;
  // 先验参数值
  real alpha_loc;
  real alpha_scale;
  vector[K] beta_loc;
  vector[K] beta_scale;
}
parameters {
  real alpha;
  vector[K] beta;
}
transformed parameters {
  // linear predictor
  vector[N] eta;
  eta = alpha + X * beta;
}
model {
  alpha ~ normal(alpha_loc, alpha_scale);
  beta ~ normal(beta_loc, beta_scale);
  // y ~ bernoulli(inv_logit(eta)); 和下面等价，但慢些
  y ~ bernoulli_logit(eta);
}
generated quantities {
  // 每个观测值的对数似然
  vector[N] log_lik;
  // 概率
  vector[N] mu;
  for (i in 1:N) {
    mu[i] = inv_logit(eta[i]);
    log_lik[i] = bernoulli_logit_mpf(y[i] | eta[i]);
  }
}
"


u = read.csv("Sports.csv") # Label~PRP+VBN

u_data = list(
  N = nrow(u),
  X = u[, c(22,34)],
  K = 2,
  y = (u$Label == "subjective") + 1,  # 换成哑元
  alpha_loc = -1.5,
  alpha_scale = 2,
  beta_loc = rep(0, 2),
  beta_scale = rep(0.5, 2)
)

library(rstan)

fit2 <- stan(
  model_code = ML,
  data = u_data,
  chains = 4,
  warmup = 1000,
  iter = 2000,
  cores = 2,
  refresh = 1000
)

print(fit2, pars = c("alpha", "beta[1]", "beta[2]"))

plot(fit2, pars = c("beta[1]", "beta[2]"))

traceplot(fit2, inc_warmup = TRUE, pars = c("alpha", "beta[1]", "beta[2]"))


pairs(fits,inc_warmup=TRUE,pars=c("alpha","beta[1]","beta[2]"))



#9.4.1

XX="

data {
  int N;
  int J;
  int K;
  int id[N];
  matrix[N,K] X;
  vector[N] Y;
}
parameters {
  vector[K] gamma;
  vector[K] tau;
  vector[K] beta[J];
  real sigma;
}
model {
  vector[N] mu; //linear predictor
  //priors
  gamma ~ normal(0,5);
  tau ~ cauchy(0,5);
  sigma ~ cauchy(0,5);
  
  for(j in 1:J){
    beta[j] ~ normal(gamma,tau);
  }
  
  for(n in 1:N){
    mu[n] = X[n] * beta[id[n]];
  }
  
  //likelihood
  y ~ normal(mu,sigma);
}
"


library(rstan)
w = read.csv("sleepstudy.csv")
ss = list(N = nrow(w), J = 18, K = 2, id = rep(1:18, each = 10),
          X = cbind(1, w[,2]), y = w[,1])
m_s0 <- stan(model_code = XX, data = ss, chains = 2)

summary(m_s0)

plot(m_s0, pars = c("beta[1,1]", "beta[1,2]", "beta[2,1]", "beta[2,2]"))

traceplot(m_s0, inc_warmup = TRUE, nrow = 2)

pairs(m_s0, inc_warmup = TRUE, pars = c("gamma[1]", "tau[1]", "beta[1,1]", "beta[1,2]"))


#9.5.1


w = read.csv("cbpp.csv")
w[,4] = factor(w[,4])
w1 = data.frame(model.matrix(~ -1, w))

MLI = "
data {
  int N; //the number of observations (N=nrow(w1)=55)
  int J; //the number of groups (J=length(unique(w1$herd))=15)
  int K; //number of columns in the model matrix (K=4 (包括 1))
  int id[N]; //vector of group indeces (w1$herd)
  matrix[N,K] X; //the model matrix (56,4: w1[,c(4:7)])
  int y[N]; //the response variable (w1[,2]或w$incidence)
  int n[N]; //w1$size
}

parameters {
  real mu; //population-level regression coefficients
  real tau; //the standard deviation of the regression coefficient


vector[K] beta[J]; //matrix of group-level regression coefficients
}

model {
  vector[N] eta; //linear predictor
  //priors
  mu ~ student_t(3, 0, 1);
  tau ~ normal(0,1);

  //priors <- c(set_prior("normal(0,1)", class = "Intercept"),
  //set_prior("normal(0,.5)", class = "b", coef = " ",lb=0))

  for(j in 1:J){
    beta[j] ~ normal(mu,tau);
  }

  for (i in 1:N) {
    eta[i] = inv_logit(X[i] * beta[id[i]]);
  }
  
  //likehood binomial_lpmf(ints n|ints N,reals theta)
  //y~binomial(n,eta);
  
  for(i in 1:N){
  target +=binomial_lpmf(y[i]|n[i],eta[i]);
  }
}
"

library(rstan)

df=list(N=nrow(w1),J=15,K=4,id=w1$herd,X=w1[,4:7],y=w1[,2],n=w1$size)

mll0<-stan(model_code=MLL.data=df,chains=2)

summary(mll0)

plot(mll0,pars=c("beta[1,1","beta[1,2]","beta[2,1]","beta[2,2]"))

pairs(mll0,inc_warmup=TRUE,pars=c("mu","tau","beta[1,1","beta[1,2]"))