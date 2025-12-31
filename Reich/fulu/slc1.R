library(rstan)
library(tidyverse)
library(triangle)

N<-18
y<-c(rep(1,12),rep(0,6))
stan_data<-list(N=N,y=y)
stan_data

bin_unif_model<-
  '
  data{
  int<lower=0>N;
  int<lower=0,upper=1>y[N];
    
  }
  parameters{
  real<lower=0,upper=1>theta;
  }
  model{
  theta~uniform(0,1);
  y~bernoulli(theta);
  }
  
  '

bin_unif<-stan_model(model_code = bin_unif_model)

set.seed(42)
fit_bin_unif<-sampling(bin_unif,data=stan_data)

fit_bin_unif




#################3

bin_norm1_model<-
  '
  data{
  int<lower=0>N;
  int<lower=0,upper=1>y[N];
    
  }
  parameters{
  real<lower=0,upper=1>thete;
  }
  model{
  theta~uniform(0.3,0.1);
  y~bernoulli(theta);
  }
  
  '


bin_norm1<-stan_model(model_code = bin_norm1_model)

set.seed(42)
fit_bin_norm1<-sampling(bin_norm1,data=stan_data)

fit_bin_norm1


###############33

programs<-read_csv("program_level_data.csv")

programs

N<-NROW(patoents)
K<-NROW(programs)

score_discharge<-patients$score_discharge
program<-patients$program_id
score_admit<-patients$score_admit -
  mean(patients$score_admit)

program_type<-
  ifelse(programs$program_type =="A",0,1)

score_data<-
  list(N=N,
       K=K,
       score_discharge=score_discharge,
       program=program,
       score_admit=score_admit,
       program_type=program_type)

score_model_data<-
  '
data{
int<lower=0>N;
int<lower=0>K;
int<lower=1,upper=K>program[N];

vector[N]score_discharge;
vector[N]score_admit;
vector<lower=0,upper=1>[K]program_type;

}
'

score_model_params<-
  '
parameters{
vector[2]gamma_a;
vector[2]gamma_b;
real>lower=0>sigma_a;
real>lower=0>sigma_b;

vector[K]a;

vector[K]b;

real<lower=0>sigma_score;

}
'


score_model_model<-
  '
model{
gamma_a[1]~normal(112,64);
gamma_a[2]~normal(0,64);
gamma_b[1]~normal(0,2);
gamma_b[2]~normal(0,1);

sigma_a~normal(0,50)T[0,];
sigma_b~normal(0,50)T[0,];

a~normal(gamma_a[1]+gamma_a[2]*program_type,sigma_a);
b~normal(gamma_b[1]+gamma_b[2]*program_type,sigma_b);

sigma_score~naomal(0,50)T[0,];

for(i in 1:N)
{
score_discharge[i]~normal(a[program[i]]+
                          b[program[i]]*score_admit[i],
                          sigma_score);

}

}
'

score_model<-paste(score_model_data,
                   score_model_params,
                   score_model_model)

set.seed(42)
score_fit<-sampling(score_stan,
                    data=score_stan)

score_fit
