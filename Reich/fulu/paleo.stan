//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data { #specify all data feeding in to the model
  int<lower=0> n;
  vector[n] mass;
  vector[n] age;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters { #specify all model parameters
  real beta1;
  real beta2;
  real <lower=0> sigma;
}

transformed parameters{ #specify all func / calculate of core parameters , that u want sampled as well 

}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {  #the likelihood and any priors
  vector[n] mu;
  beta1 ~ normal(0,1000000);
  beta2 ~ normal(0,1000000);
  sigma ~ cauchy(0.0,1000);
  mu = beta1 + beta2 * age;
  mass ~ normal(mu,sigma);
}





