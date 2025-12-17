# Load required packages
library(rstan)
library(ggplot2)
library(bayesplot)

# Set Stan options for better performance
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Simulate example data
set.seed(123)
N <- 50  # number of observations
true_theta <- 0.7  # true probability of success

# Generate data from binomial distribution
n_trials <- sample(10:30, N, replace = TRUE)  # varying trial sizes
y_success <- rbinom(N, n_trials, true_theta)

# Prepare data for Stan
stan_data <- list(
  N = N,
  y = y_success,
  n = n_trials
)

# Compile and run the model
model_fit <- stan(
  file = "beta_binomial.stan",  # path to your .stan file
  data = stan_data,
  iter = 2000,          # total iterations per chain
  warmup = 1000,        # warmup iterations
  chains = 4,           # number of Markov chains
  thin = 1,            # thinning rate
  seed = 123,
  control = list(adapt_delta = 0.9)
)

# Alternative: Specify model directly in R code
model_code <- "
data {
  int<lower=0> N;
  int<lower=0> y[N];
  int<lower=0> n[N];
}
parameters {
  real<lower=0, upper=1> theta;
  real<lower=0> alpha;
  real<lower=0> beta;
}
model {
  // Priors
  alpha ~ gamma(1, 0.1);
  beta ~ gamma(1, 0.1);
  theta ~ beta(alpha, beta);
  y ~ binomial(n, theta);
}
generated quantities {
  int y_rep[N];
  for (i in 1:N) {
    y_rep[i] = binomial_rng(n[i], theta);
  }
}
"

# Run with model code directly
model_fit <- stan(
  model_code = model_code,
  data = stan_data,
  iter = 2000,
  warmup = 1000,
  chains = 4,
  seed = 123
)

## Model Diagnostics and Results

# Print summary
print(model_fit, pars = c("theta", "alpha", "beta"))

# Trace plots
traceplot(model_fit, pars = c("theta", "alpha", "beta"))

# Posterior density plots
plot(model_fit, pars = c("theta", "alpha", "beta"))

# Extract posterior samples
posterior_samples <- extract(model_fit)

# Check posterior of theta
cat("\nTrue theta:", true_theta, "\n")
cat("Posterior mean of theta:", mean(posterior_samples$theta), "\n")
cat("95% credible interval for theta:", 
    quantile(posterior_samples$theta, c(0.025, 0.975)), "\n")

# Posterior predictive checks
y_rep <- posterior_samples$y_rep

# Compare observed vs replicated data
ppc_stat(y_success, y_rep, stat = "mean")
ppc_stat(y_success, y_rep, stat = "sd")

# Plot observed vs predicted successes
par(mfrow = c(1, 2))
hist(y_success, main = "Observed Successes", xlab = "Successes")
hist(apply(y_rep, 2, mean), main = "Predicted Successes", xlab = "Successes")

## Simplified Version (if you want fixed alpha and beta)

simple_model_code <- "
data {
  int<lower=0> N;
  int<lower=0> y[N];
  int<lower=0> n[N];
}
parameters {
  real<lower=0, upper=1> theta;
}
model {
  // Prior for theta (Beta(2,2) - weakly informative)
  theta ~ beta(2, 2);
  
  // Likelihood
  y ~ binomial(n, theta);
}
generated quantities {
  // Posterior predictive distribution
  real<lower=0, upper=1> theta_pred;
  theta_pred = beta_rng(sum(y) + 2, sum(n) - sum(y) + 2);
}
"

# Run simplified model
simple_fit <- stan(
  model_code = simple_model_code,
  data = stan_data,
  iter = 2000,
  warmup = 1000,
  chains = 4,
  seed = 123
)

print(simple_fit, pars = "theta")

## Plotting results
# Extract theta samples
theta_samples <- extract(simple_fit)$theta

# Create posterior density plot
ggplot(data.frame(theta = theta_samples), aes(x = theta)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  geom_vline(xintercept = true_theta, color = "red", linetype = "dashed") +
  labs(title = "Posterior Distribution of Theta",
       x = "Probability of Success (theta)",
       y = "Density") +
  theme_minimal()



##############


data {
  int<lower=0> N;           # number of observations
  int<lower=0> y[N];        #successes
  int<lower=0> n[N];        # trials
}

parameters {
  real<lower=0, upper=1> theta;  # probability of success
  real<lower=0> alpha;           # prior alpha
  real<lower=0> beta;           #prior beta
}

model {
  # Priors
  alpha ~ gamma(1, 0.1);  # weakly informative prior
  beta ~ gamma(1, 0.1);   # weakly informative prior
  
  #Prior for theta (hyperprior)
  theta ~ beta(alpha, beta);
  
  #Likelihood
  y ~ binomial(n, theta);
}

generated quantities {
  #osterior predictive checks
  int y_rep[N];
  for (i in 1:N) {
    y_rep[i] = binomial_rng(n[i], theta);
  }
}

