#可以正常跑通

#Load packages
library(bayesrules)
library(tidyverse)
library(bayesplot)
library(rstanarm)

ggplot(bikes, aes(y = rides, x = temp_feel)) +
  geom_point(size = 0.2) +
  geom_smooth(method = "lm", se = FALSE)



bike_model <- stan_glm(rides ~ temp_feel, data = bikes,
                       family = gaussian,
                       prior_intercept = normal(5000, 1000),
                       prior = normal(100, 40),
                       prior_aux = exponential(0.0008),
                       chains = 4, iter = 5000*2, seed = 84735)




neff_ratio(bike_model)

rhat(bike_model)

# Trace plots of parallel chains
mcmc_trace(bike_model, size = 0.1)
# Density plots of parallel chains
mcmc_dens_overlay(bike_model)



# Store the 4 chains for each parameter in 1 data frame
bike_model_df <- as.data.frame(bike_model)




first_set <- head(bike_model_df, 1)
first_set

beta_0 <- first_set$`(Intercept)`
beta_1 <- first_set$temp_feel
sigma <- first_set$sigma
set.seed(84735)
one_simulation <- bikes %>%
  mutate(mu = beta_0 + beta_1 * temp_feel,
         simulated_rides = rnorm(500, mean = mu, sd = sigma)) %>%
  select(temp_feel, rides, simulated_rides)


head(one_simulation, 2)

ggplot(one_simulation, aes(x = simulated_rides)) +
  geom_density(color = "lightblue") +
  geom_density(aes(x = rides), color = "darkblue")


# Examine 50 of the 20000 simulated samples
pp_check(bike_model, nreps = 50) +
  xlab("rides")

bikes %>%
  filter(date == "2012-10-22") %>%
  select(temp_feel, rides)

# Simulate the posterior predictive model
set.seed(84735)
predict_75 <- bike_model_df %>%
  mutate(mu = `(Intercept)` + temp_feel*75,
         y_new = rnorm(20000, mean = mu, sd = sigma))
# Plot the posterior predictive model
ggplot(predict_75, aes(x = y_new)) +
  geom_density()


predict_75 %>%
  summarize(mean = mean(y_new), error = 6228 - mean(y_new))


predict_75 %>%
  summarize(sd = sd(y_new), error = 6228 - mean(y_new),
            error_scaled = error / sd(y_new))

predict_75 %>%
  summarize(lower_95 = quantile(y_new, 0.025),
            lower_50 = quantile(y_new, 0.25),
            upper_50 = quantile(y_new, 0.75),
            upper_95 = quantile(y_new, 0.975))


set.seed(84735)
predictions <- posterior_predict(bike_model, newdata = bikes)
dim(predictions)


# Posterior predictive summaries
set.seed(84735)
prediction_summary(bike_model, data = bikes)

set.seed(84735)
cv_procedure <- prediction_summary_cv(
  model = bike_model, data = bikes, k = 10)

cv_procedure$folds


cv_procedure$cv

# Posterior predictive summaries for original data
set.seed(84735)
prediction_summary(bike_model, data = bikes)


model_elpd <- loo(bike_model)
model_elpd$estimates

library(bayesrules)
data("coffee_ratings")
coffee_ratings <- coffee_ratings %>%
  select(farm_name, total_cup_points, aroma, aftertaste)

set.seed(84735)
new_coffee <- coffee_ratings %>%
  group_by(farm_name) %>%
  sample_n(1) %>%
  ungroup()
dim(new_coffee)



































