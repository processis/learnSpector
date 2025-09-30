# Load some packages
library(bayesrules)
library(rstanarm)
library(bayesplot)
library(tidyverse)
library(broom.mixed)
library(tidybayes)
# Load the data
data(weather_WU)
weather_WU %>%
  group_by(location) %>%
  tally()


weather_WU <- weather_WU %>%
  select(location, windspeed9am, humidity9am, pressure9am, temp9am, temp3pm)


ggplot(weather_WU, aes(x = temp9am, y = temp3pm)) +
  geom_point(size = 0.2)


weather_model_1 <- stan_glm(
  temp3pm ~ temp9am,
  data = weather_WU, family = gaussian,
  prior_intercept = normal(25, 5),
  prior = normal(0, 2.5, autoscale = TRUE),
  prior_aux = exponential(1, autoscale = TRUE),
  chains = 4, iter = 5000*2, seed = 84735)
# Prior specification
prior_summary(weather_model_1)
# MCMC diagnostics
mcmc_trace(weather_model_1, size = 0.1)
mcmc_dens_overlay(weather_model_1)
mcmc_acf(weather_model_1)
neff_ratio(weather_model_1)
rhat(weather_model_1)


# Posterior credible intervals
posterior_interval(weather_model_1, prob = 0.80)

pp_check(weather_model_1)

ggplot(weather_WU, aes(x = temp3pm, fill = location)) +
  geom_density(alpha = 0.5)

weather_model_2 <- stan_glm(
  temp3pm ~ location,
  data = weather_WU, family = gaussian,
  prior_intercept = normal(25, 5),
  prior = normal(0, 2.5, autoscale = TRUE),
  prior_aux = exponential(1, autoscale = TRUE),
  chains = 4, iter = 5000*2, seed = 84735)


# MCMC diagnostics
mcmc_trace(weather_model_2, size = 0.1)
mcmc_dens_overlay(weather_model_2)
mcmc_acf(weather_model_2)


# Posterior summary statistics
tidy(weather_model_2, effects = c("fixed", "aux"),
     conf.int = TRUE, conf.level = 0.80) %>%
  select(-std.error)

as.data.frame(weather_model_2) %>%
  mutate(uluru = `(Intercept)`,
         wollongong = `(Intercept)` + locationWollongong) %>%
  mcmc_areas(pars = c("uluru", "wollongong"))

ggplot(weather_WU, aes(y = temp3pm, x = temp9am, color = location)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


weather_model_3_prior <- stan_glm(
  temp3pm ~ temp9am + location,
  data = weather_WU, family = gaussian,
  prior_intercept = normal(25, 5),
  prior = normal(0, 2.5, autoscale = TRUE),
  prior_aux = exponential(1, autoscale = TRUE),
  chains = 4, iter = 5000*2, seed = 84735,
  prior_PD = TRUE)





set.seed(84735)
weather_WU %>%
  add_predicted_draws(weather_model_3_prior, n = 100) %>%
  ggplot(aes(x = .prediction, group = .draw)) +
  geom_density() +
  xlab("temp3pm")
weather_WU %>%
  add_fitted_draws(weather_model_3_prior, n = 100) %>%
  ggplot(aes(x = temp9am, y = temp3pm, color = location)) +
  geom_line(aes(y = .value, group = paste(location, .draw)))


weather_model_3 <- update(weather_model_3_prior, prior_PD = FALSE)

head(as.data.frame(weather_model_3), 3)

weather_WU %>%
  add_fitted_draws(weather_model_3, n = 100) %>%
  ggplot(aes(x = temp9am, y = temp3pm, color = location)) +
  geom_line(aes(y = .value, group = paste(location, .draw)), alpha = .1) +
  geom_point(data = weather_WU, size = 0.1)


# Posterior summaries
posterior_interval(weather_model_3, prob = 0.80,
                   pars = c("temp9am", "locationWollongong"))

# Simulate a set of predictions
set.seed(84735)
temp3pm_prediction <- posterior_predict(
  weather_model_3,
  newdata = data.frame(temp9am = c(10, 10),
                       location = c("Uluru", "Wollongong")))
# Plot the posterior predictive models
mcmc_areas(temp3pm_prediction) +
  ggplot2::scale_y_discrete(labels = c("Uluru", "Wollongong")) +
  xlab("temp3pm")

ggplot(weather_WU, aes(y = temp3pm, x = humidity9am, color = location)) +
  geom_point(size = 0.5) +
  geom_smooth(method = "lm", se = FALSE)

interaction_model <- stan_glm(
  temp3pm ~ location + humidity9am + location:humidity9am,
  data = weather_WU, family = gaussian,
  prior_intercept = normal(25, 5),
  prior = normal(0, 2.5, autoscale = TRUE),
  prior_aux = exponential(1, autoscale = TRUE),
  chains = 4, iter = 5000*2, seed = 84735)

# Posterior summary statistics
tidy(interaction_model, effects = c("fixed", "aux"))

weather_WU %>%
  add_fitted_draws(interaction_model, n = 200) %>%
  ggplot(aes(x = humidity9am, y = temp3pm, color = location)) +
  geom_line(aes(y = .value, group = paste(location, .draw)), alpha = 0.1)

data(bike_users)
bike_users %>%
  group_by(user) %>%
  tally()

bike_casual <- bike_users %>%
  filter(user == "casual")
bike_registered <- bike_users %>%
  filter(user == "registered")

ggplot(bike_casual, aes(y = rides, x = temp_actual, color = weekend)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "casual riders")

ggplot(bike_casual, aes(y = rides, x = temp_actual, color = humidity)) +
  geom_point()
ggplot(bike_casual,
       aes(y = rides, x = temp_actual,
           color = cut(humidity, 2, labels = c("low","high")))) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(color = "humidity_level") +
  lims(y = c(0, 2500))


# Example syntax
ggplot(bike_users, aes(y = rides, x = user, fill = weather_cat)) +
  geom_boxplot()

weather_WU %>%
  names()


weather_model_4 <- stan_glm(
  temp3pm ~ .,
  data = weather_WU, family = gaussian,
  prior_intercept = normal(25, 5),
  prior = normal(0, 2.5, autoscale = TRUE),
  prior_aux = exponential(1, autoscale = TRUE),
  chains = 4, iter = 5000*2, seed = 84735)
# Confirm prior specification
prior_summary(weather_model_4)
# Check MCMC diagnostics
mcmc_trace(weather_model_4)
mcmc_dens_overlay(weather_model_4)
mcmc_acf(weather_model_4)


# Posterior summaries
posterior_interval(weather_model_4, prob = 0.95)

# Posterior predictive checks. For example:
pp_check(weather_model_1)


set.seed(84735)
predictions_1 <- posterior_predict(weather_model_1, newdata = weather_WU)

# Posterior predictive models for weather_model_1
ppc_intervals(weather_WU$temp3pm, yrep = predictions_1,
              x = weather_WU$temp9am, prob = 0.5, prob_outer = 0.95) +
  labs(x = "temp9am", y = "temp3pm")
# Posterior predictive models for weather_model_2
ppc_violin_grouped(weather_WU$temp3pm, yrep = predictions_2,
                   group = weather_WU$location, y_draw = "points") +
  labs(y = "temp3pm")


# Posterior predictive models for weather_model_3
ppc_intervals_grouped(weather_WU$temp3pm, yrep = predictions_3,
                      x = weather_WU$temp9am, group = weather_WU$location,
                      prob = 0.5, prob_outer = 0.95,
                      facet_args = list(scales = "fixed")) +
  labs(x = "temp9am", y = "temp3pm")



# Calculate ELPD for the 4 models
set.seed(84735)
loo_1 <- loo(weather_model_1)
loo_2 <- loo(weather_model_2)
loo_3 <- loo(weather_model_3)
loo_4 <- loo(weather_model_4)
# Results
c(loo_1$estimates[1], loo_2$estimates[1],
  loo_3$estimates[1], loo_4$estimates[1])


# Compare the ELPD for the 4 models
loo_compare(loo_1, loo_2, loo_3, loo_4)


# Take 2 separate samples
set.seed(84735)
weather_shuffle <- weather_australia %>%
  filter(temp3pm < 30, location == "Wollongong") %>%
  sample_n(nrow(.))
sample_1 <- weather_shuffle %>% head(40)
sample_2 <- weather_shuffle %>% tail(40)


# Save the plot for later
g <- ggplot(sample_1, aes(y = temp3pm, x = day_of_year)) +
  geom_point()
g


g + geom_smooth(method = "lm", se = FALSE)
g + stat_smooth(method = "lm", se = FALSE, formula = y ~ poly(x, 2))
g + stat_smooth(method = "lm", se = FALSE, formula = y ~ poly(x, 12))


model_1 <- stan_glm(
  temp3pm ~ day_of_year,
  data = sample_1, family = gaussian,
  prior_intercept = normal(25, 5),
  prior = normal(0, 2.5, autoscale = TRUE),
  prior_aux = exponential(1, autoscale = TRUE),
  chains = 4, iter = 5000*2, seed = 84735)
# Ditto the syntax for models 2 and 3
model_2 <- stan_glm(temp3pm ~ poly(day_of_year, 2), ...)
model_3 <- stan_glm(temp3pm ~ poly(day_of_year, 12), ...)


set.seed(84735)
prediction_summary(model = model_1, data = sample_1)
prediction_summary_cv(model = model_1, data = sample_1, k = 10)$cv



























































































































