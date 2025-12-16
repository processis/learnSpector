# Load packages
library(bayesrules)
library(tidyverse)
library(bayesplot)
library(rstanarm)
library(tidybayes)
library(broom.mixed)
library(janitor)



# Import, rename, & clean data
data(climbers_sub)
climbers <- climbers_sub %>%
  select(expedition_id, member_id, success, year, season,
         age, expedition_role, oxygen_used)


nrow(climbers)

climbers %>%
  tabyl(success)

# Size per expedition
climbers_per_expedition <- climbers %>%
  group_by(expedition_id) %>%
  summarize(count = n())
# Number of expeditions
nrow(climbers_per_expedition)

climbers_per_expedition %>%
  head(3)

# Calculate the success rate for each exhibition
expedition_success <- climbers %>%
  group_by(expedition_id) %>%
  summarize(success_rate = mean(success))
# Plot the success rates across exhibitions
ggplot(expedition_success, aes(x = success_rate)) +
  geom_histogram(color = "white")

# Calculate the success rate by age and oxygen use
data_by_age_oxygen <- climbers %>%
  group_by(age, oxygen_used) %>%
  summarize(success_rate = mean(success))
# Plot this relationship
ggplot(data_by_age_oxygen, aes(x = age, y = success_rate,
                               color = oxygen_used)) +
  geom_point()

climb_model <- stan_glmer(
  success ~ age + oxygen_used + (1 | expedition_id),
  data = climbers, family = binomial,
  prior_intercept = normal(0, 2.5, autoscale = TRUE),
  prior = normal(0, 2.5, autoscale = TRUE),
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 4, iter = 5000*2, seed = 84735
)

# Confirm prior specifications
prior_summary(climb_model)
# MCMC diagnostics
mcmc_trace(climb_model, size = 0.1)
mcmc_dens_overlay(climb_model)
mcmc_acf(climb_model)
neff_ratio(climb_model)
rhat(climb_model)

# Define success rate function
success_rate <- function(x){mean(x == 1)}
# Posterior predictive check
pp_check(climb_model, nreps = 100,
         plotfun = "stat", stat = "success_rate") +
  xlab("success rate")

tidy(climb_model, effects = "fixed", conf.int = TRUE, conf.level = 0.80)

climbers %>%
  add_fitted_draws(climb_model, n = 100, re_formula = NA) %>%
  ggplot(aes(x = age, y = success, color = oxygen_used)) +
  geom_line(aes(y = .value, group = paste(oxygen_used, .draw)),
            alpha = 0.1) +
  labs(y = "probability of success")

# New expedition
new_expedition <- data.frame(
  age = c(20, 20, 60, 60), oxygen_used = c(FALSE, TRUE, FALSE, TRUE),
  expedition_id = rep("new", 4))
new_expedition


# Posterior predictions of binary outcome
set.seed(84735)
binary_prediction <- posterior_predict(climb_model, newdata = new_expedition)
# First 3 prediction sets
head(binary_prediction, 3)

# Summarize the posterior predictions of Y
colMeans(binary_prediction)

set.seed(84735)
classification_summary(data = climbers, model = climb_model, cutoff = 0.5)

set.seed(84735)
classification_summary(data = climbers, model = climb_model, cutoff = 0.65)

#18.2

# Load data
data(airbnb)
# Number of listings
nrow(airbnb)

# Number of neighborhoods
airbnb %>%
  summarize(nlevels(neighborhood))
nlevels(neighborhood)

ggplot(airbnb, aes(x = reviews)) +
  geom_histogram(color = "white", breaks = seq(0, 200, by = 10))
ggplot(airbnb, aes(y = reviews, x = rating)) +
  geom_jitter()
ggplot(airbnb, aes(y = reviews, x = room_type)) +
  geom_violin()

airbnb %>%
  filter(neighborhood %in%
           c("Albany Park", "East Garfield Park", "The Loop")) %>%
  ggplot(aes(y = reviews, x = rating, color = room_type)) +
  geom_jitter() +
  facet_wrap(~ neighborhood)

airbnb_model_1 <- stan_glmer(
  reviews ~ rating + room_type + (1 | neighborhood),
  data = airbnb, family = poisson,
  prior_intercept = normal(3, 2.5, autoscale = TRUE),
  prior = normal(0, 2.5, autoscale = TRUE),
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 4, iter = 5000*2, seed = 84735
)

  pp_check(airbnb_model_1) +
  xlim(0, 200) +
  xlab("reviews")

airbnb_model_2 <- stan_glmer(
  reviews ~ rating + room_type + (1 | neighborhood),
  data = airbnb, family = neg_binomial_2,
  prior_intercept = normal(3, 2.5, autoscale = TRUE),
  prior = normal(0, 2.5, autoscale = TRUE),
  prior_aux = exponential(1, autoscale = TRUE),
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 4, iter = 5000*2, seed = 84735
)


pp_check(airbnb_model_2) +
  xlim(0, 200) +
  xlab("reviews")

tidy(airbnb_model_2, effects = "fixed", conf.int = TRUE, conf.level = 0.80)

tidy(airbnb_model_2, effects = "ran_vals",
     conf.int = TRUE, conf.level = 0.80) %>%
  select(level, estimate, conf.low, conf.high) %>%
  filter(level %in% c("Albany_Park", "East_Garfield_Park", "The_Loop"))

# Posterior predictions of reviews
set.seed(84735)
predicted_reviews <- posterior_predict(
  airbnb_model_2,
  newdata = data.frame(
    rating = rep(5, 3),
    room_type = rep("Entire home/apt", 3),
    neighborhood = c("Albany Park", "East Garfield Park", "The Loop")))
mcmc_areas(predicted_reviews, prob = 0.8) +
  ggplot2::scale_y_discrete(
    labels = c("Albany Park", "East Garfield Park", "The Loop")) +
  xlim(0, 150) +
  xlab("reviews")

set.seed(84735)
prediction_summary(model = airbnb_model_2, data = airbnb)

