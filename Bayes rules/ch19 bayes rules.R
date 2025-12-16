# Load packages
library(bayesrules)
library(tidyverse)
library(bayesplot)
library(rstanarm)
library(janitor)
library(tidybayes)
library(broom.mixed)

data(airbnb)
# Number of listings
nrow(airbnb)

airbnb %>%
  summarize(nlevels(neighborhood), min(price), max(price))
nlevels(neighborhood) min(price) max(price)


ggplot(airbnb, aes(x = price)) +
  geom_histogram(color = "white", breaks = seq(0, 500, by = 20))
ggplot(airbnb, aes(x = log(price))) +
  geom_histogram(color = "white", binwidth = 0.5)

ggplot(airbnb, aes(y = log(price), x = bedrooms)) +
  geom_jitter()
ggplot(airbnb, aes(y = log(price), x = rating)) +
  geom_jitter()

ggplot(airbnb, aes(y = log(price), x = room_type)) +
  geom_boxplot()

ggplot(airbnb, aes(y = log(price), x = neighborhood)) +
  geom_boxplot() +
  scale_x_discrete(labels = c(1:44))

airbnb_model_1 <- stan_glmer(
  log(price) ~ bedrooms + rating + room_type + (1 | neighborhood),
  data = airbnb, family = gaussian,
  prior_intercept = normal(4.6, 2.5, autoscale = TRUE),
  prior = normal(0, 2.5, autoscale = TRUE),
  prior_aux = exponential(1, autoscale = TRUE),
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 4, iter = 5000*2, seed = 84735
)

prior_summary(airbnb_model_1)

pp_check(airbnb_model_1) +
  labs(title = "airbnb_model_1 of log(price)") +
  xlab("log(price)")

airbnb %>%
  select(price, neighborhood, bedrooms, rating, room_type) %>%
  head(3)

airbnb %>%
  select(price, neighborhood, walk_score, transit_score) %>%
  head(3)

# Calculate mean log(price) by neighborhood
nbhd_features <- airbnb %>%
  group_by(neighborhood, walk_score) %>%
  summarize(mean_log_price = mean(log(price)), n_listings = n()) %>%
  ungroup()
# Plot mean log(price) vs walkability
ggplot(nbhd_features, aes(y = mean_log_price, x = walk_score)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

airbnb_model_2 <- stan_glmer(
  log(price) ~ walk_score + bedrooms + rating + room_type +
    (1 | neighborhood),
  data = airbnb, family = gaussian,
  prior_intercept = normal(4.6, 2.5, autoscale = TRUE),
  prior = normal(0, 2.5, autoscale = TRUE),
  prior_aux = exponential(1, autoscale = TRUE),
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 4, iter = 5000*2, seed = 84735
)
# Don't forget to check the prior specifications!
prior_summary(airbnb_model_2)

# Get relationship summaries for both models
model_1_mean <- tidy(airbnb_model_1, effects = "fixed")
model_2_mean <- tidy(airbnb_model_2, effects = "fixed")
# Combine the summaries for both models
combined_summaries <- model_1_mean %>%
  right_join(., model_2_mean, by = "term",
             suffix = c("_model_1", "_model_2")) %>%
  select(-starts_with("std.error"))
combined_summaries

# Get variance summaries for both models
model_1_var <- tidy(airbnb_model_1, effects = "ran_pars")
model_2_var <- tidy(airbnb_model_2, effects = "ran_pars")
# Combine the summaries for both models
model_1_var %>%
  right_join(., model_2_var, by = "term",
             suffix = c("_model_1", "_model_2")) %>%
  select(-starts_with("group"))

nbhd_features %>%
  filter(neighborhood %in% c("Edgewater", "Pullman"))

combined_summaries %>%
  filter(term %in% c("(Intercept)", "walk_score"))

# Get neighborhood summaries from both models
model_1_nbhd <- tidy(airbnb_model_1, effects = "ran_vals")
model_2_nbhd <- tidy(airbnb_model_2, effects = "ran_vals")
# Combine the summaries for both models
model_1_nbhd %>%
  right_join(., model_2_nbhd, by = "level",
             suffix = c("_model_1", "_model_2")) %>%
  select(-starts_with(c("group", "term", "std.error"))) %>%
  filter(level %in% c("Edgewater", "Pullman"))

nbhd_features %>%
  filter(neighborhood %in% c("Edgewater", "Pullman"))

#19.2

#Import, rename, & clean data
data(climbers_sub)
climbers <- climbers_sub %>%
  select(peak_name, expedition_id, member_id, success,
         year, season, age, expedition_role, oxygen_used)

# Summarize expeditions
expeditions <- climbers %>%
  group_by(peak_name, expedition_id) %>%
  summarize(n_climbers = n())
head(expeditions, 2)

# Summarize peaks
peaks <- expeditions %>%
  group_by(peak_name) %>%
  summarize(n_expeditions = n(), n_climbers = sum(n_climbers))
head(peaks, 2)

# Number of climbers
nrow(climbers)

# Number of expeditions
nrow(expeditions)

# Number of peaks
nrow(peaks)

climbers %>%
  group_by(peak_name) %>%
  summarize(p_success = mean(success)) %>%
  ggplot(., aes(x = p_success)) +
  geom_histogram(color = "white")

climb_model_1 <- stan_glmer(
  success ~ age + oxygen_used + (1 | expedition_id),
  data = climbers, family = binomial,
  prior_intercept = normal(0, 2.5, autoscale = TRUE),
  prior = normal(0, 2.5, autoscale = TRUE),
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 4, iter = 5000*2, seed = 84735
)
climb_model_2 <- stan_glmer(
  success ~ age + oxygen_used + (1 | expedition_id) + (1 | peak_name),
  data = climbers, family = binomial,
  prior_intercept = normal(0, 2.5, autoscale = TRUE),
  prior = normal(0, 2.5, autoscale = TRUE),
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 4, iter = 5000*2, seed = 84735
)

# Get trend summaries for both models
climb_model_1_mean <- tidy(climb_model_1, effects = "fixed")
climb_model_2_mean <- tidy(climb_model_2, effects = "fixed")
# Combine the summaries for both models
climb_model_1_mean %>%
  right_join(., climb_model_2_mean, by ="term",
             suffix = c("_model_1", "_model_2")) %>%
  select(-starts_with("std.error"))

# Get variance summaries for both models
climb_model_1_var <- tidy(climb_model_1, effects = "ran_pars")
climb_model_2_var <- tidy(climb_model_2, effects = "ran_pars")
# Combine the summaries for both models
climb_model_1_var %>%
  right_join(., climb_model_2_var, by = "term",
             suffix =c("_model_1", "_model_2")) %>%
  select(-starts_with("group"))

# Global regression parameters
climb_model_2_mean %>%
  select(term, estimate)

# Group-level terms
group_levels_2 <- tidy(climb_model_2, effects = "ran_vals") %>%
  select(level, group, estimate)

group_levels_2 %>%
  filter(group == "peak_name") %>%
  head(2)

group_levels_2 %>%
  filter(group == "expedition_id") %>%
  head(2)

#19.3.2

data(spotify)
spotify_small <- spotify %>%
  filter(artist %in% c("Beyoncé", "Camila Cabello", "Camilo",
                       "Frank Ocean", "J. Cole", "Kendrick Lamar")) %>%
  select(artist, album_id, popularity, valence)


#19.8

# Load & process the data
data("big_word_club")
bwc <- big_word_club %>%
  mutate(treat = as.factor(treat))