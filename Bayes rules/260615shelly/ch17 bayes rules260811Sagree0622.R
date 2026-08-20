#有一个^小报错  不影响后面


# Load packages
library(bayesrules)
library(tidyverse)
library(rstanarm)
library(bayesplot)
library(tidybayes)
library(broom.mixed)
# Load data
#data(cherry_blossom_sample)
#running <- cherry_blossom_sample

running<-read_csv("agree0622.csv")

# Remove NAs
running <- running %>%
  select(runner, age, net) %>%
  na.omit()

#17.1

# Posterior summary statistics
model_summary <- tidy(complete_pooled_model,
                      conf.int = TRUE, conf.level = 0.80)
model_summary
# A tibble: 2 x 5



# Posterior median model
B0 <- model_summary$estimate[1]
B1 <- model_summary$estimate[2]
ggplot(running, aes(x = age, y = net)) +
  geom_point() +
  geom_abline(aes(intercept = B0, slope = B1))

#17.2

running_model_1_prior <- stan_glmer(
  net ~ age + (1 | runner),
  data = running, family = gaussian,
  prior_intercept = normal(6.95, 3.01),
  prior = normal(-1.7, 0.52),
  prior_aux = exponential(1, autoscale = TRUE),
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 4, iter = 5000*2, seed = 84735,
  prior_PD = TRUE)



set.seed(84735)
running %>%
  add_fitted_draws(running_model_1_prior, n = 4) %>%
  ggplot(aes(x = age, y = net)) +
  geom_line(aes(y = .value, group = paste(runner, .draw))) +
  facet_wrap(~ .draw)
running %>%
  add_predicted_draws(running_model_1_prior, n = 100) %>%
  ggplot(aes(x = net)) +
  geom_density(aes(x = .prediction, group = .draw)) +
  xlim(-100,300)

ggplot(running, aes(x = age, y = net)) +
  geom_point() +
  facet_wrap(~ runner)


# Simulate the posterior
running_model_1 <- update(running_model_1_prior, prior_PD = FALSE)
# Check the prior specifications
prior_summary(running_model_1)
# Markov chain diagnostics
mcmc_trace(running_model_1)
mcmc_dens_overlay(running_model_1)
mcmc_acf(running_model_1)
neff_ratio(running_model_1)
rhat(running_model_1)


tidy_summary_1 <- tidy(running_model_1, effects = "fixed",
                       conf.int = TRUE, conf.level = 0.80)
tidy_summary_1

B0 <- tidy_summary_1$estimate[1]
B1 <- tidy_summary_1$estimate[2]
running %>%
  add_fitted_draws(running_model_1, n = 200, re_formula = NA) %>%
  ggplot(aes(x = age, y = net)) +
  geom_line(aes(y = .value, group = .draw), alpha = 0.1) +
  geom_abline(intercept = B0, slope = B1, color = "blue") +
  lims(y = c(75, 110))


# Posterior summaries of runner-specific intercepts
runner_summaries_1 <- running_model_1 %>%
  spread_draws(`(Intercept)`, b[,runner]) %>%
  mutate(runner_intercept = `(Intercept)` + b) %>%
  select(-`(Intercept)`, -b) %>%
  median_qi(.width = 0.80) %>%
  select(runner, runner_intercept, .lower, .upper)


runner_summaries_1 %>%
  filter(runner %in% c("runner:4", "runner:5"))


# 100 posterior plausible models for runners 4 & 5
running %>%
  filter(runner %in% c("4", "5")) %>%
  add_fitted_draws(running_model_1, n = 100) %>%
  ggplot(aes(x = age, y = net)) +
  geom_line(
    aes(y = .value, group = paste(runner, .draw), color = runner),
    alpha = 0.1) +
  geom_point(aes(color = runner))


# Plot runner-specific models with the global model
ggplot(running, aes(y = net, x = age, group = runner)) +
  geom_abline(data = runner_summaries_1, color = "gray",
              aes(intercept = runner_intercept, slope = B1)) +
  geom_abline(intercept = B0, slope = B1, color = "blue") +
  lims(x = c(1, 6), y = c(0, 0.8))


tidy_sigma <- tidy(running_model_1, effects = "ran_pars")
tidy_sigma


sigma_0 <- tidy_sigma[1,3]
sigma_y <- tidy_sigma[2,3]
sigma_0^2 / (sigma_0^2 + sigma_y^2)

sigma_y^2 / (sigma_0^2 + sigma_y^2)

#17.3

# Plot runner-specific models in the data
running %>%
  filter(runner %in% c("4", "5", "20", "29")) %>%
  ggplot(., aes(x = age, y = net)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(~ runner)


ggplot(running, aes(x = age, y = net, group = runner)) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5)


running_model_2 <- stan_glmer(
  net ~ age + (age | runner),
  data = running, family = gaussian,
  prior_intercept = normal(100, 10),
  prior = normal(2.5, 1),
  prior_aux = exponential(1, autoscale = TRUE),
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 4, iter = 5000*2, seed = 84735, adapt_delta = 0.99999
)

# Confirm the prior model specifications
prior_summary(running_model_2)


# Quick summary of global regression parameters
tidy(running_model_2, effects = "fixed", conf.int = TRUE, conf.level = 0.80)


# Get MCMC chains for the runner-specific intercepts & slopes
runner_chains_2 <- running_model_2 %>%
  spread_draws(`(Intercept)`, b[term, runner], `age`) %>%
  pivot_wider(names_from = term, names_glue = "b_{term}",
              values_from = b) %>%
  mutate(runner_intercept = `(Intercept)` + `b_(Intercept)`,
         runner_age = age + b_age)


# Posterior medians of runner-specific models
runner_summaries_2 <- runner_chains_2 %>%
  group_by(runner) %>%
  summarize(runner_intercept = median(runner_intercept),
            runner_age = median(runner_age))
# Check it out
head(runner_summaries_2, 3)


ggplot(running, aes(y = net, x = age, group = runner)) +
  geom_abline(data = runner_summaries_2, color = "gray",
              aes(intercept = runner_intercept, slope = runner_age)) +
  lims(x = c(1, 6), y = c(0, 0.8))

runner_summaries_2 %>%
  filter(runner %in% c("runner:1", "runner:10"))


tidy(running_model_2, effects = "ran_pars")


#17.4

pp_check(complete_pooled_model) +
  labs(x = "net", title = "complete pooled model")
pp_check(running_model_1) +
  labs(x = "net", title = "running model 1")
pp_check(running_model_2) +
  labs(x = "net", title = "running model 2")


# Calculate prediction summaries
set.seed(84735)
prediction_summary(model = running_model_1, data = running)

prediction_summary(model = running_model_2, data = running)


prediction_summary_cv(model = running_model_1, data = running,
                      k = 10, group = "runner")



#Calculate ELPD for the 2 models
elpd_hierarchical_1 <- loo(running_model_1)
elpd_hierarchical_2 <- loo(running_model_2)
# Compare the ELPD
loo_compare(elpd_hierarchical_1, elpd_hierarchical_2)


#17.5

# Plot runner-specific trends for runners 1 & 10
running %>%
  filter(runner %in% c("6", "8")) %>%
  ggplot(., aes(x = age, y = net)) +
  geom_point() +
  facet_grid(~ runner) +
  lims(x = c(1, 8))

# Simulate posterior predictive models for the 3 runners
set.seed(84735)
predict_next_race <- posterior_predict(
  running_model_1,
  newdata = data.frame(runner = c("6", "P11", "8"),
                       age = c(5, 1,6)))


B0 + B1 * 61



# Posterior predictive model plots
mcmc_areas(predict_next_race, prob = 0.8) +
  ggplot2::scale_y_discrete(labels = c("runner 6", "P11", "runner 8"))


##########

# 计算每个选手的分位数
quantiles <- apply(predict_next_race, 2, function(x) {
  quantile(x, probs = c(0.25, 0.50, 0.75))
})
# 将结果转换为更易读的格式
quantiles_df <- as.data.frame(quantiles)
colnames(quantiles_df) <- c("runner6", "P11", "runner8")
rownames(quantiles_df) <- c("25%", "50%", "75%")
# 显示结果
print(quantiles_df)


#17.7

# Import and wrangle the data
data(spotify)
spotify <- spotify %>%
  select(artist, title, danceability, valence, genre)


ggplot(spotify, aes(y = danceability, x = genre)) +
  geom_boxplot()
ggplot(spotify, aes(y = danceability, x = valence)) +
  geom_point()
ggplot(spotify, aes(y = danceability, x = valence, group = artist)) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5)



spotify_model_1 <- stan_glmer(
  danceability ~ valence + genre + (1 | artist),
  data = spotify, family = gaussian,
  prior_intercept = normal(50, 2.5, autoscale = TRUE),
  prior = normal(0, 2.5, autoscale = TRUE),
  prior_aux = exponential(1, autoscale = TRUE),
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 4, iter = 5000*2, seed = 84735)
spotify_model_2 <- stan_glmer(
  danceability ~ valence + genre + (valence | artist),
  data = spotify, family = gaussian,
  prior_intercept = normal(50, 2.5, autoscale = TRUE),
  prior = normal(0, 2.5, autoscale = TRUE),
  prior_aux = exponential(1, autoscale = TRUE),
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 4, iter = 5000*2, seed = 84735)
# Check out the prior specifications
prior_summary(spotify_model_1)
prior_summary(spotify_model_2)











pp_check(spotify_model_1) +
  xlab("danceability")
pp_check(spotify_model_2) +
  xlab("danceability")

# Calculate ELPD for the 2 models
elpd_spotify_1 <- loo(spotify_model_1)
elpd_spotify_2 <- loo(spotify_model_2)
# Compare the ELPD
loo_compare(elpd_spotify_1, elpd_spotify_2)

tidy(spotify_model_1, effects = "fixed",
     conf.int = TRUE, conf.level = 0.80)

# Plot the posterior models of the genre coefficients
mcmc_areas(spotify_model_1, pars = vars(starts_with("genre")), prob = 0.8) +
  geom_vline(xintercept = 0)

tidy(spotify_model_1, effects = "ran_vals",
     conf.int = TRUE, conf.level = 0.80) %>%
  filter(level %in% c("Camilo", "Missy_Elliott")) %>%
  select(level, estimate, conf.low, conf.high)

# Simulate posterior predictive models for the 3 artists
set.seed(84735)
predict_next_song <- posterior_predict(
  spotify_model_1,
  newdata = data.frame(
    artist = c("Camilo", "Mohsen Beats", "Missy Elliott"),
    valence = c(80, 60, 90), genre = c("latin", "rock", "rap")))
# Posterior predictive model plots
mcmc_areas(predict_next_song, prob = 0.8) +
  ggplot2::scale_y_discrete(
    labels = c("Camilo", "Mohsen Beats", "Missy Elliott"))









































































































