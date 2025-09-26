# Load packages that will be used in this chapter
library(bayesrules)
library(tidyverse)
library(janitor)

##4.1 Different priors, different posteriors 
# Import data
data(bechdel, package = "bayesrules")
# Take a sample of 20 movies
set.seed(84735)
bechdel_20 <- bechdel %>%
  sample_n(20)

bechdel_20 %>%
  head(3)

bechdel_20 %>%
  tabyl(binary) %>%
  adorn_totals("row")

#4.2 Different data, different posteriors
bechdel %>%
  filter(year == 1991) %>%
  tabyl(binary) %>%
  adorn_totals("row")

bechdel %>%
  filter(year == 2000) %>%
  tabyl(binary) %>%
  adorn_totals("row")

bechdel %>%
  filter(year == 2013) %>%
  tabyl(binary) %>%
  adorn_totals("row")

#4.3 Striking a balance between the prior & data 

# Plot the Beta-Binomial model
plot_beta_binomial(alpha = _, beta = _, y = _, n = _)
# Obtain numerical summaries of the Beta-Binomial model
summarize_beta_binomial(alpha = _, beta = _, y = _, n = _)


