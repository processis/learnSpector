#可以正常跑通

# Load packages
library(bayesrules)
library(tidyverse)
library(e1071)
library(janitor)
# Load data
data(penguins_bayes)
penguins <- penguins_bayes

penguins %>%
  tabyl(species)

ggplot(penguins %>% drop_na(above_average_weight),
       aes(fill = above_average_weight, x = species)) +
  geom_bar(position = "fill")

penguins %>%
  select(species, above_average_weight) %>%
  na.omit() %>%
  tabyl(species, above_average_weight) %>%
  adorn_totals(c("row", "col"))


ggplot(penguins, aes(x = bill_length_mm, fill = species)) +
  geom_density(alpha = 0.7) +
  geom_vline(xintercept = 50, linetype = "dashed")

# Calculate sample mean and sd for each Y group
penguins %>%
  group_by(species) %>%
  summarize(mean = mean(bill_length_mm, na.rm = TRUE),
            sd = sd(bill_length_mm, na.rm = TRUE))

ggplot(penguins, aes(x = bill_length_mm, color = species)) +
  stat_function(fun = dnorm, args = list(mean = 38.8, sd = 2.66),
                aes(color = "Adelie")) +
  stat_function(fun = dnorm, args = list(mean = 48.8, sd = 3.34),
                aes(color = "Chinstrap")) +
  stat_function(fun = dnorm, args = list(mean = 47.5, sd = 3.08),
                aes(color = "Gentoo")) +
  geom_vline(xintercept = 50, linetype = "dashed")


dnorm(50, mean = 38.8, sd = 2.66)
dnorm(50, mean = 48.8, sd = 3.34)
dnorm(50, mean = 47.5, sd = 3.08)

ggplot(penguins, aes(x = bill_length_mm, fill = species)) +
  geom_density(alpha = 0.6)
ggplot(penguins, aes(x = flipper_length_mm, fill = species)) +
  geom_density(alpha = 0.6)

ggplot(penguins,
       aes(x = flipper_length_mm, y = bill_length_mm, color = species)) +
  geom_point()


# Calculate sample mean and sd for each Y group
penguins %>%
  group_by(species) %>%
  summarize(mean = mean(flipper_length_mm, na.rm = TRUE),
            sd = sd(flipper_length_mm, na.rm = TRUE))

dnorm(195, mean = 190, sd = 6.54)
dnorm(195, mean = 196, sd = 7.13)
dnorm(195, mean = 217, sd = 6.48)

naive_model_1 <- naiveBayes(species ~ bill_length_mm, data = penguins)
naive_model_2 <- naiveBayes(species ~ bill_length_mm + flipper_length_mm,
                            data = penguins)

our_penguin <- data.frame(bill_length_mm = 50, flipper_length_mm = 195)


predict(naive_model_1, newdata = our_penguin, type = "raw")
predict(naive_model_1, newdata = our_penguin)

predict(naive_model_2, newdata = our_penguin, type = "raw")
predict(naive_model_2, newdata = our_penguin)

penguins <- penguins %>%
  mutate(class_1 = predict(naive_model_1, newdata = .),
         class_2 = predict(naive_model_2, newdata = .))

set.seed(84735)
penguins %>%
  sample_n(4) %>%
  select(bill_length_mm, flipper_length_mm, species, class_1, class_2) %>%
  rename(bill = bill_length_mm, flipper = flipper_length_mm)


# Confusion matrix for naive_model_1
penguins %>%
  tabyl(species, class_1) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()


# Confusion matrix for naive_model_2
penguins %>%
  tabyl(species, class_2) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()



set.seed(84735)
cv_model_2 <- naive_classification_summary_cv(
  model = naive_model_2, data = penguins, y = "species", k = 10)


cv_model_2$cv
































































