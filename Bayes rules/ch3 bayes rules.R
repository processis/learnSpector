# Plot the Beta(45, 55) prior
#ch3.1
plot_beta(45, 55)

#ch3.3

summarize_beta_binomial(alpha = 45, beta = 55, y = 30, n = 50)

#3.5 Simulating the Beta-Binomial
set.seed(84735)
michelle_sim <- data.frame(pi = rbeta(10000, 45, 55)) %>%
  mutate(y = rbinom(10000, size = 50, prob = pi))

ggplot(michelle_sim, aes(x = pi, y = y)) +
  geom_point(aes(color = (y == 30)), size = 0.1)


# Keep only the simulated pairs that match our data
michelle_posterior <- michelle_sim %>%
  filter(y == 30)
# Plot the remaining pi values
ggplot(michelle_posterior, aes(x = pi)) +
  geom_density()


michelle_posterior %>%
  summarize(mean(pi), sd(pi))

nrow(michelle_posterior)

#ch3.6.1

# Beta(1,10) prior
plot_beta(alpha = 1, beta = 10)
            
            