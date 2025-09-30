library(ggplot2)


set.seed(84375)
mc_tour <- data.frame(mu = rnorm(5000, mean = 4, sd = 0.6))
ggplot(mc_tour, aes(x = mu)) +
  geom_histogram(aes(y = ..density..), color = "white", bins = 15) +
  stat_function(fun = dnorm, args = list(4, 0.6), color = "blue")

current <- 3
set.seed(8)
proposal <- runif(1, min = current - 1, max = current + 1)
proposal

proposal_plaus <- dnorm(proposal, 0, 1) * dnorm(6.25, proposal, 0.75)
proposal_plaus

current_plaus <- dnorm(current, 0, 1) * dnorm(6.25, current, 0.75)
current_plaus

alpha <- min(1, proposal_plaus / current_plaus)
alpha

next_stop <- sample(c(proposal, current),
                    size = 1, prob = c(alpha, 1-alpha))
next_stop


one_mh_iteration <- function(w, current){
  # STEP 1: Propose the next chain location
  proposal <- runif(1, min = current - w, max = current + w)
  # STEP 2: Decide whether or not to go there
  proposal_plaus <- dnorm(proposal, 0, 1) * dnorm(6.25, proposal, 0.75)
  current_plaus <- dnorm(current, 0, 1) * dnorm(6.25, current, 0.75)
  alpha <- min(1, proposal_plaus / current_plaus)
  next_stop <- sample(c(proposal, current),
                      size = 1, prob = c(alpha, 1-alpha))
  # Return the results
  return(data.frame(proposal, alpha, next_stop))
}

set.seed(8)
one_mh_iteration(w = 1, current = 3)

set.seed(83)
one_mh_iteration(w = 1, current = 3)

set.seed(7)
one_mh_iteration(w = 1, current = 3)

mh_tour <- function(N, w){
  # 1. Start the chain at location 3
  current <- 3
  # 2. Initialize the simulation
  mu <- rep(0, N)
  # 3. Simulate N Markov chain stops
  for(i in 1:N){
    # Simulate one iteration
    sim <- one_mh_iteration(w = w, current = current)
    # Record next location
    mu[i] <- sim$next_stop
    # Reset the current location
    current <- sim$next_stop
  }
  # 4. Return the chain locations
  return(data.frame(iteration = c(1:N), mu))
}

set.seed(84735)
mh_simulation_1 <- mh_tour(N = 5000, w = 1)


ggplot(mh_simulation_1, aes(x = iteration, y = mu)) +
  geom_line()
ggplot(mh_simulation_1, aes(x = mu)) +
  geom_histogram(aes(y = ..density..), color = "white", bins = 20) +
  stat_function(fun = dnorm, args = list(4,0.6), color = "blue")

set.seed(84735)
mh_simulation_2 <- mh_tour(N = 5000, w = 0.01)
ggplot(mh_simulation_2, aes(x = iteration, y = mu)) +
  geom_line() +
  lims(y = c(1.6, 6.4))


set.seed(7)
mh_simulation_3 <- mh_tour(N = 5000, w = 100)
ggplot(mh_simulation_3, aes(x = iteration, y = mu)) +
  geom_line() +
  lims(y = c(1.6,6.4))

one_iteration <- function(a, b, current){
  # STEP 1: Propose the next chain location
  proposal <- rbeta(1, a, b)
  # STEP 2: Decide whether or not to go there
  proposal_plaus <- dbeta(proposal, 2, 3) * dbinom(1, 2, proposal)
  proposal_q <- dbeta(proposal, a, b)
  current_plaus <- dbeta(current, 2, 3) * dbinom(1, 2, current)
  current_q <- dbeta(current, a, b)
  alpha <- min(1, proposal_plaus / current_plaus * current_q / proposal_q)
  next_stop <- sample(c(proposal, current),
                      size = 1, prob = c(alpha, 1-alpha))
  return(data.frame(proposal, alpha, next_stop))
}



betabin_tour <- function(N, a, b){
  # 1. Start the chain at location 0.5
  current <- 0.5
  # 2. Initialize the simulation
  pi <- rep(0, N)
  # 3. Simulate N Markov chain stops
  for(i in 1:N){
    # Simulate one iteration
    sim <- one_iteration(a = a, b = b, current = current)
    # Record next location
    pi[i] <- sim$next_stop
    # Reset the current location
    current <- sim$next_stop
  }
  # 4. Return the chain locations
  return(data.frame(iteration = c(1:N), pi))
}


set.seed(84735)
betabin_sim <- betabin_tour(N = 5000, a = 1, b = 1)
# Plot the results
ggplot(betabin_sim, aes(x = iteration, y = pi)) +
  geom_line()
ggplot(betabin_sim, aes(x = pi)) +
  geom_histogram(aes(y = ..density..), color = "white") +
  stat_function(fun = dbeta, args = list(3, 4), color = "blue")














