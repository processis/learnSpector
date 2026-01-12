############### BR1 code on Erdogmu Exp1 data
# 2025.11.1
# calculate z-score for normal distribution (deepseek)
# "calculate z vlaue in normal distribution R code example" (deepseek chat)
# basic example
x <- 85
mean <- 75
sd <- 10
z <- (x - mean / sd)
print(paste("Z-score:",z))

#Find prob  P(Z <= z) for a given z-score
z <- 1.96
probability <- pnorm(z)
print(paste("P(Z <=", z, ") =",round(probability, 4)))

#Find prob  for value above z
z <- 1.96
prob_above <- 1 - pnorm(z)
print(paste("P(Z >", z, ") =",round(prob_above, 4)))

########
# modify basic z score to calculate for binom  N and p
N <- 28
propor <- 0.5
x <- 17
mean <-propor *N
sd <-sqrt(N*propor*(1 - propor))
z <- ((x - mean) / sd)
print(paste("Z-score:",z))
# calculate the one tail p-value 
prob_above <- 1 - pnorm(z)
print(paste("P(Z >", z, ") =",round(prob_above, 4)))
# repeat for Rep1 N=25 , x =19
N <- 25
propor <- 0.5
x <- 19
mean <-propor *N
sd <-sqrt(N*propor*(1 - propor))
z <- ((x - mean) / sd)
print(paste("Z-score:",z))
# calculate the one tail p-value 
prob_above <- 1 - pnorm(z)
print(paste("P(Z >", z, ") =",round(prob_above, 4)))
# repeat for Rep2: N=12 , x =8
N <- 12
propor <- 0.5
x <- 8
mean <-propor *N
sd <-sqrt(N*propor*(1 - propor))
z <- ((x - mean) / sd)
print(paste("Z-score:",z))
# calculate the one tail p-value 
prob_above <- 1 - pnorm(z)
print(paste("P(Z >", z, ") =",round(prob_above, 4)))
# repeat for Rep3: N=12 , x =9
N <- 12
propor <- 0.5
x <- 9
mean <-propor *N
sd <-sqrt(N*propor*(1 - propor))
z <- ((x - mean) / sd)
print(paste("Z-score:",z))
# calculate the one tail p-value 
prob_above <- 1 - pnorm(z)
print(paste("P(Z >", z, ") =",round(prob_above, 4)))

# repeat for Rep4: N=11 , x =8
N <- 11
propor <- 0.5
x <- 8
mean <-propor *N
sd <-sqrt(N*propor*(1 - propor))
z <- ((x - mean) / sd)
print(paste("Z-score:",z))
# calculate the one tail p-value 
prob_above <- 1 - pnorm(z)
print(paste("P(Z >", z, ") =",round(prob_above, 4)))

#Find prob  P(Z <= z) for a given z-score
z <- 1.645
probability <- pnorm(z)
print(paste("P(Z <=", z, ") =",round(probability, 4)))
# calculate the one tail p-value 
prob_above <- 1 - pnorm(z)
print(paste("P(Z >", z, ") =",round(prob_above, 4)))

# repeat for Rep5: N=10 , x =7
N <- 10
propor <- 0.5
x <- 7
mean <-propor *N
sd <-sqrt(N*propor*(1 - propor))
z <- ((x - mean) / sd)
print(paste("Z-score:",z))
# calculate the one tail p-value 
prob_above <- 1 - pnorm(z)
print(paste("P(Z >", z, ") =",round(prob_above, 4)))

# repeat for Rep6: N=10 , x =8
N <- 10
propor <- 0.5
x <- 8
mean <-propor *N
sd <-sqrt(N*propor*(1 - propor))
z <- ((x - mean) / sd)
print(paste("Z-score:",z))
# calculate the one tail p-value 
prob_above <- 1 - pnorm(z)
print(paste("P(Z >", z, ") =",round(prob_above, 4)))

################
# calculate binomial distribution probability R code examples (ask deepseek)
# Hypothesis testing
# test if a coin is fair (p = 0.5 ) given 7 heads in 10 flips
binom_test <- binom.test(7, 10 , p = 0.5)
print(binom_test)
# extract p - value
p_value <-binom_test$p.value
cat("p-value:",round(p_value,4),"\n")
# try for x=4 N=5 for Repeat 7
binom_test <- binom.test(4, 5 , p = 0.5)
print(binom_test)
# extract p - value
p_value <-binom_test$p.value
cat("p-value:",round(p_value,4),"\n")

# try for x=8 N=10 for Repeat 6
binom_test <- binom.test(8, 10 , p = 0.5)
print(binom_test)
# extract p - value
p_value <-binom_test$p.value
cat("p-value:",round(p_value,4),"\n")

# try for x=9 N=12 for Repeat 3
binom_test <- binom.test(9, 12 , p = 0.5)
print(binom_test)
# extract p - value
p_value <-binom_test$p.value
cat("p-value:",round(p_value,4),"\n")

#################
#cal z-value and p-value for sum of N and sum of x
# repeat for sum of Exp and 7 repeats: N=113 , x =80
N <- 113
propor <- 0.5
x <- 80
mean <-propor *N
sd <-sqrt(N*propor*(1 - propor))
z <- ((x - mean) / sd)
print(paste("Z-score:",z))
# calculate the one tail p-value 
prob_above <- 1 - pnorm(z)
print(paste("P(Z >", z, ") =",round(prob_above, 4)))