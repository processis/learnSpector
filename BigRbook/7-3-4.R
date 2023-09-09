#7.3.4Advanced Piping

# This will not work, because lm() is not designed for the pipe.
lm1 <- tibble("x" = runif(10))%>%
  within(y <- 2 * x + 4 + rnorm(10, mean=0, sd=0.5)) %>%
  lm(y ~ x)

# The Tidyverse only makes the %>% pipe available. So, to use the
# special pipes, we need to load magrittr
library(magrittr)

lm2 <- tibble("x" = runif(10))%>%
  within(y <- 2 * x + 4 + rnorm(10, mean=0,sd=0.5))%$%
  lm(y ~ x)
summary(lm2)


coeff <- tibble("x" = runif(10)) %>%
      within(y <- 2 * x + 4 + rnorm(10, mean=0,sd=0.5)) %$%
      lm(y ~x)    %>%
      summary     %>%
      coefficients
coeff

x <- c(1,2,3)
# The following line:
x <- x %>% mean
# is equivalent with the following:
x %<>% mean
# Show x:
x
## [1
lm(y ~ x)
summary
coefficients
coeff


library(magrittr)
t <- tibble("x" = runif(100))%>%
  within(y <- 2 * x + 4 + rnorm(10, mean=0, sd=0.5)) %T>%
  plot(col="red")
# The function plot does not return anything
# so we used the %T>% pipe.
lm3 <-t   %$%
lm(y ~ x)   %T>%
summary      %T>% 
coefficients

   # pass on the linear model
  # further pass on the linear model
tcoef <- lm3 %>% coefficients
# we anyhow need the coefficients




# Add the model (the solid line) to the previous plot:
abline(a = tcoef[1], b=tcoef[2], col="blue", lwd=3)

#7.3.4.3 Assignment Pipe
x <- c(1,2,3)
# The following line:
x <- x %>% mean
# is equivalent with the following:
x %<>% mean
# Show x:
x


#following code cannot run on my R 3.5.2, package ‘pryr’ is not available
library(pryr)
x <- runif(100)
object_size(x)

y <- x
# x and y together do not take more memory than only x.
object_size(x,y)

y <- y * 2
# Now, they are different and are stored separately in memory.
object_size(x,y)