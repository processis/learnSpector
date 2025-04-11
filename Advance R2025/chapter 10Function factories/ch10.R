power1 <- function(exp) {
  function(x) {
    x ^ exp
  }
}

square <- power1(2)
cube <- power1(3)

square(3)
#> [1] 9
cube(3)
#> [1] 27
#> 
library(rlang)
library(ggplot2)
library(scales)

# 10.2 Factory fundamentals 
# 10.2.1 Environments 
square
#> function(x) {
#>     x ^ exp
#>   }
#> <environment: 0x7fe851f7ccc0>

cube
#> function(x) {
#>     x ^ exp
#>   }
#> <bytecode: 0x7fe85512a410>
#> <environment: 0x7fe85508c390>
#> 
env_print(square)
#> <environment: 0x7fe851f7ccc0>
#> parent: <environment: global>
#> bindings:
#>  * exp: <dbl>

env_print(cube)
#> <environment: 0x7fe85508c390>
#> parent: <environment: global>
#> bindings:
#>  * exp: <dbl>
#>  
fn_env(square)$exp
#> [1] 2

fn_env(cube)$exp
#> [1] 3
#> 
# 10.2.2 Diagram conventions 
square(10)
#> [1] 100
#> 
x <- 2
square <- power1(x)
x <- 3

square(2)
#> [1] 8
#> 
power2 <- function(exp) {
  force(exp)
  function(x) {
    x ^ exp
  }
}

x <- 2
square <- power2(x)
x <- 3
square(2)
#> [1] 4
#> 
# 10.2.4 Stateful functions 
new_counter <- function() {
  i <- 0
  
  function() {
    i <<- i + 1
    i
  }
}

counter_one <- new_counter()
counter_two <- new_counter()

counter_one()
#> [1] 1
counter_one()
#> [1] 2
counter_two()
#> [1] 1
#> 
# 10.2.5 Garbage collection 
f1 <- function(n) {
  x <- runif(n)
  m <- mean(x)
  function() m
}

g1 <- f1(1e6)
lobstr::obj_size(g1)
#> 8,013,104 B

f2 <- function(n) {
  x <- runif(n)
  m <- mean(x)
  rm(x)
  function() m
}

g2 <- f2(1e6)
lobstr::obj_size(g2)
#> 12,944 B
#> 
# 10.2.6 Exercises 
force
#> function (x) 
#> x
#> <bytecode: 0x7fe8519464b0>
#> <environment: namespace:base>
#> 
pick(1)(x)
# should be equivalent to
x[[1]]

lapply(mtcars, pick(5))
# should be equivalent to
lapply(mtcars, function(x) x[[5]])

m1 <- moment(1)
m2 <- moment(2)

x <- runif(100)
stopifnot(all.equal(m1(x), 0))
stopifnot(all.equal(m2(x), var(x) * 99 / 100))

i <- 0
new_counter2 <- function() {
  i <<- i + 1
  i
}

new_counter3 <- function() {
  i <- 0
  function() {
    i <- i + 1
    i
  }
}

# 10.3 Graphical factories 
# 10.3.1 Labelling 
y <- c(12345, 123456, 1234567)
comma_format()(y)
#> [1] "12,345"    "123,456"   "1,234,567"

number_format(scale = 1e-3, suffix = " K")(y)
#> [1] "12 K"    "123 K"   "1 235 K"
#> 
df <- data.frame(x = 1, y = y)
core <- ggplot(df, aes(x, y)) + 
  geom_point() + 
  scale_x_continuous(breaks = 1, labels = NULL) +
  labs(x = NULL, y = NULL)

core
core + scale_y_continuous(
  labels = comma_format()
)
core + scale_y_continuous(
  labels = number_format(scale = 1e-3, suffix = " K")
)
core + scale_y_continuous(
  labels = scientific_format()
)

# 10.3.2 Histogram bins 
# construct some sample data with very different numbers in each cell
sd <- c(1, 5, 15)
n <- 100

df <- data.frame(x = rnorm(3 * n, sd = sd), sd = rep(sd, n))

ggplot(df, aes(x)) + 
  geom_histogram(binwidth = 2) + 
  facet_wrap(~ sd, scales = "free_x") + 
  labs(x = NULL)


binwidth_bins <- function(n) {
  force(n)
  
  function(x) {
    (max(x) - min(x)) / n
  }
}

ggplot(df, aes(x)) + 
  geom_histogram(binwidth = binwidth_bins(20)) + 
  facet_wrap(~ sd, scales = "free_x") + 
  labs(x = NULL)


base_bins <- function(type) {
  fun <- switch(type,
                Sturges = nclass.Sturges,
                scott = nclass.scott,
                FD = nclass.FD,
                stop("Unknown type", call. = FALSE)
  )
  
  function(x) {
    (max(x) - min(x)) / fun(x)
  }
}

ggplot(df, aes(x)) + 
  geom_histogram(binwidth = base_bins("FD")) + 
  facet_wrap(~ sd, scales = "free_x") + 
  labs(x = NULL)

#10.3.3 ggsave()
plot_dev <- function(ext, dpi = 96) {
  force(dpi)
  
  switch(ext,
         eps =  ,
         ps  =  function(path, ...) {
           grDevices::postscript(
             file = filename, ..., onefile = FALSE, 
             horizontal = FALSE, paper = "special"
           )
         },
         pdf = function(filename, ...) grDevices::pdf(file = filename, ...),
         svg = function(filename, ...) svglite::svglite(file = filename, ...),
         emf = ,
         wmf = function(...) grDevices::win.metafile(...),
         png = function(...) grDevices::png(..., res = dpi, units = "in"),
         jpg = ,
         jpeg = function(...) grDevices::jpeg(..., res = dpi, units = "in"),
         bmp = function(...) grDevices::bmp(..., res = dpi, units = "in"),
         tiff = function(...) grDevices::tiff(..., res = dpi, units = "in"),
         stop("Unknown graphics extension: ", ext, call. = FALSE)
  )
}

plot_dev("pdf")
#> function(filename, ...) grDevices::pdf(file = filename, ...)
#> <bytecode: 0x7fe857744590>
#> <environment: 0x7fe8575f6638>
plot_dev("png")
#> function(...) grDevices::png(..., res = dpi, units = "in")
#> <bytecode: 0x7fe85947f938>
#> <environment: 0x7fe859169548>
#> 
#> 
# 10.4 Statistical factories 
# 10.4.1 Box-Cox transformation 
boxcox1 <- function(x, lambda) {
  stopifnot(length(lambda) == 1)
  
  if (lambda == 0) {
    log(x)
  } else {
    (x ^ lambda - 1) / lambda
  }
}


boxcox2 <- function(lambda) {
  if (lambda == 0) {
    function(x) log(x)
  } else {
    function(x) (x ^ lambda - 1) / lambda
  }
}

stat_boxcox <- function(lambda) {
  stat_function(aes(colour = lambda), fun = boxcox2(lambda), size = 1)
}

ggplot(data.frame(x = c(0, 5)), aes(x)) + 
  lapply(c(0.5, 1, 1.5), stat_boxcox) + 
  scale_colour_viridis_c(limits = c(0, 1.5))

# visually, log() does seem to make sense as the transformation
# for lambda = 0; as values get smaller and smaller, the function
# gets close and closer to a log transformation
ggplot(data.frame(x = c(0.01, 1)), aes(x)) + 
  lapply(c(0.5, 0.25, 0.1, 0), stat_boxcox) + 
  scale_colour_viridis_c(limits = c(0, 1.5))

# 10.4.2 Bootstrap generators 
boot_permute <- function(df, var) {
  n <- nrow(df)
  force(var)
  
  function() {
    col <- df[[var]]
    col[sample(n, replace = TRUE)]
  }
}

boot_mtcars1 <- boot_permute(mtcars, "mpg")
head(boot_mtcars1())
#> [1] 16.4 22.8 22.8 22.8 16.4 19.2
head(boot_mtcars1())
#> [1] 17.8 18.7 30.4 30.4 16.4 21.0
#> 
#> 
boot_model <- function(df, formula) {
  mod <- lm(formula, data = df)
  fitted <- unname(fitted(mod))
  resid <- unname(resid(mod))
  rm(mod)
  
  function() {
    fitted + sample(resid)
  }
} 

boot_mtcars2 <- boot_model(mtcars, mpg ~ wt)
head(boot_mtcars2())
#> [1] 25.0 24.0 21.7 19.2 24.9 16.0
head(boot_mtcars2())
#> [1] 27.4 21.0 20.3 19.4 16.3 21.3
#> 
#> 
#10.4.3 Maximum likelihood estimation
lprob_poisson <- function(lambda, x) {
  n <- length(x)
  (log(lambda) * sum(x)) - (n * lambda) - sum(lfactorial(x))
}

x1 <- c(41, 30, 31, 38, 29, 24, 30, 29, 31, 38)

lprob_poisson(10, x1)
#> [1] -184
lprob_poisson(20, x1)
#> [1] -61.1
lprob_poisson(30, x1)
#> [1] -31
#> 
#> 
ll_poisson1 <- function(x) {
  n <- length(x)
  
  function(lambda) {
    log(lambda) * sum(x) - n * lambda - sum(lfactorial(x))
  }
}

ll_poisson2 <- function(x) {
  n <- length(x)
  sum_x <- sum(x)
  c <- sum(lfactorial(x))
  
  function(lambda) {
    log(lambda) * sum_x - n * lambda - c
  }
}

ll1 <- ll_poisson2(x1)

ll1(10)
#> [1] -184
ll1(20)
#> [1] -61.1
ll1(30)
#> [1] -31
#> 
#> 
optimise(ll1, c(0, 100), maximum = TRUE)
#> $maximum
#> [1] 32.1
#> 
#> $objective
#> [1] -30.3
#> 
#> 
optimise(lprob_poisson, c(0, 100), x = x1, maximum = TRUE)
#> $maximum
#> [1] 32.1
#> 
#> $objective
#> [1] -30
#> 
#> 
#10.4.4 Exercises 
boxcox3 <- function(x) {
  function(lambda) {
    if (lambda == 0) {
      log(x)
    } else {
      (x ^ lambda - 1) / lambda
    }
  }  
}


# 10.5 Function factories + functionals 
names <- list(
  square = 2, 
  cube = 3, 
  root = 1/2, 
  cuberoot = 1/3, 
  reciprocal = -1
)
funs <- purrr::map(names, power1)

funs$root(64)
#> [1] 8
funs$root
#> function(x) {
#>     x ^ exp
#>   }
#> <bytecode: 0x7fe85512a410>
#> <environment: 0x7fe85b21f190>
#> 
#> 
with(funs, root(100))
#> [1] 10
#> 
#> 
attach(funs)
#> The following objects are masked _by_ .GlobalEnv:
#> 
#>     cube, square
root(100)
#> [1] 10
detach(funs)

rlang::env_bind(globalenv(), !!!funs)
root(100)
#> [1] 10
#> 
#> 
rlang::env_unbind(globalenv(), names(funs))

# 10.5.1 Exercises 
funs <- list(
  mean = function(x) mean(x, na.rm = TRUE),
  sum = function(x) sum(x, na.rm = TRUE)
)

attach(funs)
#> The following objects are masked from package:base:
#> 
#>     mean, sum
mean <- function(x) stop("Hi!")
detach(funs)

env_bind(globalenv(), !!!funs)
mean <- function(x) stop("Hi!") 
env_unbind(globalenv(), names(funs))