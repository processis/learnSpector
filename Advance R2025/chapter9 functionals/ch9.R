randomise <- function(f) f(runif(1e3))
randomise(mean)
#> [1] 0.506
randomise(mean)
#> [1] 0.501
randomise(sum)

#My first functional: map()

library(purrr)

triple <- function(x) x * 3
map(1:3, triple)

simple_map <- function(x, f, ...) {
  out <- vector("list", length(x))
  for (i in seq_along(x)) {
    out[[i]] <- f(x[[i]], ...)
  }
  out
}

# 9.2.1 Producing atomic vectors 

# map_chr() always returns a character vector
map_chr(mtcars, typeof)
#>      mpg      cyl     disp       hp     drat       wt     qsec       vs 
#> "double" "double" "double" "double" "double" "double" "double" "double" 
#>       am     gear     carb 
#> "double" "double" "double"

# map_lgl() always returns a logical vector
map_lgl(mtcars, is.double)
#>  mpg  cyl disp   hp drat   wt qsec   vs   am gear carb 
#> TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE

# map_int() always returns a integer vector
n_unique <- function(x) length(unique(x))
map_int(mtcars, n_unique)
#>  mpg  cyl disp   hp drat   wt qsec   vs   am gear carb 
#>   25    3   27   22   22   29   30    2    2    3    6

# map_dbl() always returns a double vector
map_dbl(mtcars, mean)
#>     mpg     cyl    disp      hp    drat      wt    qsec      vs      am    gear 
#>  20.091   6.188 230.722 146.688   3.597   3.217  17.849   0.438   0.406   3.688 
#>    carb 
#>   2.812

pair <- function(x) c(x, x)

map(1:2, pair)
#> [[1]]
#> [1] 1 1
#> 
#> [[2]]
#> [1] 2 2
map(1:2, as.character)
#> [[1]]
#> [1] "1"
#> 
#> [[2]]
#> [1] "2"
#> 
#9.2.2 Anonymous functions and shortcuts

map_dbl(mtcars, function(x) length(unique(x)))
#>  mpg  cyl disp   hp drat   wt qsec   vs   am gear carb 
#>   25    3   27   22   22   29   30    2    2    3    6
#>   
map_dbl(mtcars, ~ length(unique(.x)))
#>  mpg  cyl disp   hp drat   wt qsec   vs   am gear carb 
#>   25    3   27   22   22   29   30    2    2    3    6
#>   
as_mapper(~ length(unique(.x)))
#> <lambda>
#> function (..., .x = ..1, .y = ..2, . = ..1) 
#> length(unique(.x))
#> attr(,"class")
#> [1] "rlang_lambda_function" "function"
#> 
x <- map(1:3, ~ runif(2))
str(x)
#> List of 3
#>  $ : num [1:2] 0.281 0.53
#>  $ : num [1:2] 0.433 0.917
#>  $ : num [1:2] 0.0275 0.8249
#>  
x <- list(
  list(-1, x = 1, y = c(2), z = "a"),
  list(-2, x = 4, y = c(5, 6), z = "b"),
  list(-3, x = 8, y = c(9, 10, 11))
)

# Select by name
map_dbl(x, "x")
#> [1] 1 4 8

# Or by position
map_dbl(x, 1)
#> [1] -1 -2 -3

# Or by both
map_dbl(x, list("y", 1))
#> [1] 2 5 9

# You'll get an error if a component doesn't exist:
map_chr(x, "z")
#> Error: Result 3 must be a single string, not NULL of length 0

# Unless you supply a .default value
map_chr(x, "z", .default = NA)
#> [1] "a" "b" NA
#> 
#9.2.3 Passing arguments with
x <- list(1:5, c(1:10, NA))
map_dbl(x, ~ mean(.x, na.rm = TRUE))
#> [1] 3.0 5.5
#> 
map_dbl(x, mean, na.rm = TRUE)
#> [1] 3.0 5.5
#> 
plus <- function(x, y) x + y

x <- c(0, 0, 0, 0)
map_dbl(x, plus, runif(1))
#> [1] 0.0625 0.0625 0.0625 0.0625
map_dbl(x, ~ plus(.x, runif(1)))
#> [1] 0.903 0.132 0.629 0.945
#> 
# 9.2.4 Argument names 
boostrap_summary <- function(x, f) {
  f(sample(x, replace = TRUE))
}

simple_map(mtcars, boostrap_summary, f = mean)
#> Error in mean.default(x[[i]], ...): 'trim' must be numeric of length one
#> 
# 9.2.5 Varying another argument 

trims <- c(0, 0.1, 0.2, 0.5)
x <- rcauchy(1000)
map_dbl(trims, ~ mean(x, trim = .x))
#> [1] -0.3500  0.0434  0.0354  0.0502
#> 
map_dbl(trims, function(trim) mean(x, trim = trim))
#> [1] -0.3500  0.0434  0.0354  0.0502
#> 
# 9.2.6 Exercises 
trials <- map(1:100, ~ t.test(rpois(10, 10), rpois(7, 10)))

x <- list(
  list(1, c(3, 9)),
  list(c(3, 6), 7, c(4, 7, 6))
)

triple <- function(x) x * 3
map(x, map, .f = triple)
#> Error in .f(.x[[i]], ...): unused argument (function (.x, .f, ...)
#> {
#> .f <- as_mapper(.f, ...)
#> .Call(map_impl, environment(), ".x", ".f", "list")
#> })

formulas <- list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt
)

bootstrap <- function(df) {
  df[sample(nrow(df), replace = TRUE), , drop = FALSE]
}

bootstraps <- map(1:10, ~ bootstrap(mtcars))

# 9.3 Purrr style 

by_cyl <- split(mtcars, mtcars$cyl)

by_cyl %>% 
  map(~ lm(mpg ~ wt, data = .x)) %>% 
  map(coef) %>% 
  map_dbl(2)
#>     4     6     8 
#> -5.65 -2.78 -2.19
#> 
by_cyl %>% 
  lapply(function(data) lm(mpg ~ wt, data = data)) %>% 
  lapply(coef) %>% 
  vapply(function(x) x[[2]], double(1))
#>     4     6     8 
#> -5.65 -2.78 -2.19
#> 
models <- lapply(by_cyl, function(data) lm(mpg ~ wt, data = data))
vapply(models, function(x) coef(x)[[2]], double(1))
#>     4     6     8 
#> -5.65 -2.78 -2.19
#> 
slopes <- double(length(by_cyl))
for (i in seq_along(by_cyl)) {
  model <- lm(mpg ~ wt, data = by_cyl[[i]])
  slopes[[i]] <- coef(model)[[2]]
}
slopes
#> [1] -5.65 -2.78 -2.19
#> 
# 9.4 Map variants 
#9.4.1 Same type of output as input: modify()
df <- data.frame(
  x = 1:3,
  y = 6:4
)

map(df, ~ .x * 2)
#> $x
#> [1] 2 4 6
#> 
#> $y
#> [1] 12 10  8
#> 
modify(df, ~ .x * 2)
#>   x  y
#> 1 2 12
#> 2 4 10
#> 3 6  8
#> 
df <- modify(df, ~ .x * 2)

simple_modify <- function(x, f, ...) {
  for (i in seq_along(x)) {
    x[[i]] <- f(x[[i]], ...)
  }
  x
}

#9.4.2 Two inputs: map2() and friends
xs <- map(1:8, ~ runif(10))
xs[[1]][[1]] <- NA
ws <- map(1:8, ~ rpois(10, 5) + 1)

map_dbl(xs, mean)
#> [1]    NA 0.463 0.551 0.453 0.564 0.501 0.371 0.443
#> 
map_dbl(xs, weighted.mean, w = ws)
#> Error in weighted.mean.default(.x[[i]], ...): 'x' and 'w' must have the same
#> length
#> 
map2_dbl(xs, ws, weighted.mean)
#> [1]    NA 0.451 0.603 0.452 0.563 0.510 0.342 0.464
#> 
map2_dbl(xs, ws, weighted.mean, na.rm = TRUE)
#> [1] 0.504 0.451 0.603 0.452 0.563 0.510 0.342 0.464
#> 
simple_map2 <- function(x, y, f, ...) {
  out <- vector("list", length(x))
  for (i in seq_along(x)) {
    out[[i]] <- f(x[[i]], y[[i]], ...)
  }
  out
}

# 9.4.3 No outputs: walk() and friends 
welcome <- function(x) {
  cat("Welcome ", x, "!\n", sep = "")
}
names <- c("Hadley", "Jenny")

# As well as generate the welcomes, it also shows 
# the return value of cat()
map(names, welcome)
#> Welcome Hadley!
#> Welcome Jenny!
#> [[1]]
#> NULL
#> 
#> [[2]]
#> NULL
#> 
walk(names, welcome)
#> Welcome Hadley!
#> Welcome Jenny!
#> 
temp <- tempfile()
dir.create(temp)

cyls <- split(mtcars, mtcars$cyl)
paths <- file.path(temp, paste0("cyl-", names(cyls), ".csv"))
walk2(cyls, paths, write.csv)

dir(temp)
#> [1] "cyl-4.csv" "cyl-6.csv" "cyl-8.csv"
#> 
# 9.4.4 Iterating over values and indices 
imap_chr(iris, ~ paste0("The first value of ", .y, " is ", .x[[1]]))
#>                             Sepal.Length 
#> "The first value of Sepal.Length is 5.1" 
#>                              Sepal.Width 
#>  "The first value of Sepal.Width is 3.5" 
#>                             Petal.Length 
#> "The first value of Petal.Length is 1.4" 
#>                              Petal.Width 
#>  "The first value of Petal.Width is 0.2" 
#>                                  Species 
#>   "The first value of Species is setosa"
#>   
x <- map(1:6, ~ sample(1000, 10))
imap_chr(x, ~ paste0("The highest value of ", .y, " is ", max(.x)))
#> [1] "The highest value of 1 is 975" "The highest value of 2 is 915"
#> [3] "The highest value of 3 is 982" "The highest value of 4 is 955"
#> [5] "The highest value of 5 is 971" "The highest value of 6 is 696"
#> 
# 9.4.5 Any number of inputs: pmap() and friends 
pmap_dbl(list(xs, ws), weighted.mean)
#> [1]    NA 0.451 0.603 0.452 0.563 0.510 0.342 0.464
#> 
pmap_dbl(list(xs, ws), weighted.mean, na.rm = TRUE)
#> [1] 0.504 0.451 0.603 0.452 0.563 0.510 0.342 0.464
#> 
trims <- c(0, 0.1, 0.2, 0.5)
x <- rcauchy(1000)

pmap_dbl(list(trim = trims), mean, x = x)
#> [1] -6.6740  0.0210  0.0235  0.0151
#> 
params <- tibble::tribble(
  ~ n, ~ min, ~ max,
  1L,     0,     1,
  2L,    10,   100,
  3L,   100,  1000
)

pmap(params, runif)
#> [[1]]
#> [1] 0.332
#> 
#> [[2]]
#> [1] 53.5 47.6
#> 
#> [[3]]
#> [1] 231 715 515
#> 
# 9.4.6 Exercises 
cyls <- split(mtcars, mtcars$cyl)
paths <- file.path(temp, paste0("cyl-", names(cyls), ".csv"))
walk2(cyls, paths, write.csv)

trans <- list(
  disp = function(x) x * 0.0163871,
  am = function(x) factor(x, labels = c("auto", "manual"))
)

nm <- names(trans)
mtcars[nm] <- map2(trans, mtcars[nm], function(f, var) f(var))

mtcars[nm] <- map(nm, ~ trans[[.x]](mtcars[[.x]]))

# 9.5 Reduce family 
# 9.5.1 Basics 
l <- map(1:4, ~ sample(1:10, 15, replace = T))
str(l)
#> List of 4
#>  $ : int [1:15] 7 1 8 8 3 8 2 4 7 10 ...
#>  $ : int [1:15] 3 1 10 2 5 2 9 8 5 4 ...
#>  $ : int [1:15] 6 10 9 5 6 7 8 6 10 8 ...
#>  $ : int [1:15] 9 8 6 4 4 5 2 9 9 6 ...
#>  
out <- l[[1]]
out <- intersect(out, l[[2]])
out <- intersect(out, l[[3]])
out <- intersect(out, l[[4]])
out
#> [1] 8 4
#> 
reduce(l, intersect)
#> [1] 8 4
#> 
reduce(l, union)
#>  [1]  7  1  8  3  2  4 10  5  9  6
#>  
simple_reduce <- function(x, f) {
  out <- x[[1]]
  for (i in seq(2, length(x))) {
    out <- f(out, x[[i]])
  }
  out
}

# 9.5.2 Accumulate 
accumulate(l, intersect)
#> [[1]]
#>  [1]  7  1  8  8  3  8  2  4  7 10 10  3  7 10 10
#> 
#> [[2]]
#> [1]  1  8  3  2  4 10
#> 
#> [[3]]
#> [1]  8  4 10
#> 
#> [[4]]
#> [1] 8 4
#> 
x <- c(4, 3, 10)
reduce(x, `+`)
#> [1] 17

accumulate(x, `+`)
#> [1]  4  7 17
#> 
# 9.5.3 Output types 
reduce(1, `+`)
#> [1] 1
#> 
reduce("a", `+`)
#> [1] "a"
#> 
reduce(integer(), `+`, .init = 0)
#> [1] 0
#> 
sum(integer())  # x + 0 = x
#> [1] 0
prod(integer()) # x * 1 = x
#> [1] 1
min(integer())  # min(x, Inf) = x
#> [1] Inf
max(integer())  # max(x, -Inf) = x
#> [1] -Inf
#> 
#  9.6 Predicate functionals 
# 9.6.1 Basics 
df <- data.frame(x = 1:3, y = c("a", "b", "c"))
detect(df, is.factor)
#> NULL
detect_index(df, is.factor)
#> [1] 0

str(keep(df, is.factor))
#> 'data.frame':    3 obs. of  0 variables
str(discard(df, is.factor))
#> 'data.frame':    3 obs. of  2 variables:
#>  $ x: int  1 2 3
#>  $ y: chr  "a" "b" "c"
# 9.6.2 Map variants 
df <- data.frame(
  num1 = c(0, 10, 20),
  num2 = c(5, 6, 7),
  chr1 = c("a", "b", "c"),
  stringsAsFactors = FALSE
)

str(map_if(df, is.numeric, mean))
#> List of 3
#>  $ num1: num 10
#>  $ num2: num 6
#>  $ chr1: chr [1:3] "a" "b" "c"
str(modify_if(df, is.numeric, mean))
#> 'data.frame':    3 obs. of  3 variables:
#>  $ num1: num  10 10 10
#>  $ num2: num  6 6 6
#>  $ chr1: chr  "a" "b" "c"
str(map(keep(df, is.numeric), mean))
#> List of 2
#>  $ num1: num 10
#>  $ num2: num 6
#>  
# 9.6.3 Exercises 
simple_reduce <- function(x, f) {
  out <- x[[1]]
  for (i in seq(2, length(x))) {
    out <- f(out, x[[i]])
  }
  out
}

scale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

# 9.7 Base functionals 
# 9.7.1 Matrices and arrays 

a2d <- matrix(1:20, nrow = 5)
apply(a2d, 1, mean)
#> [1]  8.5  9.5 10.5 11.5 12.5
apply(a2d, 2, mean)
#> [1]  3  8 13 18
#> 
a3d <- array(1:24, c(2, 3, 4))
apply(a3d, 1, mean)
#> [1] 12 13
apply(a3d, c(1, 2), mean)
#>      [,1] [,2] [,3]
#> [1,]   10   12   14
#> [2,]   11   13   15
#> 
a1 <- apply(a2d, 1, identity)
identical(a2d, a1)
#> [1] FALSE

a2 <- apply(a2d, 2, identity)
identical(a2d, a2)
#> [1] TRUE
#> 
df <- data.frame(x = 1:3, y = c("a", "b", "c"))
apply(df, 2, mean)
#> Warning in mean.default(newX[, i], ...): argument is not numeric or logical:
#> returning NA

#> Warning in mean.default(newX[, i], ...): argument is not numeric or logical:
#> returning NA
#>  x  y 
#> NA NA
#> 
# 9.7.2 Mathematical concerns 
integrate(sin, 0, pi)
#> 2 with absolute error < 2.2e-14
str(uniroot(sin, pi * c(1 / 2, 3 / 2)))
#> List of 5
#>  $ root      : num 3.14
#>  $ f.root    : num 1.22e-16
#>  $ iter      : int 2
#>  $ init.it   : int NA
#>  $ estim.prec: num 6.1e-05
str(optimise(sin, c(0, 2 * pi)))
#> List of 2
#>  $ minimum  : num 4.71
#>  $ objective: num -1
str(optimise(sin, c(0, pi), maximum = TRUE))
#> List of 2
#>  $ maximum  : num 1.57
#>  $ objective: num 1
#>  
