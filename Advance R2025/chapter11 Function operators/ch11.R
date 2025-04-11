#Function operators
# 11.1 Introduction 
chatty <- function(f) {
  force(f)
  
  function(x, ...) {
    res <- f(x, ...)
    cat("Processing ", x, "\n", sep = "")
    res
  }
}
f <- function(x) x ^ 2
s <- c(3, 2, 1)

purrr::map_dbl(s, chatty(f))
#> Processing 3
#> Processing 2
#> Processing 1
#> [1] 9 4 1
#> 
#> 
library(purrr)
library(memoise)

# 11.2 Existing function operators 
# 11.2.1 Capturing errors with purrr::safely() 

x <- list(
  c(0.512, 0.165, 0.717),
  c(0.064, 0.781, 0.427),
  c(0.890, 0.785, 0.495),
  "oops"
)

out <- rep(NA_real_, length(x))
for (i in seq_along(x)) {
  out[[i]] <- sum(x[[i]])
}
#> Error in sum(x[[i]]): invalid 'type' (character) of argument
out
#> [1] 1.39 1.27 2.17   NA
#> 
#> 
safe_sum <- safely(sum)
safe_sum
#> function (...) 
#> capture_error(.f(...), otherwise, quiet)
#> <bytecode: 0x7fafd9e2de58>
#> <environment: 0x7fafd9e2d9c0>
#> 
#> 
str(safe_sum(x[[1]]))
#> List of 2
#>  $ result: num 1.39
#>  $ error : NULL
str(safe_sum(x[[4]]))
#> List of 2
#>  $ result: NULL
#>  $ error :List of 2
#>   ..$ message: chr "invalid 'type' (character) of argument"
#>   ..$ call   : language .Primitive("sum")(..., na.rm = na.rm)
#>   ..- attr(*, "class")= chr [1:3] "simpleError" "error" "condition"
#>   
#>   
out <- map(x, safely(sum))
str(out)
#> List of 4
#>  $ :List of 2
#>   ..$ result: num 1.39
#>   ..$ error : NULL
#>  $ :List of 2
#>   ..$ result: num 1.27
#>   ..$ error : NULL
#>  $ :List of 2
#>   ..$ result: num 2.17
#>   ..$ error : NULL
#>  $ :List of 2
#>   ..$ result: NULL
#>   ..$ error :List of 2
#>   .. ..$ message: chr "invalid 'type' (character) of argument"
#>   .. ..$ call   : language .Primitive("sum")(..., na.rm = na.rm)
#>   .. ..- attr(*, "class")= chr [1:3] "simpleError" "error" "condition"
#>   
#>   
out <- transpose(map(x, safely(sum)))
str(out)
#> List of 2
#>  $ result:List of 4
#>   ..$ : num 1.39
#>   ..$ : num 1.27
#>   ..$ : num 2.17
#>   ..$ : NULL
#>  $ error :List of 4
#>   ..$ : NULL
#>   ..$ : NULL
#>   ..$ : NULL
#>   ..$ :List of 2
#>   .. ..$ message: chr "invalid 'type' (character) of argument"
#>   .. ..$ call   : language .Primitive("sum")(..., na.rm = na.rm)
#>   .. ..- attr(*, "class")= chr [1:3] "simpleError" "error" "condition"
#>   
#>   
ok <- map_lgl(out$error, is.null)
ok
#> [1]  TRUE  TRUE  TRUE FALSE

x[!ok]
#> [[1]]
#> [1] "oops"

out$result[ok]
#> [[1]]
#> [1] 1.39
#> 
#> [[2]]
#> [1] 1.27
#> 
#> [[3]]
#> [1] 2.17
#> 
#> 
fit_model <- function(df) {
  glm(y ~ x1 + x2 * x3, data = df)
}

models <- transpose(map(datasets, safely(fit_model)))
ok <- map_lgl(models$error, is.null)

# which data failed to converge?
datasets[!ok]

# which models were successful?
models[ok]


# 11.2.2 Caching computations with memoise::memoise() 

slow_function <- function(x) {
  Sys.sleep(1)
  x * 10 * runif(1)
}
system.time(print(slow_function(1)))
#> [1] 0.808
#>    user  system elapsed 
#>   0.000   0.001   1.120

system.time(print(slow_function(1)))
#> [1] 8.34
#>    user  system elapsed 
#>   0.003   0.000   1.019
#>   
#>   
fast_function <- memoise::memoise(slow_function)
system.time(print(fast_function(1)))
#> [1] 6.01
#>    user  system elapsed 
#>   0.001   0.000   1.003

system.time(print(fast_function(1)))
#> [1] 6.01
#>    user  system elapsed 
#>    0.02    0.00    0.02
#>    
#>    
fib <- function(n) {
  if (n < 2) return(1)
  fib(n - 2) + fib(n - 1)
}
system.time(fib(23))
#>    user  system elapsed 
#>   0.049   0.002   0.050
system.time(fib(24))
#>    user  system elapsed 
#>   0.069   0.002   0.070
#>   
#>   
fib2 <- memoise::memoise(function(n) {
  if (n < 2) return(1)
  fib2(n - 2) + fib2(n - 1)
})
system.time(fib2(23))
#>    user  system elapsed 
#>   0.009   0.000   0.008
#>   
#>   
system.time(fib2(24))
#>    user  system elapsed 
#>   0.001   0.000   0.000
#>   
#>   
#11.3 Case study: Creating your own function operators
urls <- c(
  "adv-r" = "https://adv-r.hadley.nz", 
  "r4ds" = "http://r4ds.had.co.nz/"
  # and many many more
)
path <- paste(tempdir(), names(urls), ".html")

walk2(urls, path, download.file, quiet = TRUE)

for(i in seq_along(urls)) {
  Sys.sleep(0.1)
  if (i %% 10 == 0) cat(".")
  download.file(urls[[i]], paths[[i]])
}



delay_by <- function(f, amount) {
  force(f)
  force(amount)
  
  function(...) {
    Sys.sleep(amount)
    f(...)
  }
}
system.time(runif(100))
#>    user  system elapsed 
#>       0       0       0
system.time(delay_by(runif, 0.1)(100))
#>    user  system elapsed 
#>    0.00    0.00    0.13
#>    
#>    
#>    
walk2(urls, path, delay_by(download.file, 0.1), quiet = TRUE)


dot_every <- function(f, n) {
  force(f)
  force(n)
  
  i <- 0
  function(...) {
    i <<- i + 1
    if (i %% n == 0) cat(".")
    f(...)
  }
}
walk(1:100, runif)
walk(1:100, dot_every(runif, 10))
#> ..........
#> 
#> 
walk2(
  urls, path, 
  dot_every(delay_by(download.file, 0.1), 10), 
  quiet = TRUE
)

walk2(
  urls, path, 
  download.file %>% dot_every(10) %>% delay_by(0.1), 
  quiet = TRUE
)


