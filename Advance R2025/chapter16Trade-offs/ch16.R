# 16 Trade-offs 

# 16.3.1 Namespacing 

#plot(data)       # plot some data
#plot(bank_heist) # plot a crime
#plot(land)       # create a new plot of land
#plot(movie)      # extract plot of a movie


new_stack <- function(items = list()) {
  structure(list(items = items), class = "stack")
}

push <- function(x, y) {
  x$items <- c(x$items, list(y))
  x
}

pop <- function(x) {
  n <- length(x$items)
  
  item <- x$items[[n]]
  x$items <- x$items[-n]
  
  list(item = item, x = x)
}

s <- new_stack()
s <- push(s, 10)
s <- push(s, 20)

out <- pop(s)
out$item
#> [1] 20
s <- out$x
s
#> $items
#> $items[[1]]
#> [1] 10
#> 
#> 
#> attr(,"class")
#> [1] "stack"

library(zeallot)

c(value, s) %<-% pop(s)
value
#> [1] 10

Stack <- R6::R6Class("Stack", list(
  items = list(),
  push = function(x) {
    self$items <- c(self$items, x)
    invisible(self)
  },
  pop = function() {
    item <- self$items[[self$length()]]
    self$items <- self$items[-self$length()]
    item
  },
  length = function() {
    length(self$items)
  }
))

s <- Stack$new()
s$push(10)
s$push(20)
s$pop()
#> [1] 20
#> 
#> 
# 16.3.3 Method chaining 
s <- Stack$new()
s$
  push(10)$
  push(20)$
  pop()
#> [1] 20