#7.3.1Tibbles
x <- seq(from =0,to=2*pi,length.out=100)
s <- sin(x)
c <- cos(x)
z <- s + c
plot(x, z, type="l",col="red",lwd=7)
lines(x, c, col="blue",lwd=1.5)
lines(x, s, col="darkolivegreen",lwd=1.5)

x <- seq(from = 0, to = 2 * pi, length.out = 100)
#df <- as.data.frame((x))
df <- rbind(as.data.frame((x)),cos(x),sin(x), cos(x) + sin(x))
# plot etc.

library(tidyverse)
x <- seq(from = 0, to = 2 * pi, length.out = 100)
tb <- tibble(x, sin(x), cos(x), cos(x) + sin(x))


# Note how concise and relevant the output is:
print(tb)
# This does the same as for a data-frame:
plot(tb)
# Actually a tibble will still behave as a data frame:
is.data.frame(tb)

tb$`sin(x)`[1]

tb<- tibble(`1` = 1:3, `2` = sin(`1`), `1` * pi, 1 * pi)
tb

# -- data frame --
df <- data.frame("value" = pi, "name" = "pi")
df$na
# partial matching of column names
# automatic conversion to factor, plus data frame
# accepts strings:
df[,"name"]
df[,c("name", "value")]
df <- tibble("value" = pi, "name" = "pi")
df$name

df[,"name"]
# this returns a tibble (no simplification)

df[,c("name", "value")] # no conversion to factor

tb <- tibble(c("a", "b", "c"), c(1,2,3), 9L,9)
is.data.frame(tb)
# Note also that tibble did no conversion to factors, and
# note that the tibble also recycles the scalars:
tb

# Coerce the tibble to data-frame:
as.data.frame(tb)

# A tibble does not recycle shorter vectors, so this fails:
fail <- tibble(c("a", "b", "c"), c(1,2))