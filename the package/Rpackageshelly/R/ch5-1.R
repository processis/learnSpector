
#ch5-1
infile <- "swim.csv"
(dat <- read.csv(infile))

dat$english[dat$where == "beach"] <- "US"
dat$english[dat$where == "coast"] <- "US"
dat$english[dat$where == "seashore"] <- "UK"
dat$english[dat$where == "seaside"] <- "UK"

dat$temp[dat$english == "US"] <- (dat$temp[dat$english == "US"] - 32) * 5/9
dat


now <- Sys.time()
timestamp <- format(now, "%Y-%B-%d_%H-%M-%S")
(outfile <- paste0(timestamp, "_", sub("(.*)([.]csv$)", "\\1_clean\\2", infile)))
#> [1] "2025-April-06_07-14-45_swim_clean.csv"
write.csv(dat, file = outfile, quote = FALSE, row.names = FALSE)


#ch5-2

library(tidyverse)

infile <- "swim.csv"
dat <- read_csv(infile, col_types = cols(name = "c", where = "c", temp = "d"))

lookup_table <- tribble(
  ~where, ~english,
  "beach",     "US",
  "coast",     "US",
  "seashore",     "UK",
  "seaside",     "UK"
)

dat <- dat %>%
  left_join(lookup_table)

f_to_c <- function(x) (x - 32) * 5/9

dat <- dat %>%
  mutate(temp = if_else(english == "US", f_to_c(temp), temp))
dat

now <- Sys.time()
timestamp <- function(time) format(time, "%Y-%B-%d_%H-%M-%S")
outfile_path <- function(infile) {
  paste0(timestamp(now), "_", sub("(.*)([.]csv$)", "\\1_clean\\2", infile))
}
write_csv(dat, outfile_path(infile))


#CH5-3
library(tidyverse)

localize_beach <- function(dat) {
  lookup_table <- read_csv(
    "beach-lookup-table.csv",
    col_types = cols(where = "c", english = "c")
  )
  left_join(dat, lookup_table)
}

f_to_c <- function(x) (x - 32) * 5/9

celsify_temp <- function(dat) {
  mutate(dat, temp = if_else(english == "US", f_to_c(temp), temp))
}

now <- Sys.time()
timestamp <- function(time) format(time, "%Y-%B-%d_%H-%M-%S")
outfile_path <- function(infile) {
  paste0(timestamp(now), "_", sub("(.*)([.]csv$)", "\\1_clean\\2", infile))
}





library(tidyverse)
source("/home/user/Rpackagetest/Rpackageshelly/R/cleaning-helpers.R")

infile <- "swim.csv"
dat <- read_csv(infile, col_types = cols(name = "c", where = "c", temp = "d"))

(dat <- dat %>%
    localize_beach() %>%
    celsify_temp())

write_csv(dat, outfile_path(infile))






library(tidyverse)
library(delta)

infile <- "swim.csv"
dat <- read_csv(infile, col_types = cols(name = "c", where = "c", temp = "d"))

dat <- dat %>%
  localize_beach() %>%
  celsify_temp()

write_csv(dat, outfile_path(infile))





library(tidyverse)
library(delta)

infile <- "swim.csv"
dat <- read_csv(infile, col_types = cols(name = "c", where = "c", temp = "d"))

dat <- dat %>%
  localize_beach() %>%
  celsify_temp()
#> Error in localize_beach(.) : could not find function "localize_beach"

write_csv(dat, outfile_path(infile))
#> Error in outfile_path(infile) : could not find function "outfile_path"





#ch5-6
lookup_table <- dplyr::tribble(
  ~where, ~english,
  "beach",     "US",
  "coast",     "US",
  "seashore",     "UK",
  "seaside",     "UK"
)

#' @export
localize_beach <- function(dat) {
  dplyr::left_join(dat, lookup_table)
}

f_to_c <- function(x) (x - 32) * 5/9

#' @export
celsify_temp <- function(dat) {
  dplyr::mutate(dat, temp = dplyr::if_else(english == "US", f_to_c(temp), temp))
}

now <- Sys.time()
timestamp <- function(time) format(time, "%Y-%B-%d_%H-%M-%S")

#' @export
outfile_path <- function(infile) {
  paste0(timestamp(now), "_", sub("(.*)([.]csv$)", "\\1_clean\\2", infile))
}



#ch5-6
Sys.time()


outfile_path("INFILE.csv")


now <- Sys.time()
timestamp <- function(time) format(time, "%Y-%B-%d_%H-%M-%S")
outfile_path <- function(infile) {
  paste0(timestamp(now), "_", sub("(.*)([.]csv$)", "\\1_clean\\2", infile))
}



lookup_table <- dplyr::tribble(
  ~where, ~english,
  "beach",     "US",
  "coast",     "US",
  "seashore",     "UK",
  "seaside",     "UK"
)

now <- Sys.time()
timestamp <- function(time) format(time, "%Y-%B-%d_%H-%M-%S")
outfile_path <- function(infile) {
  paste0(timestamp(now), "_", sub("(.*)([.]csv$)", "\\1_clean\\2", infile))
}


# always timestamp as "now"
outfile_path <- function(infile) {
  ts <- timestamp(Sys.time())
  paste0(ts, "_", sub("(.*)([.]csv$)", "\\1_clean\\2", infile))
}

# allow user to provide a time, but default to "now"
outfile_path <- function(infile, time = Sys.time()) {
  ts <- timestamp(time)
  paste0(ts, "_", sub("(.*)([.]csv$)", "\\1_clean\\2", infile))
}


#ch5-7

format(Sys.time(), "%Y-%B-%d_%H-%M-%S")
#> [1] "2025-April-06_07-14-45"



timestamp <- function(time = Sys.time()) {
  Sys.setlocale("LC_TIME", "C")
  Sys.setenv(TZ = "UTC")
  format(time, "%Y-%B-%d_%H-%M-%S")
}

format(Sys.time(), "%Y-%B-%d_%H-%M-%S")

outfile_path("INFILE.csv")
#> [1] "2025-April-06_07-14-45_INFILE_clean.csv"

format(Sys.time(), "%Y-%B-%d_%H-%M-%S")
#> [1] "2025-April-06_07-14-45"


# use withr::local_*() functions to keep the changes local to timestamp()
timestamp <- function(time = Sys.time()) {
  withr::local_locale(c("LC_TIME" = "C"))
  withr::local_timezone("UTC")
  format(time, "%Y-%B-%d_%H-%M-%S")
}

# use the tz argument to format.POSIXct()
timestamp <- function(time = Sys.time()) {
  withr::local_locale(c("LC_TIME" = "C"))
  format(time, "%Y-%B-%d_%H-%M-%S", tz = "UTC")
}

# put the format() call inside withr::with_*()
timestamp <- function(time = Sys.time()) {
  withr::with_locale(
    c("LC_TIME" = "C"),
    format(time, "%Y-%B-%d_%H-%M-%S", tz = "UTC")
  )
}


