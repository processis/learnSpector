my_delta_data <- sample(1000)
usethis::use_data(my_delta_data)

lobstr::mem_used()
#> 58.74 MB
library(nycflights13)
lobstr::mem_used()
#> 60.49 MB

invisible(flights)
lobstr::mem_used()
#> 101.20 MB
#>
usethis::use_data_raw()

usethis::use_data_raw("my_delta_data")


usethis::use_data(internal_this, internal_that, internal = TRUE)

system.file("extdata", package = "readxl") |> list.files()
#>  [1] "clippy.xls"    "clippy.xlsx"   "datasets.xls"  "datasets.xlsx"
#>  [5] "deaths.xls"    "deaths.xlsx"   "geometry.xls"  "geometry.xlsx"
#>  [9] "type-me.xls"   "type-me.xlsx"

system.file("extdata", "clippy.xlsx", package = "readxl")
#> [1] "/home/runner/work/_temp/Library/readxl/extdata/clippy.xlsx"
#>
system.file("extdata", "I_do_not_exist.csv", package = "readr")
#> [1] ""
#>
fs::path_package("extdata", package = "idonotexist")
#> Error: Can't find package `idonotexist` in library locations:
#>   - '/home/runner/work/_temp/Library'
#>   - '/opt/R/4.5.0/lib/R/site-library'
#>   - '/opt/R/4.5.0/lib/R/library'

fs::path_package("extdata", "I_do_not_exist.csv", package = "readr")
#> Error: File(s) '/home/runner/work/_temp/Library/readr/extdata/I_do_not_exist.csv' do not exist

fs::path_package("extdata", "chickens.csv", package = "readr")
#> /home/runner/work/_temp/Library/readr/extdata/chickens.csv
#>

readxl_example <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package = "readxl"))
  } else {
    system.file("extdata", path, package = "readxl", mustWork = TRUE)
  }
}

readxl::readxl_example()
#>  [1] "clippy.xls"    "clippy.xlsx"   "datasets.xls"  "datasets.xlsx"
#>  [5] "deaths.xls"    "deaths.xlsx"   "geometry.xls"  "geometry.xlsx"
#>  [9] "type-me.xls"   "type-me.xlsx"

readxl::readxl_example("clippy.xlsx")
#> [1] "/home/runner/work/_temp/Library/readxl/extdata/clippy.xlsx"
#>

favorite_letters <- letters[1:3]

#' Report my favorite letters
#' @export
mfl <- function() {
  favorite_letters
}

#' Change my favorite letters
#' @export
set_mfl <- function(l = letters[24:26]) {
  old <- favorite_letters
  favorite_letters <<- l
  invisible(old)
}

mfl()
#> [1] "a" "b" "c"

set_mfl(c("j", "f", "b"))
#> Error in set_mfl() :
#>   cannot change value of locked binding for 'favorite_letters'
#>
#>
#>
the <- new.env(parent = emptyenv())
the$favorite_letters <- letters[1:3]

#' Report my favorite letters
#' @export
mfl2 <- function() {
  the$favorite_letters
}

#' Change my favorite letters
#' @export
set_mfl2 <- function(l = letters[24:26]) {
  old <- the$favorite_letters
  the$favorite_letters <- l
  invisible(old)
}


mfl2()
#> [1] "a" "b" "c"

set_mfl2(c("j", "f", "b"))

mfl2()
#> [1] "j" "f" "b"
#>
#>
#>
tools::R_user_dir("delta", which = "data")
#> [1] "/home/runner/.local/share/R/pkg"
tools::R_user_dir("delta", which = "config")
#> [1] "/home/runner/.config/R/pkg"
tools::R_user_dir("delta", which = "cache")
#> [1] "/home/runner/.cache/R/pkg"
