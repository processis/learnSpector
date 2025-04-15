##ch6

#6.4.1
dataTableDependency <- list(
  htmlDependency(
    "datatables", "1.10.2",
    c(file = system.file("www/datatables", package = "shinybootstrap2")),
    script = "js/jquery.dataTables.min.js"
  ),
  htmlDependency(
    "datatables-bootstrap", "1.10.2",
    c(file = system.file("www/datatables", package = "shinybootstrap2")),
    stylesheet = c("css/dataTables.bootstrap.css", "css/dataTables.extra.css"),
    script = "js/dataTables.bootstrap.js"
  )
)


#6.4.2
show_ansi_colors <- function(colors = num_colors()) {
  if (colors < 8) {
    cat("Colors are not supported")
  } else if (colors < 256) {
    cat(ansi_colors_8, sep = "")
    invisible(ansi_colors_8)
  } else {
    cat(ansi_colors_256, sep = "")
    invisible(ansi_colors_256)
  }
}


ansi_colors_8 <- # code to generate a vector covering basic terminal colors
  
  ansi_colors_256 <- # code to generate a vector covering 256 colors
  
  
  
  show_ansi_colors <- function(colors = num_colors()) {
    if (colors < 8) {
      cat("Colors are not supported")
    } else if (colors < 256) {
      cat(ansi_colors_8(), sep = "")
      invisible(ansi_colors_8())
    } else {
      cat(ansi_colors_256(), sep = "")
      invisible(ansi_colors_256())
    }
  }

ansi_colors_8 <- function() {
  # code to generate a vector covering basic terminal colors
}

ansi_colors_256 <- function() {
  # code to generate a vector covering 256 colors
}



#ch6.5
x <- c("bernard", "bérénice", "béatrice", "boris")

withr::with_locale(c(LC_COLLATE = "fr_FR"), sort(x))
#> [1] "béatrice" "bérénice" "bernard"  "boris"
withr::with_locale(c(LC_COLLATE = "C"), sort(x))
#> [1] "bernard"  "boris"    "béatrice" "bérénice"

x <- c("a", "A", "B", "b", "A", "b")

withr::with_locale(c(LC_COLLATE = "en_CA"), sort(x))
#> [1] "a" "A" "A" "b" "b" "B"
withr::with_locale(c(LC_COLLATE = "C"), sort(x))
#> [1] "A" "A" "B" "a" "b" "b"
#> 


##ch6.5.1

f <- function(x, sig_digits) {
  # imagine lots of code here
  withr::with_options(
    list(digits = sig_digits),
    print(x)
  )
  # ... and a lot more code here
}


g <- function(x, sig_digits) {
  withr::local_options(list(digits = sig_digits))
  print(x)
  # imagine lots of code here
}


#ch6.5.4

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.dplyr <- list(
    dplyr.show_progress = TRUE
  )
  toset <- !(names(op.dplyr) %in% names(op))
  if (any(toset)) options(op.dplyr[toset])
  
  invisible()
}


.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to my package")
}