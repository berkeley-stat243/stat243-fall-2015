## @knitr environments

modify_argument <- function(x) {
  x$x <- 10
}

a_list <- list(x = 3)
an_environment <- new.env()
an_environment$x <- 3

modify_argument(a_list)
modify_argument(an_environment)
