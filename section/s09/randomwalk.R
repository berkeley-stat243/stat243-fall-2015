## @knitr q4.sol
makeWalk <- function(n, fullpath = TRUE){
  # using cumsum() to avoid loops
  if(length(n) > 1 || !is.numeric(n) ||  n %% 1 != 0 || n < 1)
    stop("rw_fast: 'n' must be a positive integer")
  updown <- sample(c(0, 1), n, replace = TRUE)
  steps <-  sample(c(-1, 1), n, replace = TRUE)
  xsteps <- (updown == 0) * steps
  ysteps <- (updown == 1) * steps
  x <- c(0, cumsum(xsteps))
  y <- c(0, cumsum(ysteps))
  if(fullpath){
    return(cbind(x, y))
  } else{
    return(c(x[n+1], y[n+1]))
  }
}

## @knitr q4class.sol
rw <- function(n = 100, start = c(0, 0), fullpath = TRUE){

  path <- makeWalk(n, fullpath)
  if(fullpath){
    if(!identical(start, c(0, 0)))
      path <- t(t(path) + start)
  } else{ # don't bother to check if start is non-zero
          #   as time to add is trivial
    path <- path + start
  }

  obj <- list(path = path, length = n+1)
  class(obj) <- "rw"
  return(obj)
}

print.rw <- function(obj){
  cat("Random walk of length ", obj$length, ".\n", sep = "")
  invisible(obj) # following template of print.lm()
}

# user can pass in additional args through the ... mechanism
plot.rw <- function(obj, type = "l", main = "Random walk", xlab = "x", ylab = "y", ...){
  plot(x = obj$path[ , 1], y = obj$path[ , 2], main = main,
       xlab = xlab, ylab = ylab, type = type, ...)
}

`[.rw` <- function(obj, i){
  return(obj$path[i, ])
}

# generic start replacement function
`start<-` <- function(x, ...){
  UseMethod("start<-")
}

`start<-.rw` <- function(obj, value){
  if(is.numeric(value) && length(value) == 2 && sum(value%%1) == 0){
    obj$path <- t(t(obj$path) - obj$path[1] + value)
    # note use of [.rw operator
  } else{
    stop("Value argument needs to be a vector of integers of length 2")
  }
  return(obj)
}

out <- rw(100)
out
out[5]
plot(out)

