##########################################################################
### Demo code for Unit 12 of Stat243, "Integration and Differentiation"
### Chris Paciorek, November 2015
##########################################################################

#########################################
# 1: Differentiation
#########################################

### 1.1 Numerical differentiation

## @knitr diff

## compute first and second derivatives of log(gamma(x)) at x=1/2
options(digits = 9, width = 120)
h <- 10^(-(1:15)) 
x <- 1/2
fx <- lgamma(x)
## targets: actual derivatives can be computed very accurately
## using built-in R functions:
digamma(x) # accurate first derivative
trigamma(x) # accurate second derivative
## calculate discrete differences
fxph <- lgamma(x+h)
fxmh <- lgamma(x-h)
fxp2h <- lgamma(x+2*h)
fxm2h <- lgamma(x-2*h)
## now find numerical derivatives
fp1 <- (fxph - fx)/h # forward difference
fp2 <- (fxph - fxmh)/(2*h) # central difference
## second derivatives 
fpp1 <- (fxp2h - 2*fxph + fx)/(h*h) # forward difference
fpp2 <- (fxph - 2*fx + fxmh)/(h*h) # central difference
## table of results
cbind(h,fp1,fp2,fpp1,fpp2)

## @knitr dummy

### 1.2 Numerical differentiation in R

## @knitr numericDeriv
x <- 1/2 
numericDeriv(quote(lgamma(x)), "x") 

## @knitr dummy

### 1.3 Symbolic differentiation

## @knitr symbolic-diff

deriv(quote(atan(x)), "x")  # derivative of simple expression
## derivative of a function; note we need to pass in an expression, 
## not the entire function
f <- function(x,y) sin(x * y)+x^3+exp(y)
newBody <- deriv(body(f), c("x", "y"), hessian = TRUE)
## now create a new version of f that provides gradient 
## and hessian as attributes of the output, 
## in addition to the function value as the return value
f <- function(x, y) {} # function template
body(f) <- newBody
## try out the new function
f(3,1)
attr(f(3,1), "gradient")
attr(f(3,1), "hessian")

## @knitr dummy

### symbolic differentiation in Mathematica

### first partial derivative wrt x
# D[ Exp[x^n] - Cos[x y], x]
### second partial derivative
# D[ Exp[x^n] - Cos[x y], {x, 2}]
### partials
# D[ Exp[x^n] - Cos[x y], x, y]
### trig function example
# D[ ArcTan[x], x] 


#########################################
# 2: Integration
#########################################

## @knitr numeric-integ-example

f <- function(x) sin(x)
## mathematically, the integral from 0 to pi is 2
## quadrature through integrate()
integrate(f, 0, pi)
system.time(integrate(f, 0, pi))
## MC estimate
ninteg <- function(n) mean(sin(runif(n, 0, pi))*pi)
n <- 1000
ninteg(n)
system.time(ninteg(n))
n <- 10000
ninteg(n)
system.time(ninteg(n))
n <- 1000000
ninteg(n)
system.time(ninteg(n))
## that was fairly slow, 
## especially if you need to do a lot of individual integrals

## @knitr dummy

### 2.2 Numerical integration in R

## @knitr numeric-integ
integrate(dnorm, -Inf, Inf, 0, .1)
integrate(dnorm, -Inf, Inf, 0, .001)
integrate(dnorm, -Inf, Inf, 0, .0001) # THIS FAILS!

## @knitr dummy

### 2.3 Singularities and infinite ranges

## @knitr singularity

## doing it directly with integrate()
f <- function(x)   
	exp(x)/sqrt(x)
integrate(f, 0, 1) 
## subtracting off the singularity
f <- function(x) (exp(x) - 1)/sqrt(x)
x <- seq(0,1, len = 200) 
integrate(f, 0, 1)

## analytic change of variables, followed by numeric integration 
f <- function(u)   
	2*exp(u^2)
integrate(f, 0, 1) 

### 2.4 Symbolic integration

### in Mathematica
### one-dimensional integration
# Integrate[Sin[x]^2, x]
### two-dimensional integration
# Integrate[Sin[x] Exp[-y^2], x, y]
