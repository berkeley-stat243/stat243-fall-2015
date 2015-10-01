#########################################################
### Demo code for Unit 7 of Stat243, "Computer numbers"
### Chris Paciorek, October 2015
#########################################################

############################
# 1: Basic representations
############################

## @knitr char-bits

library(pryr)
bits('a')
bits('b')

bits('0')
bits('1')

bits('@')

## @knitr bits

library(pryr)
bits(0)
bytes(0)

bits(1L)
bytes(1L)

bits(2L)
bytes(2L)

bits(-1L)
bytes(-1L)

## @knitr not-closed

a <- as.integer(3423333)  # 3423333L
a * a

## @knitr storage

doubleVec <- rnorm(100000)
intVec <- 1:100000
set.seed(0)
charVec <- sample(letters, 100000, replace = TRUE)
object.size(doubleVec)
object.size(intVec) # so how many bytes per integer in R?
object.size(charVec) 
.Internal(inspect(charVec)) # anything jump out at you?

## @knitr int-max-bits
bits(.Machine$integer.max)
bits(-.Machine$integer.max)
bits(-1L)

## @knitr

############################
# 2: Floating point basics
############################

### 2.1 Representing real numbers

## @knitr imprecision

0.3 - 0.2 == 0.1
0.3
0.2
0.1 # Hmmm...
options(digits=22)  # on some machines this may not have
                    # have any effect in showing more digits...
                    # should work on BCE
a <- 0.3
b <- 0.2
a
b
a - b 
0.1
1/3
# so empirically, it looks like we're accurate up to the
#  16th decimal place

## @knitr machine-precision

1e-16 + 1
1e-15 + 1
2e-16 + 1
.Machine$double.eps
.Machine$double.eps + 1

## @knitr bits-floating

bits(2^(-1)) # 1/2
bits(2^0)  # 1
bits(2^1)  # 2
bits(2^2)  # 4

bits(-2)

## @knitr what-exact

.1
.5
.25
.26
1/32
1/33

## @knitr

### 2.2 Overflow and underflow

## @knitr double-max

log10(2^1024) # whoops ... we've actually just barely overflowed
log10(2^1023)

.Machine$double.xmax
.Machine$double.xmin

## @knitr

### 2.3 Integers or floats

## @knitr ints-represent

x <- 2^45
z <- 25
class(x)
class(z)
as.integer(x)
as.integer(z)

1e308
1e309

2^31
2147483647L
2147483648L

## @knitr force-ints

x <- 3; typeof(x)
x <- as.integer(3); typeof(x)
x <- 3L; typeof(x)
x <- 3:5; typeof(x)

## @knitr

### 2.4 Precision

## @knitr precision

options(digits = 22)
# large vs. small numbers
.1234123412341234 
1234.1234123412341234 # not accurate to 16 places 
123412341234.123412341234 # only accurate to 4 places 
1234123412341234.123412341234 # no places! 
12341234123412341234 # fewer than no places! 

## @knitr precision-implications

# How precision affects calculations
1234567812345678 - 1234567812345677
12345678123456788888 - 12345678123456788887
12345678123456780000 - 12345678123456770000
.1234567812345678 - .1234567812345677
.12345678123456788888 - .12345678123456788887
.00001234567812345678 - .00001234567812345677 
# not as close as we'd expect, should be 1e-20
.000012345678123456788888 - .000012345678123456788887

123456781234 - .0000123456781234 # the correct answer is 123456781233.99998765....

## @knitr spacing-example

1000000.1

## @knitr spacing-ints
2^52
2^52+1
2^53
2^53+1
2^53+2
2^54
2^54+2
2^54+4

bits(2^53)
bits(2^53+1)
bits(2^53+2)
bits(2^54)
bits(2^54+2)
bits(2^54+4)

## @knitr spacing-doubles

0.1234567812345678
0.12345678123456781
0.12345678123456782
0.12345678123456783
0.12345678123456784

bits(0.1234567812345678)
bits(0.12345678123456781)
bits(0.12345678123456782)
bits(0.12345678123456783)
bits(0.12345678123456784)

## @knitr

### 2.5 Working with higher precision numbers

## @knitr higher-precision

require(Rmpfr)
piLong <- Const("pi", prec = 260) # pi "computed" to correct 260-bit precision 
piLong # nicely prints 80 digits 
mpfr(".1234567812345678", 40)
mpfr(".1234567812345678", 80)
mpfr(".1234567812345678", 600)

## @knitr

##################################################
# 3: Implications for calculation and comparisons
##################################################

### 3.1 Computer arithmetic is not mathematical arithmetic!

## @knitr non-associative

val1 <- 1/10; val2 <- 0.31; val3 <- 0.57
res1 <- val1*val2*val3
res2 <- val3*val2*val1
identical(res1, res2)
res1
res2

## @knitr

### 3.3 Comparisons

## @knitr comparisons

4L - 3L == 1L
4.0 - 3.0 == 1.0
4.1 - 3.1 == 1.0

## @knitr approx-equality

a = 12345678123456781000
b = 12345678123456782000

approxEqual = function(a, b){
  if(abs(a - b) < .Machine$double.eps * abs(a + b))
    print("approximately equal") else print ("not equal")
}

approxEqual(a,b)

a = 1234567812345678
b = 1234567812345677

approxEqual(a,b)   

## @knitr

### 3.4 Calculations

## @knitr catastrophic-cancel

# catastrophic cancellation w/ large numbers
123456781234.56 - 123456781234.00
# how many accurate decimal places?

## @knitr catastrophic-cancel-small

# catastrophic cancellation w/ small numbers
a = .000000000000123412341234
b = .000000000000123412340000

# so we know the right answer is .000000000000000000001234 EXACTLY

a-b

## @knitr real-catastrophe

x <- c(-1, 0, 1) + 1e8
n <- length(x)
sum(x^2)-n*mean(x)^2 # that's not good!
sum((x - mean(x))^2)

## @knitr magnitude-diff

123456781234 - 0.000001

## @knitr

# part of the predictive density calculation example:
exp(-1000)

## @knitr numerical-linalg

xs <- 1:100
dists <- rdist(xs)
corMat <- exp(- (dists/10)^2) # this is a p.d. matrix (mathematically)
eigen(corMat)$values[80:100]  # but not numerically


