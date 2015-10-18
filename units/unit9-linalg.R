################################################################
### Demo code for Unit 9 of Stat243, "Numerical linear algebra"
### Chris Paciorek, October 2015
################################################################

########################
# 1: Preliminaries
########################

### 1.7 Some vector and matrix properties

## @knitr linalg-syntax
A + B
A %*% B # matrix multiplication
A * B # Hadamard (direct) product

## @knitr

### 1.9 rank, LIN, basis vectors

norm2 <- function(x)
  sqrt(sum(x^2))

# suppose I want to use (1, 1) and (1, -1) as basis vectors in R^2 rather than (1, 0), and (0, 1)

# express (1, 0) in terms of the basis vectors:
x = c(1, 0)
v1 = c(1, 1)
v2 = c(1, -1)
v1 = v1/norm2(v1)
v2 = v2/norm2(v2)

c1 = sum(x*v1)
c2 = sum(x*v2)

c1*v1 + c2*v2  # this should be x


### 1.10 Invertibility, etc.

mat0 = matrix(c(1,2,2,4), 2)  # symmetric, not full rank
mat1 = matrix(c(1,2,4,1), 2)  # not symmetric
mat2 = matrix(c(1,.5,.25,1), 2) # not symmetric
mat3 = matrix(c(1,1,1,1), 2)  # not full rank
mat4 = matrix(c(1,.5,.5,1), 2) # symmetric, full rank

# let's look at the eigendecompositions
eigen(mat0)

eigen(mat4) # numerically positive definite!

### 1.11 Generalized inverses

## @knitr geninv

precMat <- matrix(c(1,-1,0,0,0,-1,2,-1,0,0,0,-1,2,-1,
    0,0,0,-1,2,-1,0,0,0,-1,1), 5)
e <- eigen(precMat)
e$values
e$vectors[ , 5]

## @knitr geninv-realiz

# generate a realization
e$values[1:4] <- 1 / e$values[1:4]
y <- e$vec %*% (sqrt(e$values) * rnorm(5))
sum(y)

## @knitr

###########################
# 2: Computational issues
###########################

### 2.3 Ill-conditioned problems

## @knitr ill-cond

norm2 <- function(x) sqrt(sum(x^2))
A <- matrix(c(10,7,8,7,7,5,6,5,8,6,10,9,7,5,9,10),4)
e <- eigen(A)
b <- c(32, 23, 33, 31)
bPerturb <- c(32.1, 22.9, 33.1, 30.9)
x <- solve(A, b)
xPerturb <- solve(A, bPerturb)
norm2(x - xPerturb)
norm2(b - bPerturb)
norm2(x - xPerturb)/norm2(x)
(e$val[1]/e$val[4])*norm2(b - bPerturb)/norm2(b)

## @knitr improve-cond
x1 <- 1990:2010
x2 <- x1 - 2000 # centered
x3 <- x2/10 # centered and scaled
X1 <- cbind(rep(1, 21), x1, x1^2)
X2 <- cbind(rep(1, 21), x2, x2^2)
X3 <- cbind(rep(1, 21), x3, x3^2)
e1 <- eigen(crossprod(X1))
e1$values
e2 <- eigen(crossprod(X2))
e2$values
e3 <- eigen(crossprod(X3))
e3$values

## @knitr

##################################################
# 3: Matrix factorizations (decompositions) and
#      solving systems of linear equations
##################################################

### 3.1 Triangular systems

n <- 20
X <- crossprod(matrix(rnorm(n^2), n))
b <- rnorm(n)
U <- chol(crossprod(X)) # U is upper-triangular
L <- t(U) # L is lower-triangular
out1 <- backsolve(U, b) 
out2 <- forwardsolve(L, b) 
all.equal(out1, c(solve(U) %*% b)) 
all.equal(out2, c(solve(L) %*% b)) 

# 3.2 LU

# example of the impact of pivoting

# eqn 1:  .0001 x1 + x2 = 1
# eqn 2:        x1 + x2 = 2

A = matrix(c(.0001, 1, 1 ,1), nr = 2, byrow = TRUE)
b = c(1,2)

x = solve(A, b)
options(digits = 16)
print(x)

c1 = -A[2,1]/A[1,1]
A[2 , ] = c1*A[1,] + A[2, ]
b[2] = c1*b[1] + b[2]

# compute solution if numbers only available with 3 digits of accuracy
x3dig = rep(NA, 2)
x3dig[2] = round(b[2], -3)/round(A[2,2], -3) # round to 3 significant digits
x3dig[1] = (b[1] - A[1,2]*x3dig[2])/A[1,1]

print(x3dig) # whoops

# switch the rows

A = matrix(c(1, 1, .0001, 1), nr = 2, byrow = TRUE)
b = c(2,1)

c1 = -A[2,1]/A[1,1]
A[2 , ] = c1*A[1,] + A[2, ]
b[2] = c1*b[1] + b[2]

# solution based on 3 digits of accuracy with pivoting
x3dig = rep(NA, 2)
x3dig[2] = round(b[2], 3)/round(A[2,2], 3) # numbers are now less than 1 - round to 3 decimal places
x3dig[1] = (b[1] - A[1,2]*x3dig[2])/A[1,1]

print(x3dig) # now a good approximation


### 3.3 Cholesky decomposition

backsolve(U, b, transpose = TRUE) 
forwardsolve(L, b, transpose = TRUE)


backsolve(U, backsolve(U, b, transpose = TRUE))
backsolve(U, forwardsolve(t(U), b)) # equivalent but less efficient

U <- chol(A)
y <- crossprod(U, rnorm(n)) # i.e., t(U)%*%rnorm(n), but much faster

# numerically not positive definite
library(fields)
locs <- runif(100)
rho <- .1
C <- exp(-rdist(locs)^2/rho^2)
e <- eigen(C)
e$values[96:100]
U <- chol(C)
vals <- abs(e$values)
max(vals)/min(vals)
U <- chol(C, pivot = TRUE)

# not sure why there are 1s on the diagonal for what should be the
# zero-block induced by the numerical negative definiteness...

# a simpler example

cor <- 0.2
mat <- matrix(c(1,cor,cor,cor,1,1,cor,1,1), 3, byrow = TRUE)
U <- chol(mat, pivot = TRUE)
U
t(U)%*%U
# because the redundancy is in the 3rd dimension,
# we seem to get a numerical zero in the 3,3 position
# but not an imposed exact zero

# now have the redundancy in the 1st/2nd dimensions
mat <- matrix(c(1,1,cor,1,1,cor,cor,cor,1), 3, byrow = TRUE)
U <- chol(mat, pivot = TRUE)
U
# now we see an exact zero, and the effect of the permutation
t(U) %*% U




### 3.4 QR decomposition


### 3.4.3 Regression and the QR in R

X.qr = qr(X)
Q = qr.Q(X.qr)
R = qr.R(X.qr) 

### 3.4.4 Computing the QR

nlz = function(vec) vec/norm2(vec)
orth = function(vec, std) vec - sum(std*vec)*std
norm2 = function(x) sqrt(sum(x^2))

# modified Gram-Schmidt
X = matrix(rnorm(5*4), nr=5)
# X[ ,4] = 0.5*X[ ,1] - .7*X[ ,2] + rnorm(5, 0, .00000001)
Q = matrix(NA, 5, 4)
R = matrix(0, 4, 4)

Q[ ,1] = nlz(X[ ,1])

Q[ ,2] = nlz(orth(X[ ,2], Q[ ,1]))
Q[ ,3] = orth(X[ ,3], Q[ ,1])
Q[ ,4] = orth(X[ ,4], Q[ ,1])

Q[ ,3] = nlz(orth(Q[, 3], Q[ ,2]))
Q[ ,4] = orth(Q[ ,4], Q[ ,2])

Q[ ,4] = nlz(orth(Q[, 4], Q[ ,3]))

# compare with R's QR

print(Q)
qr.Q(qr(X))
   
# original Gram-Schmidt
Q = matrix(NA, 5, 4)
Q2[ ,1] = nlz(X[ ,1])

Q2[ ,2] = nlz(orth(X[ ,2], Q2[ ,1]))

Q2[ ,3] = orth(X[ ,3], Q2[ ,1])
Q2[ ,3] = nlz(orth(Q2[ ,3], Q2[ ,2]))

Q2[ ,4] = orth(X[ ,4], Q2[ ,1])
Q2[ ,4] = orth(Q2[ ,4], Q2[ ,2])
Q2[ ,4] = nlz(orth(Q2[, 4], Q2[ ,3]))

### 3.5 Determinants

myqr = qr(A)
magn = sum(log(abs(diag(myqr$qr)))) # magnitude on log scale

##################################
# 4: Eigendecomposition and SVD
##################################

### 4.1 Eigendecomposition

# computing the top few eigenvalue/vector pairs

A = matrix(c(3, 1.3, .7, 1.3, 2, .5, .7, .5, 1), 3)

e = eigen(A)

z1 = c(1, 0, 0)
for(i in 1:20){
  z1 = A %*% z1
  z1 = z1/norm2(z1)
  print(z1)
}

val1 = (A %*% z1 / z1)
val1 = val1[1,1]

# following Gentle-NLA, p. 125, we can get the next largest eigenvalue, which is the largest eigenvalue of the matrix B
B = A - val1 * outer(c(z1), c(z1))

z2 = c(1, 0, 0)
for(i in 1:200){
  z2 = B %*% z2
  z2 = z2/norm2(z2)
  print(z2)
}

val2 = B%*%z2/z2 + val1

##################################
# 5: Computation
##################################

### 5.2 Sparse matrices

require(spam)
mat = matrix(c(0,0,1,0,10,0,0,0,100,0,rep(0,5),1000,rep(0,4)), nrow = 4, byrow = TRUE)
mat = as.spam(mat)
mat@entries
mat@rowpointers
mat@colindices
