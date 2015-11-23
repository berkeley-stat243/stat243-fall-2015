################################################################
### Demo code for Unit 11 of Stat243, "Optimization"
### Chris Paciorek, November 2015
################################################################

#######################################
# 3: Univariate function optimization
#######################################

### 3.3.3 How can Newton's method go wrong? 

## @knitr newton-wrong

par(mfrow = c(1,2))
fp <- function(x, theta = 1){
	exp(x*theta)/(1+exp(x*theta)) - .5
}
fpp <- function(x, theta = 1){
	exp(x*theta)/((1+exp(x*theta))^2)
}
xs <- seq(-15, 15, len = 300)
plot(xs, fp(xs), type = 'l')
lines(xs, fpp(xs), lty = 2)

## good starting point
x0 <- 2
xvals <- c(x0,rep(NA,9))
for(t in 2:10){
	xvals[t]=xvals[t-1] - fp(xvals[t-1]) / fpp(xvals[t-1])
}
print(xvals)
points(xvals, fp(xvals), pch = as.character(1:length(xvals)))

## bad starting point
x0 <- 2.5
xvals <- c(x0,rep(NA,9))
for(t in 2:10){
	xvals[t]=xvals[t-1] - fp(xvals[t-1]) / fpp(xvals[t-1])
}
print(xvals)
points(xvals, fp(xvals), pch = as.character(1:length(xvals)), col = 'red')
## whoops

## example of mistakenly climbing uphill

# original fxn
f <- function(x) cos(x)
# gradient
fp <- function(x) -sin(x)
# second derivative
fpp <- function(x) -cos(x)
xs <- seq(0, 2*pi, len = 300)
plot(xs, f(xs), type = 'l', lwd = 2)
lines(xs, fp(xs))
lines(xs, fpp(xs), lty = 2)
x0 <- 0.2 # starting point
fp(x0) # negative
fpp(x0) # negative
x1 <- x0 - fp(x0)/fpp(x0) # whoops, we've gone uphill 
## because of the negative second derivative
xvals <- c(x0,rep(NA,9))
for(t in 2:10){
	xvals[t]=xvals[t-1]-fp(xvals[t-1])/fpp(xvals[t-1])
}
xvals
points(xvals, fp(xvals), pch = as.character(1:length(xvals)), col = 'red')
## and we've found a maximum rather than a minimum...

## @knitr dummy

#######################################
# 4: Convergence ideas
#######################################

### 4.2 Starting values

## @knitr rastrigin

rastrigin <- function(x) {
	A <- 10
	n <- length(x)
	return(A*n + sum(x^2 - A * cos(2*pi*x)))
}
const <- 5.12
nGrid <- 100
gr <- seq(-const, const, len = nGrid)
xs <- expand.grid(x1 = gr, x2 = gr)
y <- apply(xs, 1, rastrigin)
require(fields)
image.plot(gr, gr, matrix(y, nGrid, nGrid), col=tim.colors(32))

## @knitr dummy

### 4.3 Convergence rates

## @knitr convergence

options(digits = 10)
f <- function(x) cos(x)
fp <- function(x) -sin(x)
fpp <- function(x) -cos(x)
xstar <- pi # known minimum

## N-R
x0 <- 2
xvals <- c(x0,rep(NA,9))
for(t in 2:10){
	xvals[t] <- xvals[t-1] - fp(xvals[t-1]) / fpp(xvals[t-1])
}
print(xvals)

## bisection
bisecStep <- function(interval, fp){
	xt <- mean(interval)
	if(fp(interval[1]) * fp(xt) <= 0) interval[2] <- xt else interval[1] <- xt
	return(interval)
}
nIt <- 30
a0 <- 2; b0 <- (3*pi/2) - (xstar - a0) 
## have b0 be as far from min as a0 for fair comparison with N-R
interval <- matrix(NA, nr = nIt, nc = 2)
interval[1, ] <- c(a0, b0)
for(t in 2:nIt){
	interval[t, ] <- bisecStep(interval[t-1, ], fp)
}
rowMeans(interval)

## @knitr dummy

########################################
### 5: Multivariate optimization
########################################

### 5.2 Newton-Raphson (Newton's method)

library(MASS)
head(wtloss)
attach(wtloss)

plot(Days, Weight, ylab = "Weight (kg)")

### Linear fit - not a good model
wtloss.lm <- lm(Weight ~ Days, data = wtloss)
coef(wtloss.lm) # estimates of the intercept and slope
lines(Days, fitted(wtloss.lm), col = "grey")

## we need some starting values
## guess that beta2 approx 100 since the midpoint of Days is near 100

beta2.init = 100
plot(2^(-Days/beta2.init), Weight)
tmpMod = lm(Weight ~ I(2^(-Days/beta2.init)))
beta0.init = tmpMod$coef[1]
beta1.init = tmpMod$coef[2]

plot(Days, Weight, ylab = "Weight (kg)")
lines(Days, beta0.init + beta1.init * 2^(-Days/beta2.init), col = 'red')
## not bad; let's go with that

expr = quote((Weight - (beta0 + beta1 * 2^(-Days/beta2)))^2) # objective is sum of this quantity over the observations
## let R do the differentiation of the basic expression for us (since human error in taking derivatives is very common)
## R won't deal with the summation, so we'll have to do that ourselves
deriv(expr, c("beta0", "beta1", "beta2"), function.arg = TRUE)
deriv3(expr, c("beta0", "beta1", "beta2"), function.arg = TRUE) # same as deriv()  with Hessian = TRUE

f =function(betas){
  sum((Weight - (betas[1] + betas[2] * 2^(-Days/betas[3])))^2)
}

## use the basic code based on deriv() and deriv3() 
fp = function(betas){
  ## a bit sloppy as I use Days and Weight as global vars here
  beta0 = betas[1]
  beta1 = betas[2]
  beta2 = betas[3]
  .expr3 <- 2^(-Days/beta2)
  .expr6 <- Weight - (beta0 + beta1 * .expr3)
  grad = - c(sum(2*.expr6), sum(2*(.expr3*.expr6)),
             sum(2* (beta1 * (.expr3 * (log(2) * (Days/beta2^2))) * .expr6)))
  return(grad)
}

fpp = function(betas){
  ## a bit sloppy as I use Days and Weight as global vars here
  n = length(Days)
  beta0 = betas[1]
  beta1 = betas[2]
  beta2 = betas[3]
  .expr3 <- 2^(-Days/beta2)
  .expr6 <- Weight - (beta0 + beta1 * .expr3)
  .expr11 <- log(2)
  .expr12 <- beta2^2
  .expr14 <- .expr11 * (Days/.expr12)
  .expr15 <- .expr3 * .expr14
  .expr16 <- beta1 * .expr15
  
  hessian = matrix(0, 3, 3)
  hessian[1, 1] <- 2*n # don't forget to do summation (i.e., multiply by n)
  hessian[1, 2] <- sum(2 * .expr3)
  hessian[1, 3] <- sum( 2 *  .expr16)
  hessian[2, 2] <- sum( 2 * (.expr3 * .expr3))
  
  hessian[2, 3] <-  - sum(2 * 
                          (.expr15 * .expr6 - .expr3 * .expr16))
  
  hessian[3, 3] <- - sum(2 * (beta1 * (.expr15 * 
                                       .expr14 - .expr3 * (.expr11 * (Days * (2 * beta2)/.expr12^2))) * 
                              .expr6 - .expr16 * .expr16))
  hessian[lower.tri(hessian)] = t(hessian[upper.tri(hessian)])
  return(hessian) 
}



nIt = 20
xvals = matrix(NA, nr = nIt, nc = 3)
xvals[1, ] = c(beta0.init, beta1.init, beta2.init)


for(t in 2:nIt){
  xvals[t, ] = xvals[t-1, ] - solve(fpp(xvals[t-1, ]), fp(xvals[t-1, ]))
  if(FALSE){ # in case not numerically p.d., use pseudoinverse
    e = eigen(fpp(xvals[t-1, ])) 
    e$val = 1/e$val
    e$val[e$val < 0] = 0 # pseudo-inverse
    xvals[t, ] = xvals[t-1, ] - e$vec%*%((t(e$vec)*e$val)%*%fp(xvals[t-1, ]))
  }
}

## let's check against R's built-in nonlinear least squares function
beta.start = xvals[1, ]
names(beta.start) = c("beta0", "beta1", "beta2")
wtloss.fm <- nls(Weight ~ beta0 + beta1*2^(-Days/beta2), 
                 data = wtloss, start = beta.start, trace = TRUE)
nlsEst = coef(wtloss.fm)

f(xvals[20, ])
f(nlsEst)

plot(Days, Weight, ylab = "Weight (kg)")
lines(Days, fitted(wtloss.fm), col = "blue")

### 5.3 Fisher scoring

## data are only involved in .expr6 and all Hessian terms involving .expr6 are linear in it, so replace with its expectation, which is simply 0

fppFS = function(betas){
  ## a bit sloppy as I use Days and Weight as global vars here
  n = length(Days)
  beta0 = betas[1]
  beta1 = betas[2]
  beta2 = betas[3]
    .expr3 <- 2^(-Days/beta2)
    .expr6 <- 0 # Weight - (beta0 + beta1 * .expr3)
    .expr11 <- log(2)
    .expr12 <- beta2^2
    .expr14 <- .expr11 * (Days/.expr12)
    .expr15 <- .expr3 * .expr14
    .expr16 <- beta1 * .expr15
 
  hessian = matrix(0, 3, 3)
  hessian[1, 1] <- 2*n # don't forget to do summation (i.e., multiply by n)
    hessian[1, 2] <- sum(2 * .expr3)
    hessian[1, 3] <- sum( 2 *  .expr16)
    hessian[2, 2] <- sum( 2 * (.expr3 * .expr3))
  
    hessian[2, 3] <-  - sum(2 * 
        (.expr15 * .expr6 - .expr3 * .expr16))

    hessian[3, 3] <- - sum(2 * (beta1 * (.expr15 * 
        .expr14 - .expr3 * (.expr11 * (Days * (2 * beta2)/.expr12^2))) * 
        .expr6 - .expr16 * .expr16))
  hessian[lower.tri(hessian)] = t(hessian[upper.tri(hessian)])
  return(hessian) 
}


nIt = 20
xvals = matrix(NA, nr = nIt, nc = 3)
xvals[1, ] = c(beta0.init, beta1.init, beta2.init)

fpp(xvals[1, ])
fppFS(xvals[1, ])
## pretty similar - some terms in the observed FI didn't involve the data, so don't change anyway

for(t in 2:nIt){
  xvals[t, ] = xvals[t-1, ] - solve(fppFS(xvals[t-1, ]), fp(xvals[t-1, ]))
}

## FS seems to give slightly faster convergence here


### 5.5.1 Descent methods and Newton-like methods

## steepest descent

## @knitr steepest

par(mai = c(.5,.4,.1,.4))
f <- function(x){
	x[1]^2/1000 + 4*x[1]*x[2]/1000 + 5*x[2]^2/1000
}
fp <- function(x){
	c(2 * x[1]/1000 + 4 * x[2]/1000,
	4 * x[1]/1000 + 10 * x[2]/1000)
}
lineSearch <- function(alpha, xCurrent, direction, FUN){
	newx <- xCurrent + alpha * direction
	FUN(newx)
}
nIt <- 50
xvals <- matrix(NA, nr = nIt, nc = 2)
xvals[1, ] <- c(7, -4)
for(t in 2:50){
	newalpha <- optimize(lineSearch, interval = c(-5000, 5000),
		xCurrent = xvals[t-1, ], direction = fp(xvals[t-1, ]),
		FUN = f)$minimum 
	xvals[t, ] <- xvals[t-1, ] + newalpha * fp(xvals[t-1, ])
}
x1s <- seq(-5, 8, len = 100); x2s = seq(-5, 2, len = 100)
fx <- apply(expand.grid(x1s, x2s), 1, f)
## plot f(x) surface on log scale
image.plot(x1s, x2s, matrix(log(fx), 100, 100), 
	xlim = c(-5, 8), ylim = c(-5,2)) 
lines(xvals) ## overlay optimization path
## kind of slow

## @knitr dummy

### 5.6 Gauss-Seidel

## @knitr Gauss-Seidel

f <- function(x){
	return(x[1]^2/1000 + 4*x[1]*x[2]/1000 + 5*x[2]^2/1000)
}
f1 <- function(x1, x2){ # f(x) as a function of x1
	return(x1^2/1000 + 4*x1*x2/1000 + 5*x2^2/1000)
}
f2 <- function(x2, x1){ # f(x) as a function of x2
	return(x1^2/1000 + 4*x1*x2/1000 + 5*x2^2/1000)
}
x1s <- seq(-5, 8, len = 100); x2s = seq(-5, 2, len = 100)
fx <- apply(expand.grid(x1s, x2s), 1, f)
image.plot(x1s, x2s, matrix(log(fx), 100, 100))
nIt <- 49
xvals <- matrix(NA, nr = nIt, nc = 2)
xvals[1, ] <- c(7, -4)
## 5, -10
for(t in seq(2, nIt, by = 2)){
	newx1 <- optimize(f1, x2 = xvals[t-1, 2], interval = c(-40, 40))$minimum
	xvals[t, ] <- c(newx1, xvals[t-1, 2])
	newx2 <- optimize(f2, x1 = newx1, interval = c(-40, 40))$minimum
	xvals[t+1, ] <- c(newx1, newx2)
}
lines(xvals)

## @knitr dummy

### 5.7 Nelder-Mead

## set up tuning factors
alpha = 1
gamma = 2
beta = .5
delta = .5

## auxiliary function to plot line segments
plotseg = function(ind1, ind2, col = 1){
  if(length(ind1) == 1){
    segments(xs[ind1, 1], xs[ind1 , 2], xs[ind2, 1], xs[ind2, 2], col = col)
  } else{
    segments(ind1[1], ind1[2], xs[ind2, 1], xs[ind2, 2], col = col)    
  }
}
  
## initial polytope
xs = matrix(c(-2,3,-6,4,-4,2),nc=2,byrow=T)

plot(xs,xlim=c(-7,-1),ylim=c(1,8))
plotseg(2, 3)
plotseg(1, 3)
plotseg(1, 2)

xbar = (xs[1,]+xs[2,])/2

text(xs[3,1], xs[3,2], expression(x[p+1]))

points(xbar[1],xbar[2], col = 'red')

### reflection

xr = (1+alpha)*xbar - alpha*xs[3,]
points(xr[1],xr[2],col = 'red', pch = 16, cex = .4)
text(xr[1],xr[2], expression(x[r]))

plotseg(xr, 1, col = 'red')
plotseg(xr, 2, col = 'red')
plotseg(1, 2, col = 'red')
## red triangle is now our proposed new polytope

### consider expansion if xr is better than all the other points

xe = gamma*xr + (1-gamma)*xbar
points(xe[1], xe[2], col = 'green', pch = 16, cex = .4)
text(xe[1],xe[2], expression(x[e]))
## if xe is better than xr, use xe, o.w. use xr

plotseg(xe, 1, col = 'green')
plotseg(xe, 2, col = 'green')
plotseg(1, 2, col = 'green')

### consider contraction if xr is worse than all the other points

points(xs[3,1], xs[3, 2], col = 'blue')

## set xh to be the best of these two points
xh = xs[3, ] # suppose the original point is better than the reflection
xc = beta*xh + (1-beta)*xbar
points(xc[1], xc[2], col = 'blue', pch = 16, cex = .4)
text(xc[1],xc[2], expression(x[c]))
## if xc is better than xh, then contract

plotseg(xc, 1, col = 'blue')
plotseg(xc, 2, col = 'blue')
plotseg(1, 2, col = 'blue')

## if not, shrink simplex toward the best point (xs[1, ])

### shrinkage

xs[2, ] = delta*xs[2, ] + (1-delta)*xs[1, ]
xs[3, ] = delta*xs[3, ] + (1-delta)*xs[1, ]

points(xs[2, 1], xs[2, 2], col = 'purple')
points(xs[3, 1], xs[3, 2], col = 'purple')

plotseg(1, 2, 'purple')
plotseg(1, 3, 'purple')
plotseg(2, 3, 'purple')

### Nelder-Mead on our quadratic toy example

## @knitr nelder-example

f <- function(x, plot = TRUE){
    if(plot && cnt < 10) {
        points(x[1], x[2], pch = as.character(cnt))
        if(cnt < 10) cnt <<- cnt + 1 else cnt <<- 1
    } else if(plot) points(x[1], x[2])
    # if(plot) print(c(x,x[1]^2/1000 + 4*x[1]*x[2]/1000 + 5*x[2]^2/1000))
    return(x[1]^2/1000 + 4*x[1]*x[2]/1000 + 5*x[2]^2/1000)
}

library(fields)
par(mfrow = c(1,2))

x1s <- seq(-5, 8, len = 100); x2s = seq(-5, 2, len = 100)
fx <- apply(expand.grid(x1s, x2s), 1, f, FALSE)
cnt <- 1
image.plot(x1s, x2s, matrix(log(fx), 100, 100))
init <- c(7, -4)
optim(init, f, method = "Nelder-Mead")

x1s <- seq(-.2, .2, len = 100); x2s = seq(-.12, .12, len = 100)
fx <- apply(expand.grid(x1s, x2s), 1, f, FALSE)
cnt <- 1
image.plot(x1s, x2s, matrix(log(fx), 100, 100))
init <- c(-0, 0)
optim(init, f, method = "Nelder-Mead")

## @knitr dummy

### 5.8 Simulated annealing

x= seq(0,10,len=100)
f=function(x)  sin(x)

plot(x, f(x), type='l', ylim = c(-1, 3), col = 'grey')
tau = 10
## plot the modified function
lines(x, exp(-f(x)/tau), col = 'red')   # try tau = 3, 1, .3, etc.
## effect of cooling...
tau =3
# plot the modified function
lines(x, exp(-f(x)/tau), col='yellow')   # try tau = 3, 1, .3, etc.
tau =1
## plot the modified function
lines(x, exp(-f(x)/tau), col = 'blue')   # try tau = 3, 1, .3, etc.

##############################
# 6: Optimization in R
##############################

### 6.1 Core optimization functions

## using optim()

yHundredths = scan('../data/precipData.txt')  # precip in hundredths of inches
yHundredths = yHundredths[!is.na(yHundredths)]
y = yHundredths/100  # precip now in inches

par(mfrow = c(1, 2))
hist(y)
thresh = 3
hist(y[y > thresh])

npy=31+28+31 # number of days in winter season
cutoff = (1/25.4) # wet days defined as those with > 1 mm (1/25.4 inches) of precip
thresh = as.numeric(quantile(y[y > cutoff],.98,na.rm=T)) # 98%ile of wet days


pp.lik <- function(par, y, thresh, npy) {
  mu <- par[1]
  sc <- par[2]
  xi <- par[3]
  uInd = y > thresh
  if(sc <= 0) 
    return(10^6);
  if ((1 + ((xi * (thresh - mu))/sc)) < 0) {
    l <- 10^6; 
  }
  else {
    y <- (y - mu)/sc
    y <- 1 + xi * y
    if (min(y^uInd) <= 0){   
      l <- 10^6;
    } else{
      ytmp = y
      ytmp[!uInd]=1  # 'zeroes' out those below the threshold after applying the log in next line
      l <- sum(uInd * log(sc)) + sum(uInd * log(ytmp) * 
                                     (1/xi + 1)) + length(y)/npy * mean((1 + (xi * (thresh - mu))/sc)^(-1/xi))
    }
  }
  l
}

## optim() usage
## out <- optim(init, pp.lik, hessian = TRUE, method = "BFGS", control = list(maxit = maxit, trace = TRUE))

yExc = y[y > thresh]
in2 <- sqrt(6 * var(yExc))/pi  # have initial values depend only on those above the threshold
in1 <- mean(yExc) - 0.57722 * in2
init0 = c(in1, in2, 0.1)

## fit with Nelder-Mead (default) and BFGS
fit1 <- optim(init0, pp.lik, y = y, thresh = thresh, npy = npy, control = list(trace = TRUE)) # 118 fxn evals
fit2 <- optim(init0, pp.lik, y = y, thresh = thresh, npy = npy, method = 'BFGS', control = list(trace = TRUE)) # ~ 10 its
print(fit1)
print(fit2)

mle <- fit2$par

system.time(optim(init0, pp.lik, y = y, thresh = thresh, npy = npy))
system.time(optim(init0, pp.lik, y = y, thresh = thresh, npy = npy, method = 'BFGS'))

## different starting value 
init1 = c(mean(y[y > thresh]), sd(y[y > thresh]), -0.1)
optim(init1, pp.lik, y = y, thresh = thresh, npy = npy, control = list(trace = TRUE)) 
optim(init1, pp.lik, y = y, thresh = thresh, npy = npy, method = 'BFGS', control = list(trace = TRUE)) 

## bad starting value for BFGS
init2 = c(thresh, .01, .1)
out = optim(init2, pp.lik, y = y, thresh = thresh, npy = npy, control = list(trace = TRUE), hessian = TRUE)
solve(out$hessian)
out2 = optim(init2, pp.lik, y = y, thresh = thresh, npy = npy, method = 'BFGS', control = list(trace = TRUE), hessian = TRUE)
solve(out2$hessian)


## suppose the data were on a different scale
yExc2 <- yExc * 1000
y2 <- y * 1000
thresh2 <- thresh * 1000


in2 <- sqrt(6 * var(yExc2))/pi  # have initial values depend only on those above the threshold
in1 <- mean(yExc2) - 0.57722 * in2
init3 = c(in1, in2, 0.1)

optim(init3, pp.lik, y = y2, thresh = thresh2, npy = npy, control = list(trace = TRUE)) ## note that the parameters have changed even beyond a scaling effect
optim(init3, pp.lik, y = y2, thresh = thresh2, npy = npy, method = 'BFGS', control = list(trace = TRUE)) # note lack of convergence after 100 its
optim(init3, pp.lik, y = y2, thresh = thresh2, npy = npy, method = 'BFGS', control = list(trace = TRUE, maxit = 1000)) # convergence, but to values not concordant with those from fitting the original y data

## when we have y2 = y*1000, the location and scale parameters are on very different scales than the shape parameter
## can we use parscale to deal with the problems when the data are on a different scale?
optim(init3, pp.lik, y = y2, thresh = thresh2, npy = npy, control = list(trace = TRUE, parscale = c(1000,1000,1))) 
optim(init3, pp.lik, y = y2, thresh = thresh2, npy = npy, method = 'BFGS', control = list(trace = TRUE, parscale = c(1000,1000,1))) 
## yes, that works! the parameter estimates are now equivalent to those from the standard fitting of the original data

## default step size for numerical derivative is only .001; perhaps we should try with higher accuracy
optim(init0, pp.lik, y = y, thresh = thresh, npy = npy, method = 'BFGS', control = list(trace = TRUE, ndeps = rep(1e-6, 3)))
## note that we needed only 33 function evaluations instead of the original 43, presumably because of higher accuracy in the derivative

#### let's do some plotting of the objective fxn as a sanity check

## 3-d grid of location, scale, shape
locVals = seq(0, 5, len = 30)
scaleVals = seq(.1, 3, len = 30)
shapeVals = seq(-.3, .25, by = .05)
parGrid = expand.grid(loc = locVals, scale = scaleVals, shape = shapeVals)

obj=apply(parGrid, 1, pp.lik, y=y, thresh=thresh, npy=npy) # compute objective function for all parameter combos

tmp = cbind(parGrid,obj)

library(fields)

par(mfrow=c(3, 4), mai = c(.6,.5, .3, .1), mgp = c(1.8, .7, 0))
for( i in 1:length(shapeVals)){
  tmp2 = tmp[tmp$shape == shapeVals[i],] # slice of objective fxn for fixed shape parameter
  image.plot(locVals, scaleVals, matrix((tmp2$obj), length(locVals), length(scaleVals)), col = tim.colors(32), zlim = c(40,80), main = as.character(shapeVals[i]))
}
## note there a couple weird things about this log likelihood - (1) weird things happen when the shape parameter is very close to 0 and (2) for certain combinations the likelihood is not defined (it's set to 1e6) - this is why some of the colors go from aqua to white without showing red values - reparameterizing or optimizing with explicit constraints may be better approaches

## apart from that, it appears that optim() has probably found the minimum

##############################
# 9: Convex optimization
##############################

### 9.6 Interior point methods

## @knitr constrOptim

## based on the example in ?constrOptim
fr <- function(x) {   ## Rosenbrock Banana function
  x1 <- x[1]
  x2 <- x[2]
  100 * (x2 - x1 * x1)^2 + (1 - x1)^2
}
grr <- function(x) { ## Gradient of 'fr'
  x1 <- x[1]
  x2 <- x[2]
  c(-400 * x1 * (x2 - x1 * x1) - 2 * (1 - x1),
    200 *      (x2 - x1 * x1))
}

m <- 100
x1s <- x2s <- seq(-5, 5, len = m)
xs <- expand.grid(x1s, x2s)

f <- apply(xs, 1, fr)

image.plot(x1s, x2s, matrix(log(f), m))

ui = rbind(c(-1,0), c(1,-1))
ci = c(-0.9, -0.1)
## x1 <= 0.9
## x2 <= x1 + 0.1
## this is Region "I" in the figure

abline(v = 0.9)
abline(.1, 1)

out <- constrOptim(c(.5,0), fr, grr, ui = ui, ci = ci)
out$par
points(out$par[1], out$par[2])
text(-1,-2, "I", cex = 2)

## what about constraining to a different region? ("Region II")

ui = rbind(c(-1,0), c(-1, 1))
ci = c(-0.9,0.1)
## x1 <= 0.9
## x2 >= x1 + 0.1

out <- constrOptim(c(.5,0), fr, grr, ui = ui, ci = ci)
## whoops, not feasible!
out <- constrOptim(c(-3, 2), fr, grr, ui = ui, ci = ci)
points(out$par[1], out$par[2], pch = 2)
text(-3,0, "II", cex = 2)

## how about optimizing along a line (equality constraint)?
## x1 - x2 = 0.1 is the same as
## x1 - x2 <= 0.1
## x1 - x2 >= 0.1
ui = rbind(c(-1, 1), c(1, -1))
ci = c(-0.1, 0.1)

out <- constrOptim(c(3.1, 3.0), fr, grr, ui = ui, ci = ci)
## hmmm, numerical issues?

## ok, how about making a long narrow region around the line?
## this takes a while...
ui = rbind(c(1,-1), c(-1,1))
ci = c(.099,-.101)
## hmmm, out1 thinks it has converged ...
out1 <- constrOptim(c(3.1, 3.0), fr, grr, ui = ui, ci = ci)
out2 <- constrOptim(c(3.1, 3.0), fr, NULL, ui = ui, ci = ci)

image.plot(x1s, x2s, matrix(log(f), m))
abline(-0.1, 1)
points(out1$par[1], out1$par[2], pch = "1")
points(out2$par[1], out2$par[2], pch = "2")



