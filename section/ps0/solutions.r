# 1.1

1:50

!as.logical(1:50%%2)

50:1

c(1:50, 49:1)

-10:10

3*1:16
seq(3, 48, by=3)

as.character(3*1:16)

rep(c("a","b","c","d"), c(4,3,2,1))

factor(rep(c("a","b","c","d"), c(4,3,2,1)))

seq(-1,1, length.out=200)


# 1.2

x = 1:50
y = cos(x)
z = tan(y)
w = y*z
f = x %in% 10:29
df1 = data.frame(x=x, y=y, z=z, w=w, f=f)
names(df1) = toupper(names(df1))
df1 = data.frame(y=y, z=z, w=w, f=f)
# if x != 1:50, then you would need to additional do:
# row.names(df1) = x

# 2.1

m1 = df1[sapply(df1, is.numeric)]
m2 = m1[f, ]
df2 = df1[df1$z >= 0, names(df1) != 'z']
df3 = df1[-c(3,17), ]
df4 = df1[seq(2,nrow(df1),2), ]

# 3.1

x = seq(1,3,.1)
exp(2*x)*x^sqrt(x)

# 3.2

x = matrix(0, 5, 5)
y = x
y[abs(row(x)-col(x))==1] = 1
abs(row(x)-col(x))

# 3.3
outer(0:4, 0:4, "+")
outer(0:4, 0:4, "+")%%5

# 4.1
m1 <- matrix(sample.int(100, 5 * 6), 5, 6)

m2 <- t(apply(m1, 1, function(row) row / sum(row)))

all.equal(apply(m2, 1, sum), rep(1, 5))
all.equal(rowSums(m2), rep(1, nrow(m2)))

m3 <- apply(m1, 2, function(col) col / sum(col))

all.equal(apply(m3, 2, sum), rep(1, ncol(m3)))
all.equal(colSums(m3), rep(1, ncol(m3)))

# 4.2
x  <- seq(1, 10, by = 0.1)
y <- 2 * x + rnorm(length(x), sd = 1.5)

plot(x,y)
abline(mylm)

mylm <- lm(y ~ x)
plot(mylm)

lapply(mylm, class)
sapply(mylm, class)

all.equal(lapply(mylm, class), sapply(mylm, class))

# 5.1
sadev <- function(x, na.rm = FALSE) {
  if (!is(x, 'numeric')) {
    stop(substitute(x), ' (x) must be of type "numeric"')
  }
  med <- median(x, na.rm = na.rm)
  sum(abs(x - med), na.rm = na.rm)
}

# 5.2
x <- sample(c(0, 1), 100, replace = TRUE)
y <- sample(c(0, 1), 100, replace = TRUE)

sum_heads <- function(n) {
  sum(sample(c(0,1), n, replace = TRUE))
}

sums <- replicate(10000L, sum_heads(200))
hist(sums)

# 5.3
op <- function(x, y, operation = "add"){
  switch(operation,
    add = x + y,
    sub = x - y,
    mul = x * y,
    div = x / y,
    stop("operation unknown"))
}


# 5.4
cs <- function(x) {
  ret <- numeric(length(x))
  ret[1] <- x[1]
  for (i in 2:length(x)) {
    ret[i] <- x[i] + ret[i-1]
  }
  ret
}

