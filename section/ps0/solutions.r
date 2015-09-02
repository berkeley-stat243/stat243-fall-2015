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



