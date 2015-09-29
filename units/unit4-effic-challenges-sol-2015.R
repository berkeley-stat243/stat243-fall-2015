## challenge 1

n = 1000000
mns = c(-5, 0, 5, 10, 15)
sds = c(1, 2, 3, 4, 5)
p = 5
y = rnorm(n)

system.time(res <- matrix(NA, n, p))
system.time(for(j in 1:p) res[,j] <- dnorm(y, mns[j], sds[j]))
system.time(for(i in 1:n) res[i,] <- dnorm(y[i], mns, sds))

system.time(cbind(dnorm(y, mns[1], sds[1]),  # manually do each component
                  dnorm(y, mns[2], sds[2]),
                  dnorm(y, mns[3], sds[3]),
                  dnorm(y, mns[4], sds[4]),
                  dnorm(y, mns[5], sds[5])))

system.time(matrix(dnorm(rep(y, each = p), mns, sds), nc = p, byrow = TRUE))
# make copies of y so that they are evaluated for each mn and sd

system.time(dnorm(y,
                  matrix(mns, nr = n, nc = p, byrow = TRUE),
                  matrix(sds, nr = n, nc = p, byrow = TRUE)))
# make the mean and sd into matrices

system.time(t(dnorm(matrix(y, nc = n, nr = p, byrow = TRUE), mns, sds)))
# make y into a matrix

system.time(matrix(dnorm(y, rep(mns, each = n), rep(sds, each = n)), nc = p))
# make copies of mn and sd


## challenge 7

nExons <- 100
seqlen <- 1e6
m = 10000
set.seed(0)
nreads = rpois(m, 3)+1
nreads[nreads > 7] = 7

reads = list()
length(reads) = m

starts = 1:seqlen

for(i in seq_len(m)) {
    midpoint <- sample(starts, 1)
    start <- round(rnorm(nreads[i], midpoint, 500))
    start[start < 1] <- 1
    incr = rpois(nreads[i], 80)
    incr[incr == 0] = 1
    end = start + incr
    reads[[i]] = c(rbind(start, end))
}

start <- sample(starts, nExons, replace = TRUE)
incr <- rpois(nExons, 400)
end = start + incr
exons <- cbind(start, end)

save(reads, exons, file = 'exons.Rda')

# or one could suppose the reads come as a ragged CSV file:
#tmp = sapply(reads, paste0, collapse = ',')

## rough solution

#mx = max(sapply(tmp, function(row) length(strsplit(row, split = ',')[[1]]))) # .2
mx = max(sapply(reads, length))

m <- length(reads)

matS = matE = matrix(0, m, mx)  # .002
system.time({
for(i in 1:m) {   # .24
#    vals = matrix(as.numeric(strsplit(tmp[i], split = ',')[[1]]), ncol = 2, byrow = TRUE)
    vals = matrix(reads[[i]], ncol = 2, byrow = TRUE)
    inds = 1:nrow(vals)
    matS[i, inds] = vals[,1]
    matE[i, inds] = vals[,2]
}})

E1 = exons[1,1]
E2 = exons[1,2]
system.time({
matches =
    (E1 <= matS & matS < E2) | (E1 < matE & matE <= E2) | ( matS < E1 & matE > E2 )
})  # .03
sum(rowSums(matches) >= 1)  #.001

system.time({
cnt = 0
for(i in 1:m) {
#    vals = matrix(as.numeric(strsplit(tmp[i], split = ',')[[1]]), ncol = 2, byrow = TRUE)
    vals = matrix(reads[[i]], ncol = 2, byrow = TRUE)
    if(sum(
        (E1 <= vals[,1] & vals[,1] < E2) | (E1 < vals[,2] & vals[,2] <= E2) | ( vals[,1] < E1 & vals[,2] > E2 ) ) >= 1) cnt = cnt + 1
}
})  # .25
