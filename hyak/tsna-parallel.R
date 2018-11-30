
library(tsna)
require(networkDynamicData)
data(hospital_contact)

v = sample(1:network.size(hospital), 20)
v = 1:network.size(hospital)
tPath(hospital, v = 38, direction = "bkwd", type = "latest.depart")
out <- sapply(v, function(x) sum(tPath(hospital, v = x)$tdist<Inf))

ts <- seq(1000, 100000, 5000)

tPath(hospital, v = 38)$tdist<Inf
sum(tPath(hospital, v = 38)$tdist<Inf)
sum(tPath(hospital, v = 38, start = 1, end = 100000)$tdist<Inf)
sapply(ts, function(x) sum(tPath(hospital, v = 38, start = 1, end = x)$tdist<Inf))

g <- function(x) sum(tPath(hospital, v = 38, start = 1, end = x)$tdist<Inf)
g(100000)

sapply(ts, g)

g <- function(v) sapply(ts, function(x) sum(tPath(hospital, v = v, start = 1, end = x, direction = "fwd")$tdist<Inf))
g(2)

library(parallel)
out <- mclapply(v, g)
df <- as.data.frame(do.call("cbind", out))

matplot(ts, df, type = "l")
