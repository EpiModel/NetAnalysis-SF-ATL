
## 
## tsna anlaysis for San Francisco & Atlanta sexual networks
## 

## Packages ##
library("tsna")
library("networkDynamicData")
suppressMessages(library("EpiModel"))
library("scales")

## Set seed for analysis ##
set.seed(803)

## Setup output list ##
out <- list()


# 0. Network data ----------------------------------------------------------

## Read in network simulations ##

sim.sf <- readRDS("DataV2/artnet.NetSim.SanFrancisco.rda")
sim.atl <- readRDS("DataV2/artnet.NetSim.Atlanta.rda")

## Extract each partner type network ##

# SF networks
net.sfm <- sim.sf[[1]] # main
net.sfc <- sim.sf[[2]] # casual
net.sfi <- sim.sf[[3]] # inst

# ATL networks
net.atlm <- sim.atl[[1]] # main
net.atlc <- sim.atl[[2]] # casual
net.atli <- sim.atl[[3]] # inst


# 1. Outcome data ----------------------------------------------------------

## Function to load outcome data ##

load_data <- function(file) {
  x <- load(file = file)
  x <- out
}

## Load outcome data into global environment ##

# All partnership types
sf.all <- load_data("DataV2/sfo.all.1.rda")
atl.all <- load_data("DataV2/atl.all.1.rda")

# Main
sf.main <- load_data("DataV2/sfo.main.1.rda")
atl.main <- load_data("DataV2/atl.main.1.rda")

# Casual
sf.casl <- load_data("DataV2/sfo.casl.1.rda")
atl.casl <- load_data("DataV2/atl.casl.1.rda")

# Inst
sf.inst <- load_data("DataV2/sfo.inst.1.rda")
atl.inst <- load_data("DataV2/atl.inst.1.rda")


# 2. Validation ------------------------------------------------------------

# Time step to test 
test_ts <- 260

## Checking degree in simulations ##

test1_inp <- EpiModel::get_degree(network.collapse(net.sfm, at = test_ts))
test2_inp <- EpiModel::get_degree(network.collapse(net.sfc, at = test_ts))
test3_inp <- EpiModel::get_degree(network.collapse(net.sfi, at = test_ts))
test4_inp <- EpiModel::get_degree(network.collapse(net.atlm, at = test_ts))
test5_inp <- EpiModel::get_degree(network.collapse(net.atlc, at = test_ts))
test6_inp <- EpiModel::get_degree(network.collapse(net.atli, at = test_ts))

## Checking degree in outcome data ##

test1_out <- sf.main$degree[test_ts, ]
which(!(test1_inp == test1_out))

test2_out <- sf.casl$degree[test_ts, ]
which(!(test2_inp == test2_out))

test3_out <- sf.inst$degree[test_ts, ]
which(!(test3_inp == test3_out))

test4_out <- atl.main$degree[test_ts, ]
which(!(test4_inp == test4_out))

test5_out <- atl.casl$degree[test_ts, ]
which(!(test4_inp == test4_out))

test6_out <- atl.inst$degree[test_ts, ]
which(!(test4_inp == test4_out))


# 3. Extract vertex IDs ---------------------------------------------------

## Extract vertex ids for age & race categories ##

## Race

# Black
sf.b <- which(get.vertex.attribute(net.sfm, "race") == 0)
atl.b <- which(get.vertex.attribute(net.atlm, "race") == 0)

# White
sf.w <- which(get.vertex.attribute(net.sfm, "race") == 1)
atl.w <- which(get.vertex.attribute(net.atlm, "race") == 1)


## Age

# 15-24
sf.24 <- which(get.vertex.attribute(net.sfm, "age.grp") == 1)
atl.24 <- which(get.vertex.attribute(net.atlm, "age.grp") == 1)

# 25-34
sf.34 <- which(get.vertex.attribute(net.sfm, "age.grp") == 2)
atl.34 <- which(get.vertex.attribute(net.atlm, "age.grp") == 2)

# 35-44
sf.44 <- which(get.vertex.attribute(net.sfm, "age.grp") == 3)
atl.44 <- which(get.vertex.attribute(net.atlm, "age.grp") == 3)

# 45-54
sf.54 <- which(get.vertex.attribute(net.sfm, "age.grp") == 4)
atl.54 <- which(get.vertex.attribute(net.atlm, "age.grp") == 4)

# 55-64
sf.64 <- which(get.vertex.attribute(net.sfm, "age.grp") == 5)
atl.64 <- which(get.vertex.attribute(net.atlm, "age.grp") == 5)


# 4. Extract FRP ----------------------------------------------------------

## Setup output list ##
out <- list()

## Subset outcome data frame by FRP ##

# All partnerships
out$sfa.frp <- sf.all$frp
out$atla.frp <- atl.all$frp

# Main
out$sfm.frp <- sf.main$frp
out$atlm.frp <- atl.main$frp

# Casual
out$sfc.frp <- sf.casl$frp
atlc.frp <- atl.casl$frp

# Inst
out$sfi.frp <- sf.inst$frp
out$atli.frp <- atl.inst$frp


fn <- "data/artnet.TsnaData.rda"
saveRDS(out, file = fn)

# 5.FRP Summary -----------------------------------------------------------

## Manuscript table 2 ##

## SF all partnerships

# Transpose data frame
t.sfa.frp <- t(sfa.frp)

# Overall
summary(t.sfa.frp[, 52])

# Age summary
summary(t.sfa.frp[sf.24, 52])
summary(t.sfa.frp[sf.34, 52])
summary(t.sfa.frp[sf.44, 52])
summary(t.sfa.frp[sf.54, 52])
summary(t.sfa.frp[sf.64, 52])

# Race summary
summary(t.sfa.frp[sf.b, 52])
summary(t.sfa.frp[sf.w, 52])


## SF main

t.sfm.frp <- t(sfm.frp)
summary(t.sfm.frp[, 52])
quantile(t.sfm.frp[, 52], 0.025)
quantile(t.sfm.frp[, 52], 0.975)

# Age summary
summary(t.sfm.frp[sf.24, 52])
summary(t.sfm.frp[sf.34, 52])
summary(t.sfm.frp[sf.44, 52])
summary(t.sfm.frp[sf.54, 52])
summary(t.sfm.frp[sf.64, 52])

# Race summary
summary(t.sfm.frp[sf.b, 52])
summary(t.sfm.frp[sf.w, 52])



## SF casual

t.sfc.frp <- t(sfc.frp)
summary(t.sfc.frp[, 52])
quantile(t.sfc.frp[, 52], 0.025)
quantile(t.sfc.frp[, 52], 0.975)

# Age summary
summary(t.sfc.frp[sf.24, 52])
summary(t.sfc.frp[sf.34, 52])
summary(t.sfc.frp[sf.44, 52])
summary(t.sfc.frp[sf.54, 52])
summary(t.sfc.frp[sf.64, 52])

# Age quantiles
quantile(t.sfc.frp[sf.24, 52], 0.025)
quantile(t.sfc.frp[sf.24, 52], 0.975)
quantile(t.sfc.frp[sf.34, 52], 0.025)
quantile(t.sfc.frp[sf.34, 52], 0.975)
quantile(t.sfc.frp[sf.44, 52], 0.025)
quantile(t.sfc.frp[sf.44, 52], 0.975)
quantile(t.sfc.frp[sf.54, 52], 0.025)
quantile(t.sfc.frp[sf.54, 52], 0.975)
quantile(t.sfc.frp[sf.64, 52], 0.025)
quantile(t.sfc.frp[sf.64, 52], 0.975)

# Race summary
summary(t.sfc.frp[sf.b, 52])
summary(t.sfc.frp[sf.w, 52])

# Race quantiles
quantile(t.sfc.frp[sf.b, 52], 0.025)
quantile(t.sfc.frp[sf.b, 52], 0.975)
quantile(t.sfc.frp[sf.w, 52], 0.025)
quantile(t.sfc.frp[sf.w, 52], 0.975)


## SF one-time

t.sfi.frp <- t(sfi.frp)
summary(t.sfi.frp[, 52])
quantile(t.sfi.frp[, 52], 0.025)
quantile(t.sfi.frp[, 52], 0.975)

# Age summary
summary(t.sfi.frp[sf.24, 52])
summary(t.sfi.frp[sf.34, 52])
summary(t.sfi.frp[sf.44, 52])
summary(t.sfi.frp[sf.54, 52])
summary(t.sfi.frp[sf.64, 52])

# Age quantiles
quantile(t.sfi.frp[sf.24, 52], 0.025)
quantile(t.sfi.frp[sf.24, 52], 0.975)
quantile(t.sfi.frp[sf.34, 52], 0.025)
quantile(t.sfi.frp[sf.34, 52], 0.975)
quantile(t.sfi.frp[sf.44, 52], 0.025)
quantile(t.sfi.frp[sf.44, 52], 0.975)
quantile(t.sfi.frp[sf.54, 52], 0.025)
quantile(t.sfi.frp[sf.54, 52], 0.975)
quantile(t.sfi.frp[sf.64, 52], 0.025)
quantile(t.sfi.frp[sf.64, 52], 0.975)

# Race summary
summary(t.sfi.frp[sf.b, 52])
summary(t.sfi.frp[sf.w, 52])

# Race quantiles
quantile(t.sfi.frp[sf.b, 52], 0.025)
quantile(t.sfi.frp[sf.b, 52], 0.975)
quantile(t.sfi.frp[sf.w, 52], 0.025)
quantile(t.sfi.frp[sf.w, 52], 0.975)


## ATL all

t.atla.frp <- t(atla.frp)
summary(t.atla.frp[, 52])
quantile(t.atla.frp[, 52], 0.025)
quantile(t.atla.frp[, 52], 0.975)

# Age summary
summary(t.atla.frp[sf.24, 52])
summary(t.atla.frp[sf.34, 52])
summary(t.atla.frp[sf.44, 52])
summary(t.atla.frp[sf.54, 52])
summary(t.atla.frp[sf.64, 52])

# Race summary
summary(t.atla.frp[sf.b, 52])
summary(t.atla.frp[sf.w, 52])


## ATL main

t.atlm.frp <- t(atlm.frp)
summary(t.atlm.frp[, 52])
quantile(t.atlm.frp[, 52], 0.025)
quantile(t.atlm.frp[, 52], 0.975)

# Age summary
summary(t.atlm.frp[sf.24, 52])
summary(t.atlm.frp[sf.34, 52])
summary(t.atlm.frp[sf.44, 52])
summary(t.atlm.frp[sf.54, 52])
summary(t.atlm.frp[sf.64, 52])

# Race summary
summary(t.atlm.frp[sf.b, 52])
summary(t.atlm.frp[sf.w, 52])


## ATL casual

t.atlc.frp <- t(atlc.frp)
summary(t.atlc.frp[, 52])
quantile(t.atlc.frp[, 52], 0.025)
quantile(t.atlc.frp[, 52], 0.975)

# Age summary
summary(t.atlc.frp[atl.24, 52])
summary(t.atlc.frp[atl.34, 52])
summary(t.atlc.frp[atl.44, 52])
summary(t.atlc.frp[atl.54, 52])
summary(t.atlc.frp[atl.64, 52])

# Age quantiles
quantile(t.atlc.frp[atl.24, 52], 0.025)
quantile(t.atlc.frp[atl.24, 52], 0.975)
quantile(t.atlc.frp[atl.34, 52], 0.025)
quantile(t.atlc.frp[atl.34, 52], 0.975)
quantile(t.atlc.frp[atl.44, 52], 0.025)
quantile(t.atlc.frp[atl.44, 52], 0.975)
quantile(t.atlc.frp[atl.54, 52], 0.025)
quantile(t.atlc.frp[atl.54, 52], 0.975)
quantile(t.atlc.frp[atl.64, 52], 0.025)
quantile(t.atlc.frp[atl.64, 52], 0.975)

# Race summary
summary(t.atlc.frp[atl.b, 52])
summary(t.atlc.frp[atl.w, 52])

# Race quantiles
quantile(t.atlc.frp[atl.b, 52], 0.025)
quantile(t.atlc.frp[atl.b, 52], 0.975)
quantile(t.atlc.frp[atl.w, 52], 0.025)
quantile(t.atlc.frp[atl.w, 52], 0.975)


## ATL one-time

t.atli.frp <- t(atli.frp)
summary(t.atli.frp[, 52])
quantile(t.atli.frp[, 52], 0.025)
quantile(t.atli.frp[, 52], 0.975)

# Age summary
summary(t.atli.frp[atl.24, 52])
summary(t.atli.frp[atl.34, 52])
summary(t.atli.frp[atl.44, 52])
summary(t.atli.frp[atl.54, 52])
summary(t.atli.frp[atl.64, 52])

# Age quantiles
quantile(t.atli.frp[atl.24, 52], 0.025)
quantile(t.atli.frp[atl.24, 52], 0.975)
quantile(t.atli.frp[atl.34, 52], 0.025)
quantile(t.atli.frp[atl.34, 52], 0.975)
quantile(t.atli.frp[atl.44, 52], 0.025)
quantile(t.atli.frp[atl.44, 52], 0.975)
quantile(t.atli.frp[atl.54, 52], 0.025)
quantile(t.atli.frp[atl.54, 52], 0.975)
quantile(t.atli.frp[atl.64, 52], 0.025)
quantile(t.atli.frp[atl.64, 52], 0.975)

# Race summary
summary(t.atli.frp[atl.b, 52])
summary(t.atli.frp[atl.w, 52])

# Race quantiles
quantile(t.atli.frp[atl.b, 52], 0.025)
quantile(t.atli.frp[atl.b, 52], 0.975)
quantile(t.atli.frp[atl.w, 52], 0.025)
quantile(t.atli.frp[atl.w, 52], 0.975)


# Time steps: 6 months, 1 year, 2 years, 5 years
ts <- 52*c(0.5, 1, 2, 5)

### Table 2: All partnerships

## SF
sfa.sum1 <- summary(t(sfa.frp[ts, ]))
# sd(sfa.frp[ts, ])
frp1 <- summary(t(sfa.frp))
write.csv(frp1, "sf_frp_2_24_19.csv")

# Age ids
sfa.sum124 <- summary(t(sfa.frp[ts, sf.24]))
sfa.sum134 <- summary(t(sfa.frp[ts, sf.34]))
sfa.sum144 <- summary(t(sfa.frp[ts, sf.44]))
sfa.sum154 <- summary(t(sfa.frp[ts, sf.54]))
sfa.sum164 <- summary(t(sfa.frp[ts, sf.64]))

# Race ids
sfa.sum1b <- summary(t(sfa.frp[ts, sf.b]))
sfa.sum1w <- summary(t(sfa.frp[ts, sf.w]))

## ATL
atla.sum1 <- summary(t(atla.frp[ts,]))
frp2 <- summary(t(atla.frp))
write.csv(frp2, "atl_frp_2_24_19.csv")

# Age ids
atla.sum124 <- summary(t(atla.frp[ts, atl.24]))
atla.sum134 <- summary(t(atla.frp[ts, atl.34]))
atla.sum144 <- summary(t(atla.frp[ts, atl.44]))
atla.sum154 <- summary(t(atla.frp[ts, atl.54]))
atla.sum164 <- summary(t(atla.frp[ts, atl.64]))

# Race ids
atla.sum1b <- summary(t(atla.frp[ts, atl.b]))
atla.sum1w <- summary(t(atla.frp[ts, atl.w]))


### Table 3: Main partnerships


## SF
sfm.sum1 <- summary(t(sfm.frp[ts, ]))

# Age ids
sfm.sum124 <- summary(t(sfm.frp[ts, sf.24]))
sfm.sum134 <- summary(t(sfm.frp[ts, sf.34]))
sfm.sum144 <- summary(t(sfm.frp[ts, sf.44]))
sfm.sum154 <- summary(t(sfm.frp[ts, sf.54]))
sfm.sum164 <- summary(t(sfm.frp[ts, sf.64]))

# Race ids
sfm.sum1b <- summary(t(sfm.frp[ts, sf.b]))
sfm.sum1w <- summary(t(sfm.frp[ts, sf.w]))

## ATL
atlm.sum1 <- summary(t(atlm.frp[ts, ]))

# Age ids
atlm.sum124 <- summary(t(atlm.frp[ts, atl.24]))
atlm.sum134 <- summary(t(atlm.frp[ts, atl.34]))
atlm.sum144 <- summary(t(atlm.frp[ts, atl.44]))
atlm.sum154 <- summary(t(atlm.frp[ts, atl.54]))
atlm.sum164 <- summary(t(atlm.frp[ts, atl.64]))

# Race ids
atlm.sum1b <- summary(t(atlm.frp[ts, atl.b]))
atlm.sum1w <- summary(t(atlm.frp[ts, atl.w]))


### Table 4: Casual partnerships

## SF
sfc.sum1 <- summary(t(sfc.frp[ts, ]))

# Age ids
sfc.sum124 <- summary(t(sfc.frp[ts, sf.24]))
sfc.sum134 <- summary(t(sfc.frp[ts, sf.34]))
sfc.sum144 <- summary(t(sfc.frp[ts, sf.44]))
sfc.sum154 <- summary(t(sfc.frp[ts, sf.54]))
sfc.sum164 <- summary(t(sfc.frp[ts, sf.64]))

# Race ids
sfc.sum1b <- summary(t(sfc.frp[ts, sf.b]))
sfc.sum1w <- summary(t(sfc.frp[ts, sf.w]))

## ATL
atlc.sum1 <- summary(t(atlc.frp[ts, ]))

# Age ids
atlc.sum124 <- summary(t(atlc.frp[ts, atl.24]))
atlc.sum134 <- summary(t(atlc.frp[ts, atl.34]))
atlc.sum144 <- summary(t(atlc.frp[ts, atl.44]))
atlc.sum154 <- summary(t(atlc.frp[ts, atl.54]))
atlc.sum164 <- summary(t(atlc.frp[ts, atl.64]))

# Race ids
atlc.sum1b <- summary(t(atlc.frp[ts, atl.b]))
atlc.sum1w <- summary(t(atlc.frp[ts, atl.w]))


### Table 5: One-Time partnerships

## SF
sfi.sum1 <- summary(t(sfi.frp[ts, ]))

# Age ids
sfi.sum124 <- summary(t(sfi.frp[ts, sf.24]))
sfi.sum134 <- summary(t(sfi.frp[ts, sf.34]))
sfi.sum144 <- summary(t(sfi.frp[ts, sf.44]))
sfi.sum154 <- summary(t(sfi.frp[ts, sf.54]))
sfi.sum164 <- summary(t(sfi.frp[ts, sf.64]))

# Race ids
sfi.sum1b <- summary(t(sfi.frp[ts, sf.b]))
sfi.sum1w <- summary(t(sfi.frp[ts, sf.w]))

## ATL
atli.sum1 <- summary(t(atli.frp[ts, ]))

# Age ids
atli.sum124 <- summary(t(atli.frp[ts, atl.24]))
atli.sum134 <- summary(t(atli.frp[ts, atl.34]))
atli.sum144 <- summary(t(atli.frp[ts, atl.44]))
atli.sum154 <- summary(t(atli.frp[ts, atl.54]))
atli.sum164 <- summary(t(atli.frp[ts, atl.64]))

# Race ids
atli.sum1b <- summary(t(atli.frp[ts, atl.b]))
atli.sum1w <- summary(t(atli.frp[ts, atl.w]))

# Potentiall include
quantile(t.sfa.frp[, 52], 0.025)
quantile(t.sfa.frp[, 52], 0.975)


# 6.Mean FRP Plots --------------------------------------------------------

# par(mfrow = c(1,3))

## All partnerships (Table 2)

sfa.frp.av <- apply(sfa.frp, 1, mean)/10000
atla.frp.av <- apply(atla.frp, 1, mean)/10000

plot(x = 1:260, y = sfa.frp.av, type = "l", col = alpha("red", 0.7), lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable")
lines(x = 1:260, y = atla.frp.av, type = "l", col = alpha("blue", 0.7), 
      lwd = 2)
legend("bottomright", legend = c("San Francisco", "Atlanta"), 
       col = c("red", "blue"), lty = 1, cex(1.5))

# Race ids

sfa.b.av <- apply(sfa.frp[, sf.b], 1, mean)/10000
sfa.w.av <- apply(sfa.frp[, sf.w], 1, mean)/10000

atla.b.av <- apply(atla.frp[, atl.b], 1, mean)/10000
atla.w.av <- apply(atla.frp[, atl.w], 1, mean)/10000

plot(x = 1:260, y = sfa.b.av, type = "l", col = alpha("red", 0.5), lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable")
lines(x = 1:260, y = sfa.w.av, type = "l", col = alpha("blue", 0.5), 
      lwd = 2)
lines(x = 1:260, y = atla.b.av, type = "l", col = alpha("yellow", 0.5), 
      lwd = 2)
lines(x = 1:260, y = atla.w.av, type = "l", col = alpha("green", 0.5), 
      lwd = 2)
legend("bottomright", 
       legend = c("Black - SF", "White - SF", "Black - ATL", "White - ATL"), 
       col = c("red", "blue", "yellow", "green"), lty = 1)


# Age ids

pal <- adjustcolor(RColorBrewer::brewer.pal(5, "Set1"), alpha.f = 0.8)

sfa.24.av <- apply(sfa.frp[, sf.24], 1, mean)/10000
sfa.34.av <- apply(sfa.frp[, sf.34], 1, mean)/10000
sfa.44.av <- apply(sfa.frp[, sf.44], 1, mean)/10000
sfa.54.av <- apply(sfa.frp[, sf.54], 1, mean)/10000
sfa.64.av <- apply(sfa.frp[, sf.64], 1, mean)/10000

atla.24.av <- apply(atla.frp[, atl.24], 1, mean)/10000
atla.34.av <- apply(atla.frp[, atl.34], 1, mean)/10000
atla.44.av <- apply(atla.frp[, atl.44], 1, mean)/10000
atla.54.av <- apply(atla.frp[, atl.54], 1, mean)/10000
atla.64.av <- apply(atla.frp[, atl.64], 1, mean)/10000

par(mfrow = c(1,2))
plot(x = 1:260, y = sfa.24.av, type = "l", col = pal[1], lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable ", main = "San Francisco")
lines(x = 1:260, y = sfa.34.av, type = "l", col = pal[2], 
      lwd = 2)
lines(x = 1:260, y = sfa.44.av, type = "l", col = pal[3], 
      lwd = 2)
lines(x = 1:260, y = sfa.54.av, type = "l", col = pal[4], 
      lwd = 2)
lines(x = 1:260, y = sfa.64.av, type = "l", col = pal[5], 
      lwd = 2)
legend("bottomright", legend = c("15-24", "25-34", "35-44", "45-54", "55-64"), 
       col = pal, lty = 1)

plot(x = 1:260, y = atla.24.av, type = "l", col = pal[1], lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable", main = "Atlanta")
lines(x = 1:260, y = atla.34.av, type = "l", col = pal[2], 
      lwd = 2)
lines(x = 1:260, y = atla.44.av, type = "l", col = pal[3], 
      lwd = 2)
lines(x = 1:260, y = atla.54.av, type = "l", col = pal[4], 
      lwd = 2)
lines(x = 1:260, y = atla.64.av, type = "l", col = pal[5], 
      lwd = 2)
legend("bottomright", legend = c("15-24", "25-34", "35-44", "45-54", "55-64"), 
       col = pal, lty = 1)


## Main (Table 3)

sfm.frp.av <- apply(sfm.frp, 1, mean)/10000
atlm.frp.av <- apply(atlm.frp, 1, mean)/10000

par(mfrow = c(1,1))
plot(x = 1:260, y = sfm.frp.av, type = "l", col = alpha("red", 0.7), lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable")
lines(x = 1:260, y = atlm.frp.av, type = "l", col = alpha("blue", 0.7), 
      lwd = 2)
legend("bottomright", legend = c("San Francisco", "Atlanta"), 
       col = c("red", "blue"), lty = 1)

# Race ids

sfm.b.av <- apply(sfm.frp[, sf.b], 1, mean)/10000
sfm.w.av <- apply(sfm.frp[, sf.w], 1, mean)/10000

atlm.b.av <- apply(atlm.frp[, atl.b], 1, mean)/10000
atlm.w.av <- apply(atlm.frp[, atl.w], 1, mean)/10000

plot(x = 1:260, y = sfm.b.av, type = "l", col = alpha("red", 0.5), lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable")
lines(x = 1:260, y = sfm.w.av, type = "l", col = alpha("blue", 0.5), 
      lwd = 2)
lines(x = 1:260, y = atlm.b.av, type = "l", col = alpha("yellow", 0.5), 
      lwd = 2)
lines(x = 1:260, y = atlm.w.av, type = "l", col = alpha("green", 0.5), 
      lwd = 2)
legend("bottomright", 
       legend = c("Black - SF", "White - SF", "Black - ATL", "White - ATL"), 
       col = c("red", "blue", "yellow", "green"), lty = 1, cex = 0.8)

# Age ids

sfm.24.av <- apply(sfm.frp[, sf.24], 1, mean)/10000
sfm.34.av <- apply(sfm.frp[, sf.34], 1, mean)/10000
sfm.44.av <- apply(sfm.frp[, sf.44], 1, mean)/10000
sfm.54.av <- apply(sfm.frp[, sf.54], 1, mean)/10000
sfm.64.av <- apply(sfm.frp[, sf.64], 1, mean)/10000

atlm.24.av <- apply(atlm.frp[, atl.24], 1, mean)/10000
atlm.34.av <- apply(atlm.frp[, atl.34], 1, mean)/10000
atlm.44.av <- apply(atlm.frp[, atl.44], 1, mean)/10000
atlm.54.av <- apply(atlm.frp[, atl.54], 1, mean)/10000
atlm.64.av <- apply(atlm.frp[, atl.64], 1, mean)/10000

par(mfrow = c(1,2))
plot(x = 1:260, y = sfm.24.av, type = "l", col = pal[1], lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable", main = "San Francisco", 
     ylim = c(0.0001, 0.0005))
lines(x = 1:260, y = sfm.34.av, type = "l", col = pal[2], 
      lwd = 2)
lines(x = 1:260, y = sfm.44.av, type = "l", col = pal[3], 
      lwd = 2)
lines(x = 1:260, y = sfm.54.av, type = "l", col = pal[4], 
      lwd = 2)
lines(x = 1:260, y = sfm.64.av, type = "l", col = pal[5], 
      lwd = 2)
legend("topleft", legend = c("15-24", "25-34", "35-44", "45-54", "55-64"), 
       col = pal, lty = 1, cex = 0.8)

plot(x = 1:260, y = atlm.24.av, type = "l", col = pal[1], lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable", main = "Atlanta",
     ylim = c(0.0001, 0.0005))
lines(x = 1:260, y = atlm.34.av, type = "l", col = pal[2], 
      lwd = 2)
lines(x = 1:260, y = atlm.44.av, type = "l", col = pal[3], 
      lwd = 2)
lines(x = 1:260, y = atlm.54.av, type = "l", col = pal[4], 
      lwd = 2)
lines(x = 1:260, y = atlm.64.av, type = "l", col = pal[5], 
      lwd = 2)
legend("topleft", legend = c("15-24", "25-34", "35-44", "45-54", "55-64"), 
       col = pal, lty = 1, cex = 0.8)


## Casual (Table 4)

sfc.frp.av <- apply(sfc.frp, 1, mean)/10000
atlc.frp.av <- apply(atlc.frp, 1, mean)/10000

par(mfrow = c(1,1))

plot(x = 1:260, y = sfc.frp.av, type = "l", col = alpha("red", 0.7), lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable")
lines(x = 1:260, y = atlc.frp.av, type = "l", col = alpha("blue", 0.7), 
      lwd = 2)
legend("topleft", legend = c("San Francisco", "Atlanta"), 
       col = c("red", "blue"), lty = 1)

# atlc.frp[1:52, 1:25]
# apply(atlc.frp, 1, mean)

# Race ids

sfc.b.av <- apply(sfc.frp[, sf.b], 1, mean)/10000
sfc.w.av <- apply(sfc.frp[, sf.w], 1, mean)/10000

atlc.b.av <- apply(atlc.frp[, atl.b], 1, mean)/10000
atlc.w.av <- apply(atlc.frp[, atl.w], 1, mean)/10000

plot(x = 1:260, y = sfc.b.av, type = "l", col = alpha("red", 0.5), lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable")
lines(x = 1:260, y = sfc.w.av, type = "l", col = alpha("blue", 0.5), 
      lwd = 2)
lines(x = 1:260, y = atlc.b.av, type = "l", col = alpha("yellow", 0.5), 
      lwd = 2)
lines(x = 1:260, y = atlc.w.av, type = "l", col = alpha("green", 0.5), 
      lwd = 2)
legend("topleft", 
       legend = c("Black - SF", "White - SF", "Black - ATL", "White - ATL"), 
       col = c("red", "blue", "yellow", "green"), lty = 1, cex = 0.8)

# Age ids

sfc.24.av <- apply(sfc.frp[, sf.24], 1, mean)/10000
sfc.34.av <- apply(sfc.frp[, sf.34], 1, mean)/10000
sfc.44.av <- apply(sfc.frp[, sf.44], 1, mean)/10000
sfc.54.av <- apply(sfc.frp[, sf.54], 1, mean)/10000
sfc.64.av <- apply(sfc.frp[, sf.64], 1, mean)/10000

atlc.24.av <- apply(atlc.frp[, atl.24], 1, mean)/10000
atlc.34.av <- apply(atlc.frp[, atl.34], 1, mean)/10000
atlc.44.av <- apply(atlc.frp[, atl.44], 1, mean)/10000
atlc.54.av <- apply(atlc.frp[, atl.54], 1, mean)/10000
atlc.64.av <- apply(atlc.frp[, atl.64], 1, mean)/10000

pal <- adjustcolor(RColorBrewer::brewer.pal(5, "Set1"), alpha.f = 0.8)
jpeg("Plot3.jpeg", width = 8, height = 4, units = 'in', res = 300)
par(mfrow = c(1,2), mgp = c(2,1,0), mar = c(3,3,2,1))
plot(x = 1:260, y = sfc.24.av, type = "l", col = pal[1], lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable", main = "San Francisco")
lines(x = 1:260, y = sfc.34.av, type = "l", col = pal[2], 
      lwd = 2)
lines(x = 1:260, y = sfc.44.av, type = "l", col = pal[3], 
      lwd = 2)
lines(x = 1:260, y = sfc.54.av, type = "l", col = pal[4], 
      lwd = 2)
lines(x = 1:260, y = sfc.64.av, type = "l", col = pal[5], 
      lwd = 2)
legend("topleft", legend = c("15-24", "25-34", "35-44", "45-54", "55-64"), 
       col = pal, lty = 1, cex = 0.8)

plot(x = 1:260, y = atlc.24.av, type = "l", col = pal[1], lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable", main = "Atlanta")
lines(x = 1:260, y = atlc.34.av, type = "l", col = pal[2], 
      lwd = 2)
lines(x = 1:260, y = atlc.44.av, type = "l", col = pal[3], 
      lwd = 2)
lines(x = 1:260, y = atlc.54.av, type = "l", col = pal[4], 
      lwd = 2)
lines(x = 1:260, y = atlc.64.av, type = "l", col = pal[5], 
      lwd = 2)
legend("topleft", legend = c("15-24", "25-34", "35-44", "45-54", "55-64"), 
       col = pal, lty = 1, cex = 0.8)
dev.off()

## Inst (Table 5)

sfi.frp.av <- apply(sfi.frp, 1, mean)/10000
atli.frp.av <- apply(atli.frp, 1, mean)/10000

par(mfrow = c(1,1))

plot(x = 1:260, y = sfi.frp.av, type = "l", col = alpha("red", 0.7), lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable")
lines(x = 1:260, y = atli.frp.av, type = "l", col = alpha("blue", 0.7), 
      lwd = 2)
legend("bottomright", legend = c("San Francisco", "Atlanta"), 
       col = c("red", "blue"), lty = 1)

# Race ids

sfi.b.av <- apply(sfi.frp[, sf.b], 1, mean)/10000
sfi.w.av <- apply(sfi.frp[, sf.w], 1, mean)/10000

atli.b.av <- apply(atli.frp[, atl.b], 1, mean)/10000
atli.w.av <- apply(atli.frp[, atl.w], 1, mean)/10000

plot(x = 1:260, y = sfi.b.av, type = "l", col = alpha("red", 0.5), lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable")
lines(x = 1:260, y = sfi.w.av, type = "l", col = alpha("blue", 0.5), 
      lwd = 2)
lines(x = 1:260, y = atli.b.av, type = "l", col = alpha("yellow", 0.5), 
      lwd = 2)
lines(x = 1:260, y = atli.w.av, type = "l", col = alpha("green", 0.5), 
      lwd = 2)
legend("bottomright", 
       legend = c("Black - SF", "White - SF", "Black - ATL", "White - ATL"), 
       col = c("red", "blue", "yellow", "green"), lty = 1, cex = 0.8)

# Age ids

sfi.24.av <- apply(sfi.frp[, sf.24], 1, mean)/10000
sfi.34.av <- apply(sfi.frp[, sf.34], 1, mean)/10000
sfi.44.av <- apply(sfi.frp[, sf.44], 1, mean)/10000
sfi.54.av <- apply(sfi.frp[, sf.54], 1, mean)/10000
sfi.64.av <- apply(sfi.frp[, sf.64], 1, mean)/10000

atli.24.av <- apply(atli.frp[, atl.24], 1, mean)/10000
atli.34.av <- apply(atli.frp[, atl.34], 1, mean)/10000
atli.44.av <- apply(atli.frp[, atl.44], 1, mean)/10000
atli.54.av <- apply(atli.frp[, atl.54], 1, mean)/10000
atli.64.av <- apply(atli.frp[, atl.64], 1, mean)/10000

par(mfrow = c(1,2))
plot(x = 1:260, y = sfi.24.av, type = "l", col = pal[1], lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable", main = "San Francisco",
     ylim = c(0, 0.35))
lines(x = 1:260, y = sfi.34.av, type = "l", col = pal[2], 
      lwd = 2)
lines(x = 1:260, y = sfi.44.av, type = "l", col = pal[3], 
      lwd = 2)
lines(x = 1:260, y = sfi.54.av, type = "l", col = pal[4], 
      lwd = 2)
lines(x = 1:260, y = sfi.64.av, type = "l", col = pal[5], 
      lwd = 2)
legend("bottomright", legend = c("15-24", "25-34", "35-44", "45-54", "55-64"), 
       col = pal, lty = 1, cex = 0.7)

plot(x = 1:260, y = atli.24.av, type = "l", col = pal[1], lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable", main = "Atlanta", 
     ylim = c(0, 0.35))
lines(x = 1:260, y = atli.34.av, type = "l", col = pal[2], 
      lwd = 2)
lines(x = 1:260, y = atli.44.av, type = "l", col = pal[3], 
      lwd = 2)
lines(x = 1:260, y = atli.54.av, type = "l", col = pal[4], 
      lwd = 2)
lines(x = 1:260, y = atli.64.av, type = "l", col = pal[5], 
      lwd = 2)
legend("bottomright", legend = c("15-24", "25-34", "35-44", "45-54", "55-64"), 
       col = pal, lty = 1, cex = 0.7)



# 7.Distribution of FRPs --------------------------------------------------

# par(mfrow = c(1, 2), oma = c(2, 0, 2, 0), xpd = NA)
par(mfrow = c(1, 2))
# pal <- adjustcolor(RColorBrewer::brewer.pal(5, "Set1"), alpha.f = 0.6)
matplot(sfa.frp/10000, type = "l", xlab = "Week", ylab = "FRP",
        col = alpha("red", 0.7))
matplot(atla.frp/10000, type = "l", xlab = "Week", ylab = "FRP",
        col = alpha("blue", 0.7))

par(mfrow = c(1, 2))
# pal <- adjustcolor(RColorBrewer::brewer.pal(5, "Set1"), alpha.f = 0.6)
matplot(sfa.frp/10000, type = "l", xlab = "Week", ylab = "FRP")
matplot(atla.frp/10000, type = "l", xlab = "Week", ylab = "FRP")

matplot(sfc.frp/10000, type = "l", xlab = "Week", ylab = "FRP")
matplot(atlc.frp/10000, type = "l", xlab = "Week", ylab = "FRP")

plot(x = 1:260, y = sfa.frp.av, type = "l", col = alpha("red", 0.7), lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable")
lines(x = 1:260, y = atla.frp.av, type = "l", col = alpha("blue", 0.7), 
      lwd = 2)
legend("bottomright", legend = c("San Francisco", "Atlanta"), 
       col = c("red", "blue"), lty = 1, cex(1.5))


#title()

library("viridis")
library("wesanderson")
palv <- viridis(n = 4, alpha = 0.25, option = "inferno")
palv <- adjustcolor(wes_palette(5, name = "Zissou1"), alpha.f = 1)
palv <- rainbow(10)

## All partnerships
jpeg("Plot2.jpeg", width = 8, height = 4, units = 'in', res = 250)
par(mfrow = c(2, 3), oma = c(0, 0, 0, 0), xpd = NA, mgp = c(2,1,0), 
    mar = c(3,3,2,1))
# SF
matplot(sfm.frp, type = "l", ylim = c(0, 30), xlab = "", ylab = "FRP", lty = 1,
        col = palv, lwd = 0.1, main = "SF Main")
matplot(sfc.frp, type = "l", ylim = c(0, 10000), xlab = "", ylab = "", lty = 1,
        col = palv, lwd = 0.1, main = "SF Casual")
matplot(sfi.frp, type = "l", ylim = c(0, 10000), xlab = "", ylab = "", lty = 1,
        col = palv, lwd = 0.1, main = "SF One-Time")

# ATL
matplot(atlm.frp, type = "l", ylim = c(0, 30), xlab = "Week", ylab = "FRP", lty = 1,
        col = palv, lwd = 0.1, main = "ATL Main")
matplot(atlc.frp, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "", lty = 1,
        col = palv, lwd = 0.1, main = "ATL Casual")
matplot(atli.frp, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "", lty = 1,
        col = palv, lwd = 0.1, main = "ATL One-Time")

dev.off()

# title("Distribution of 5-Year Foward Reachable Paths by Partnership Type", 
      # outer = TRUE)



par(mfrow = c(2, 3), oma = c(2, 0, 2, 0), xpd = NA)
par(mfrow = c(1,3))
## All partnerships

# SF
matplot(sfm.frp/10000, type = "l", xlab = "Week", ylab = "FRP")
matplot(sfc.frp/10000, type = "l", ylim = c(0, 1), xlab = "Week", ylab = "FRP")
matplot(sfi.frp/10000, type = "l", ylim = c(0, 1), xlab = "Week", ylab = "FRP")
matplot(atlm.frp/10000, type = "l", xlab = "Week", ylab = "FRP")
matplot(atlc.frp/10000, type = "l", ylim = c(0, 1), xlab = "Week", ylab = "FRP")
matplot(atli.frp/10000, type = "l", ylim = c(0, 1), xlab = "Week", ylab = "FRP")




# 8.Box plots of FRPs -----------------------------------------------------

par(mfrow = c(1,1))

# experiment with box plots
df <- t(atlm.frp[ts, atl.24])
boxplot(df)
# boxplot(df, outline = FALSE)

df <- t(sfc.frp[ts, ])
boxplot(df)


# Plots for poster --------------------------------------------------------


par(mfrow = c(2, 1))

## Overall
plot(x = 1:260, y = sfm.frp.av, type = "l", col = alpha("red", 0.7), lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable", main = "Main")
lines(x = 1:260, y = atlm.frp.av, type = "l", col = alpha("blue", 0.7), 
      lwd = 2)
legend("topleft", legend = c("San Francisco", "Atlanta"), 
       col = c("red", "blue"), lty = 1)

plot(x = 1:260, y = sfc.frp.av, type = "l", col = alpha("red", 0.7), lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable", main = "Casual")
lines(x = 1:260, y = atlc.frp.av, type = "l", col = alpha("blue", 0.7), 
      lwd = 2)
# legend("topleft", legend = c("San Francisco", "Atlanta"), 
#        col = c("red", "blue"), lty = 1)

plot(x = 1:260, y = sfi.frp.av, type = "l", col = alpha("red", 0.7), lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable", main = "One-Time")
lines(x = 1:260, y = atli.frp.av, type = "l", col = alpha("blue", 0.7), 
      lwd = 2)
# legend("topleft", legend = c("San Francisco", "Atlanta"), 
#        col = c("red", "blue"), lty = 1)


# Race
plot(x = 1:260, y = sfm.b.av, type = "l", col = pal[1], lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable", 
     ylim = c(0.0001, 0.0003))
lines(x = 1:260, y = sfm.w.av, type = "l", col = pal[2], 
      lwd = 2)
lines(x = 1:260, y = atlm.b.av, type = "l", col = pal[3], 
      lwd = 2)
lines(x = 1:260, y = atlm.w.av, type = "l", col = pal[4], 
      lwd = 2)
legend("topleft", 
       legend = c("Black - SF", "White - SF", "Black - ATL", "White - ATL"), 
       col = c(pal[1], pal[2], pal[3], pal[4]), lty = 1, cex = 0.8)

plot(x = 1:260, y = sfc.b.av, type = "l", col = pal[1], lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable")
lines(x = 1:260, y = sfc.w.av, type = "l", col = pal[2], 
      lwd = 2)
lines(x = 1:260, y = atlc.b.av, type = "l", col = pal[3], 
      lwd = 2)
lines(x = 1:260, y = atlc.w.av, type = "l", col = pal[4], 
      lwd = 2)
# legend("topleft", 
#        legend = c("Black - SF", "White - SF", "Black - ATL", "White - ATL"), 
#        col = c("red", "blue", "yellow", "green"), lty = 1, cex = 0.8)

plot(x = 1:260, y = sfi.b.av, type = "l", col = pal[1], lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable")
lines(x = 1:260, y = sfi.w.av, type = "l", col = pal[2], 
      lwd = 2)
lines(x = 1:260, y = atli.b.av, type = "l", col = pal[3], 
      lwd = 2)
lines(x = 1:260, y = atli.w.av, type = "l", col = pal[4], 
      lwd = 2)
# legend("topleft", 
#        legend = c("Black - SF", "White - SF", "Black - ATL", "White - ATL"), 
#        col = c("red", "blue", "yellow", "green"), lty = 1, cex = 0.8)

# Age
par(mfrow = c(1, 2))

# SF
plot(x = 1:260, y = sfm.24.av, type = "l", col = pal[1], lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable", main = "SF - Main", 
     ylim = c(0.0001, 0.0005))
lines(x = 1:260, y = sfm.34.av, type = "l", col = pal[2], 
      lwd = 2)
lines(x = 1:260, y = sfm.44.av, type = "l", col = pal[3], 
      lwd = 2)
lines(x = 1:260, y = sfm.54.av, type = "l", col = pal[4], 
      lwd = 2)
lines(x = 1:260, y = sfm.64.av, type = "l", col = pal[5], 
      lwd = 2)
legend("topleft", legend = c("15-24", "25-34", "35-44", "45-54", "55-64"), 
       col = pal, lty = 1, cex = 0.2)

plot(x = 1:260, y = sfc.24.av, type = "l", col = pal[1], lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable", ylim = c(0, 1))
lines(x = 1:260, y = sfc.34.av, type = "l", col = pal[2], 
      lwd = 2)
lines(x = 1:260, y = sfc.44.av, type = "l", col = pal[3], 
      lwd = 2)
lines(x = 1:260, y = sfc.54.av, type = "l", col = pal[4], 
      lwd = 2)
lines(x = 1:260, y = sfc.64.av, type = "l", col = pal[5], 
      lwd = 2)
legend("topleft", legend = c("15-24", "25-34", "35-44", "45-54", "55-64"), 
        col = pal, lty = 1, cex = 0.8)

plot(x = 1:260, y = sfi.24.av, type = "l", col = pal[1], lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable", main = "SF - One-Time",
     ylim = c(0, 0.35))
lines(x = 1:260, y = sfi.34.av, type = "l", col = pal[2], 
      lwd = 2)
lines(x = 1:260, y = sfi.44.av, type = "l", col = pal[3], 
      lwd = 2)
lines(x = 1:260, y = sfi.54.av, type = "l", col = pal[4], 
      lwd = 2)
lines(x = 1:260, y = sfi.64.av, type = "l", col = pal[5], 
      lwd = 2)
# legend("bottomright", legend = c("15-24", "25-34", "35-44", "45-54", "55-64"), 
#        col = pal, lty = 1, cex = 0.7)

# ATL
plot(x = 1:260, y = atlm.24.av, type = "l", col = pal[1], lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable", main = "ATL - Main",
     ylim = c(0.0001, 0.0005))
lines(x = 1:260, y = atlm.34.av, type = "l", col = pal[2], 
      lwd = 2)
lines(x = 1:260, y = atlm.44.av, type = "l", col = pal[3], 
      lwd = 2)
lines(x = 1:260, y = atlm.54.av, type = "l", col = pal[4], 
      lwd = 2)
lines(x = 1:260, y = atlm.64.av, type = "l", col = pal[5], 
      lwd = 2)
# legend("topleft", legend = c("15-24", "25-34", "35-44", "45-54", "55-64"), 
#        col = pal, lty = 1, cex = 0.8)

plot(x = 1:260, y = sfc.24.av, type = "l", col = pal[1], lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable")
lines(x = 1:260, y = sfc.34.av, type = "l", col = pal[2], 
      lwd = 2)
lines(x = 1:260, y = sfc.44.av, type = "l", col = pal[3], 
      lwd = 2)
lines(x = 1:260, y = sfc.54.av, type = "l", col = pal[4], 
      lwd = 2)
lines(x = 1:260, y = sfc.64.av, type = "l", col = pal[5], 
      lwd = 2)
legend("topleft", legend = c("15-24", "25-34", "35-44", "45-54", "55-64"), 
       col = pal, lty = 1, cex = 0.8)

plot(x = 1:260, y = atlc.24.av, type = "l", col = pal[1], lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable")
lines(x = 1:260, y = atlc.34.av, type = "l", col = pal[2], 
      lwd = 2)
lines(x = 1:260, y = atlc.44.av, type = "l", col = pal[3], 
      lwd = 2)
lines(x = 1:260, y = atlc.54.av, type = "l", col = pal[4], 
      lwd = 2)
lines(x = 1:260, y = atlc.64.av, type = "l", col = pal[5], 
      lwd = 2)
# legend("topleft", legend = c("15-24", "25-34", "35-44", "45-54", "55-64"), 
#        col = pal, lty = 1)

plot(x = 1:260, y = atli.24.av, type = "l", col = pal[1], lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable", main = "ATL - One-Time", 
     ylim = c(0, 0.35))
lines(x = 1:260, y = atli.34.av, type = "l", col = pal[2], 
      lwd = 2)
lines(x = 1:260, y = atli.44.av, type = "l", col = pal[3], 
      lwd = 2)
lines(x = 1:260, y = atli.54.av, type = "l", col = pal[4], 
      lwd = 2)
lines(x = 1:260, y = atli.64.av, type = "l", col = pal[5], 
      lwd = 2)
# legend("bottomright", legend = c("15-24", "25-34", "35-44", "45-54", "55-64"), 
#        col = pal, lty = 1)



plot(x = 1:260, y = sfc.24.av, type = "l", col = pal[1], lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable", ylim = c(0, 1))
lines(x = 1:260, y = sfc.34.av, type = "l", col = pal[2], 
      lwd = 2)
lines(x = 1:260, y = sfc.44.av, type = "l", col = pal[3], 
      lwd = 2)
lines(x = 1:260, y = sfc.54.av, type = "l", col = pal[4], 
      lwd = 2)
lines(x = 1:260, y = sfc.64.av, type = "l", col = pal[5], 
      lwd = 2)
legend("topleft", legend = c("15-24", "25-34", "35-44", "45-54", "55-64"), 
       col = pal, lty = 1, cex = 0.8)


# END ---------------------------------------------------------------------


