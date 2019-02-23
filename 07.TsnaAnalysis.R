## TSNA Analysis
## v0.2
## 2/16/2019

library("tsna")
library("networkDynamicData")
suppressMessages(library("EpiModel"))
library("scales")

set.seed(803)


# 0.Network data ----------------------------------------------------------


## Read in sims 

sim.sf <- readRDS("DataV2/artnet.NetSim.SanFrancisco.rda")
sim.atl <- readRDS("DataV2/artnet.NetSim.Atlanta.rda")

## Extract each partner type network

# SF
net.sfm <- sim.sf[[1]] # main
net.sfc <- sim.sf[[2]] # casual
net.sfi <- sim.sf[[3]] # inst

# ATL
net.atlm <- sim.atl[[1]] # main
net.atlc <- sim.atl[[2]] # casual
net.atli <- sim.atl[[3]] # inst



# 1.Outcome data ----------------------------------------------------------


## Function to load outcome data

load_data <- function(file) {
  x <- load(file = file)
  x <- out
}

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



# 2.Validation ------------------------------------------------------------

test_ts <- 260

## Checking degree in simulations
test1_inp <- EpiModel::get_degree(network.collapse(net.sfm, at = test_ts))
test2_inp <- EpiModel::get_degree(network.collapse(net.sfc, at = test_ts))
test3_inp <- EpiModel::get_degree(network.collapse(net.sfi, at = test_ts))
test4_inp <- EpiModel::get_degree(network.collapse(net.atlm, at = test_ts))
test5_inp <- EpiModel::get_degree(network.collapse(net.atlc, at = test_ts))
test6_inp <- EpiModel::get_degree(network.collapse(net.atli, at = test_ts))

## Checking degree in outcome data
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



# 3.Vertex IDs ------------------------------------------------------------

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


# ## Race & Age
# 
# # Black 0-24
# sf.b24 <- which(get.vertex.attribute(net.sfm, "race") == "B" & 
#                      get.vertex.attribute(net.sfm, "age.grp") == "1")
# atl.b24 <- which(get.vertex.attribute(net.atlm, "race") == "B" & 
#                       get.vertex.attribute(net.atlm, "age.grp") == "1")
# 
# # White 0-24
# sf.w24 <- which(get.vertex.attribute(net.sfm, "race") == "W" & 
#                      get.vertex.attribute(net.sfm, "age.grp") == "1")
# atl.w24 <- which(get.vertex.attribute(net.atlm, "race") == "W" & 
#                       get.vertex.attribute(net.atlm, "age.grp") == "1")
# 
# 
# # Black 25-34
# sf.b34 <- which(get.vertex.attribute(net.sfm, "race") == "B" & 
#                      get.vertex.attribute(net.sfm, "age.grp") == "2")
# atl.b34 <- which(get.vertex.attribute(net.atlm, "race") == "B" & 
#                       get.vertex.attribute(net.atlm, "age.grp") == "2")
# 
# # White 25-34
# sf.w34 <- which(get.vertex.attribute(net.sfm, "race") == "W" & 
#                      get.vertex.attribute(net.sfm, "age.grp") == "2")
# atl.w34 <- which(get.vertex.attribute(net.atlm, "race") == "W" & 
#                       get.vertex.attribute(net.atlm, "age.grp") == "2")
# 
# # Black 35-44
# sf.b44 <- which(get.vertex.attribute(net.sfm, "race") == "B" & 
#                      get.vertex.attribute(net.sfm, "age.grp") == "3")
# atl.b44 <- which(get.vertex.attribute(net.atlm, "race") == "B" & 
#                       get.vertex.attribute(net.atlm, "age.grp") == "3")
# 
# # White 35-44
# sf.w44 <- which(get.vertex.attribute(net.sfm, "race") == "W" & 
#                      get.vertex.attribute(net.sfm, "age.grp") == "3")
# atl.w44 <- which(get.vertex.attribute(net.atlm, "race") == "W" & 
#                       get.vertex.attribute(net.atlm, "age.grp") == "3")
# 
# # Black 45-54
# sf.b54 <- which(get.vertex.attribute(net.sfm, "race") == "B" & 
#                      get.vertex.attribute(net.sfm, "age.grp") == "4")
# atl.b54 <- which(get.vertex.attribute(net.atlm, "race") == "B" & 
#                       get.vertex.attribute(net.atlm, "age.grp") == "4")
# 
# # White 45-54
# sf.w54 <- which(get.vertex.attribute(net.sfm, "race") == "W" & 
#                      get.vertex.attribute(net.sfm, "age.grp") == "4")
# atl.w54 <- which(get.vertex.attribute(net.atlm, "race") == "W" & 
#                       get.vertex.attribute(net.atlm, "age.grp") == "4")
# 
# # Black 55+
# sf.b64 <- which(get.vertex.attribute(net.sfm, "race") == "B" & 
#                      get.vertex.attribute(net.sfm, "age.grp") == "5")
# atl.b64 <- which(get.vertex.attribute(net.atlm, "race") == "B" & 
#                       get.vertex.attribute(net.atlm, "age.grp") == "5")
# 
# # White 55+
# sf.w64 <- which(get.vertex.attribute(net.sfm, "race") == "W" & 
#                      get.vertex.attribute(net.sfm, "age.grp") == "5")
# atl.w64 <- which(get.vertex.attribute(net.atlm, "race") == "W" & 
#                       get.vertex.attribute(net.atlm, "age.grp") == "5")



# 4.FRP Analysis ----------------------------------------------------------

## Subset df to outcome of interest

# All partnerships
sfa.frp <- sf.all$frp
atla.frp <- atl.all$frp

# Main
sfm.frp <- sf.main$frp
atlm.frp <- atl.main$frp

# Casual
sfc.frp <- sf.casl$frp
atlc.frp <- atl.casl$frp

# Inst
sfi.frp <- sf.inst$frp
atli.frp <- atl.inst$frp


# 5.FRP Summary -----------------------------------------------------------

# Time steps: 6 months, 1 year, 2 years, 5 years
ts <- 52*c(0.5, 1, 2, 5)

# summary_data <- function(df, ts, ids){
#   summary <- summary(t(df[ts, ids]))
#   sd <- sd(df[ts, ])
#   return(c(summary, sd))
# }

# test <- summary_data(sfa.frp, ts, sf.24)

### Table 2: All partnerships

## SF
sfa.sum1 <- summary(t(sfa.frp[ts, ]))
# sd(sfa.frp[ts, ])

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

# EA continue formatting as above

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



# 6.Mean FRP Plots --------------------------------------------------------


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


# box plots ---------------------------------------------------------------

# experiment with box plots
df <- t(atlm.frp[52*c(0.5, 1, 2, 4, 5), atl.24])
boxplot(df)
boxplot(df, outline = FALSE)


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

par(mfrow = c(1,2))
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


## Inst (Table 5)

sfi.frp.med <- apply(sfi.frp/10000, 1, median)
atli.frp.med <- apply(atli.frp/10000, 1, median)

plot(x = 1:260, y = sfi.frp.med, type = "l", col = alpha("red", 0.7), lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable")
lines(x = 1:260, y = atli.frp.med, type = "l", col = alpha("blue", 0.7), 
      lwd = 2)
legend("topleft", legend = c("San Francisco", "Atlanta"), 
       col = c("red", "blue"), lty = 1)

# SF - Race ids

sfi.b.med <- apply(sfi.frp[sf.b]/10000, 1, median)
sfi.w.med <- apply(sfi.frp[sf.w]/10000, 1, median)

plot(x = 1:260, y = sfi.b.med, type = "l", col = alpha("red", 0.5), lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable")
lines(x = 1:260, y = sfi.w.med, type = "l", col = alpha("blue", 0.5), 
      lwd = 2)
legend("bottomright", legend = c("Black", "White"), col = c("red", "blue"), 
       lty = 1)

# SF - Age ids

sfi.24.med <- apply(sfi.frp[sf.24]/10000, 1, median)
sfi.34.med <- apply(sfi.frp[sf.34]/10000, 1, median)
sfi.44.med <- apply(sfi.frp[sf.44]/10000, 1, median)
sfi.54.med <- apply(sfi.frp[sf.54]/10000, 1, median)
sfi.64.med <- apply(sfi.frp[sf.64]/10000, 1, median)

plot(x = 1:260, y = sfi.24.med, type = "l", col = alpha("red", 0.7), lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable ")
lines(x = 1:260, y = sfi.34.med, type = "l", col = alpha("blue", 0.7), 
      lwd = 2)
lines(x = 1:260, y = sfi.44.med, type = "l", col = alpha("yellow", 0.7), 
      lwd = 2)
lines(x = 1:260, y = sfi.54.med, type = "l", col = alpha("green", 0.7), 
      lwd = 2)
lines(x = 1:260, y = sfi.64.med, type = "l", col = alpha("purple", 0.7), 
      lwd = 2)
legend("topleft", legend = c("15-24", "25-34", "35-44", "45-54", "55-64"), 
       col = c("red", "blue", "yellow", "green", "purple"), lty = 1)

# ATL - Race ids

atli.b.med <- apply(atli.frp[atl.b]/10000, 1, median)
atli.w.med <- apply(atli.frp[atl.w]/10000, 1, median)

plot(x = 1:260, y = atli.b.med, type = "l", col = alpha("red", 0.5), lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable")
lines(x = 1:260, y = atli.w.med, type = "l", col = alpha("blue", 0.5), 
      lwd = 2)
legend("bottomright", legend = c("Black", "White"), col = c("red", "blue"), 
       lty = 1)

# ATL - Age ids

atli.24.med <- apply(atli.frp[atl.24]/10000, 1, median)
atli.34.med <- apply(atli.frp[atl.34]/10000, 1, median)
atli.44.med <- apply(atli.frp[atl.44]/10000, 1, median)
atli.54.med <- apply(atli.frp[atl.54]/10000, 1, median)
atli.64.med <- apply(atli.frp[atl.64]/10000, 1, median)

plot(x = 1:260, y = atli.24.med, type = "l", col = alpha("red", 0.7), lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable ", ylim = c(0, 0.6))
lines(x = 1:260, y = atli.34.med, type = "l", col = alpha("blue", 0.7), 
      lwd = 2)
lines(x = 1:260, y = atli.44.med, type = "l", col = alpha("yellow", 0.7), 
      lwd = 2)
lines(x = 1:260, y = atli.54.med, type = "l", col = alpha("green", 0.7), 
      lwd = 2)
lines(x = 1:260, y = atli.64.med, type = "l", col = alpha("purple", 0.7), 
      lwd = 2)
legend("topleft", legend = c("15-24", "25-34", "35-44", "45-54", "55-64"), 
       col = c("red", "blue", "yellow", "green", "purple"), lty = 1)

# END ---------------------------------------------------------------------


