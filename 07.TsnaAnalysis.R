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
  # x <- as.data.frame(x)
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
atli.frp <- sf.inst$frp


# 5.FRP Summary -----------------------------------------------------------

# unclear how you were subsetting the numerical results below, but I set up for the 
# four time points of interest. You should put both mean and medians in the tables
ts <- 52*c(0.5, 1, 2, 5)

### Table 2: All partnerships

## SF
sfa.sum1 <- summary(t(sfa.frp[ts, ])/10000)


# Age ids
sfa.sum124 <- summary(t(sfa.frp[ts, sf.24]/10000))
sfa.sum134 <- summary(t(sfa.frp[ts, sf.34]/10000))
sfa.sum144 <- summary(t(sfa.frp[ts, sf.44]/10000))
sfa.sum154 <- summary(t(sfa.frp[ts, sf.54]/10000))
sfa.sum164 <- summary(t(sfa.frp[ts, sf.64]/10000))

# Race ids
sfa.sum1b <- summary(t(sfa.frp[ts, sf.b]/10000))
sfa.sum1w <- summary(t(sfa.frp[ts, sf.w]/10000))

## ATL
atla.sum1 <- summary(t(atla.frp[ts,] /10000))

# Age ids
atla.sum124 <- summary(t(atla.frp[ts, atl.24]/10000))
atla.sum134 <- summary(t(atla.frp[ts, atl.34]/10000))
atla.sum144 <- summary(t(atla.frp[ts, atl.44]/10000))
atla.sum154 <- summary(t(atla.frp[ts, atl.54]/10000))
atla.sum164 <- summary(t(atla.frp[ts, atl.64]/10000))

# Race ids
atla.sum1b <- summary(t(atla.frp[ts, atl.b]/10000))
atla.sum1w <- summary(t(atla.frp[ts, atl.w]/10000))


### Table 3: Main partnerships

# EA continue formatting as above

## SF
sfm.sum1 <- summary(t(sfm.frp/10000))

# Age ids
sfm.sum124 <- summary(t(sfm.frp[sf.24]/10000))
sfm.sum134 <- summary(t(sfm.frp[sf.34]/10000))
sfm.sum144 <- summary(t(sfm.frp[sf.44]/10000))
sfm.sum154 <- summary(t(sfm.frp[sf.54]/10000))
sfm.sum164 <- summary(t(sfm.frp[sf.64]/10000))

# Race ids
sfm.sum1b <- summary(t(sfm.frp[sf.b]/10000))
sfm.sum1w <- summary(t(sfm.frp[sf.w]/10000))

## ATL
atlm.sum1 <- summary(t(atlm.frp/10000))

# Age ids
atlm.sum124 <- summary(t(atlm.frp[atl.24]/10000))
atlm.sum134 <- summary(t(atlm.frp[atl.34]/10000))
atlm.sum144 <- summary(t(atlm.frp[atl.44]/10000))
atlm.sum154 <- summary(t(atlm.frp[atl.54]/10000))
atlm.sum164 <- summary(t(atlm.frp[atl.64]/10000))

# Race ids
atlm.sum1b <- summary(t(atlm.frp[atl.b]/10000))
atlm.sum1w <- summary(t(atlm.frp[atl.w]/10000))


### Table 4: Casual partnerships

## SF
sfc.sum1 <- summary(t(sfc.frp/10000))

# Age ids
sfc.sum124 <- summary(t(sfc.frp[sf.24]/10000))
sfc.sum134 <- summary(t(sfc.frp[sf.34]/10000))
sfc.sum144 <- summary(t(sfc.frp[sf.44]/10000))
sfc.sum154 <- summary(t(sfc.frp[sf.54]/10000))
sfc.sum164 <- summary(t(sfc.frp[sf.64]/10000))

# Race ids
sfc.sum1b <- summary(t(sfc.frp[sf.b]/10000))
sfc.sum1w <- summary(t(sfc.frp[sf.w]/10000))

## ATL
atlc.sum1 <- summary(t(atlc.frp/10000))

# Age ids
atlc.sum124 <- summary(t(atlc.frp[atl.24]/10000))
atlc.sum134 <- summary(t(atlc.frp[atl.34]/10000))
atlc.sum144 <- summary(t(atlc.frp[atl.44]/10000))
atlc.sum154 <- summary(t(atlc.frp[atl.54]/10000))
atlc.sum164 <- summary(t(atlc.frp[atl.64]/10000))

# Race ids
atlc.sum1b <- summary(t(atlc.frp[atl.b]/10000))
atlc.sum1w <- summary(t(atlc.frp[atl.w]/10000))


### Table 5: One-Time partnerships

## SF
sfi.sum1 <- summary(t(sfi.frp/10000))

# Age ids
sfi.sum124 <- summary(t(sfi.frp[sf.24]/10000))
sfi.sum134 <- summary(t(sfi.frp[sf.34]/10000))
sfi.sum144 <- summary(t(sfi.frp[sf.44]/10000))
sfi.sum154 <- summary(t(sfi.frp[sf.54]/10000))
sfi.sum164 <- summary(t(sfi.frp[sf.64]/10000))

# Race ids
sfi.sum1b <- summary(t(sfi.frp[sf.b]/10000))
sfi.sum1w <- summary(t(sfi.frp[sf.w]/10000))

## ATL
atli.sum1 <- summary(t(atli.frp/10000))

# Age ids
atli.sum124 <- summary(t(atli.frp[atl.24]/10000))
atli.sum134 <- summary(t(atli.frp[atl.34]/10000))
atli.sum144 <- summary(t(atli.frp[atl.44]/10000))
atli.sum154 <- summary(t(atli.frp[atl.54]/10000))
atli.sum164 <- summary(t(atli.frp[atl.64]/10000))

# Race ids
atli.sum1b <- summary(t(atli.frp[atl.b]/10000))
atli.sum1w <- summary(t(atli.frp[atl.w]/10000))



# 6.FRP Median Plots ------------------------------------------------------

## All partnerships (Table 2)

sfa.frp.med <- apply(sfa.frp, 1, mean)/10000
atla.frp.med <- apply(atla.frp, 1, mean)/10000

plot(x = 1:260, y = sfa.frp.med, type = "l", col = alpha("red", 0.7), lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable")
lines(x = 1:260, y = atla.frp.med, type = "l", col = alpha("blue", 0.7), 
      lwd = 2)
legend("bottomright", legend = c("San Francisco", "Atlanta"), 
       col = c("red", "blue"), lty = 1)

# SF - Race ids

sfa.b.med <- apply(sfa.frp[, sf.b], 1, mean)/10000
sfa.w.med <- apply(sfa.frp[, sf.w], 1, mean)/10000

atla.b.med <- apply(atla.frp[, atl.b], 1, mean)/10000
atla.w.med <- apply(atla.frp[, atl.w], 1, mean)/10000

plot(x = 1:260, y = sfa.b.med, type = "l", col = alpha("red", 0.5), lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable")
lines(x = 1:260, y = sfa.w.med, type = "l", col = alpha("blue", 0.5), 
      lwd = 2)
lines(x = 1:260, y = atla.b.med, type = "l", col = alpha("yellow", 0.5), 
      lwd = 2)
lines(x = 1:260, y = atla.w.med, type = "l", col = alpha("green", 0.5), 
      lwd = 2)
# fix legend
legend("bottomright", legend = c("Black", "White"), col = c("red", "blue"), 
       lty = 1)


# SF - Age ids

pal <- adjustcolor(RColorBrewer::brewer.pal(5, "Set1"), alpha.f = 0.8)

sfa.24.med <- apply(sfa.frp[, sf.24], 1, mean)/10000
sfa.34.med <- apply(sfa.frp[, sf.34], 1, mean)/10000
sfa.44.med <- apply(sfa.frp[, sf.44], 1, mean)/10000
sfa.54.med <- apply(sfa.frp[, sf.54], 1, mean)/10000
sfa.64.med <- apply(sfa.frp[, sf.64], 1, mean)/10000

par(mfrow = c(1,2))
plot(x = 1:260, y = sfa.24.med, type = "l", col = pal[1], lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable ")
lines(x = 1:260, y = sfa.34.med, type = "l", col = pal[2], 
      lwd = 2)
lines(x = 1:260, y = sfa.44.med, type = "l", col = pal[3], 
      lwd = 2)
lines(x = 1:260, y = sfa.54.med, type = "l", col = pal[4], 
      lwd = 2)
lines(x = 1:260, y = sfa.64.med, type = "l", col = pal[5], 
      lwd = 2)
legend("bottomright", legend = c("15-24", "25-34", "35-44", "45-54", "55-64"), 
       col = pal, lty = 1)

# ATL - Race ids


# ATL - Age ids

atla.24.med <- apply(atla.frp[, atl.24], 1, mean)/10000
atla.34.med <- apply(atla.frp[, atl.34], 1, mean)/10000
atla.44.med <- apply(atla.frp[, atl.44], 1, mean)/10000
atla.54.med <- apply(atla.frp[, atl.54], 1, mean)/10000
atla.64.med <- apply(atla.frp[, atl.64], 1, mean)/10000

plot(x = 1:260, y = atla.24.med, type = "l", col = pal[1], lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable ")
lines(x = 1:260, y = atla.34.med, type = "l", col = pal[2], 
      lwd = 2)
lines(x = 1:260, y = atla.44.med, type = "l", col = pal[3], 
      lwd = 2)
lines(x = 1:260, y = atla.54.med, type = "l", col = pal[4], 
      lwd = 2)
lines(x = 1:260, y = atla.64.med, type = "l", col = pal[5], 
      lwd = 2)
legend("bottomright", legend = c("15-24", "25-34", "35-44", "45-54", "55-64"), 
       col = pal, lty = 1)


## Main (Table 3)

sfm.frp.med <- apply(sfm.frp, 1, mean)/10000
atlm.frp.med <- apply(atlm.frp, 1, mean)/10000

par(mfrow = c(1,1))
plot(x = 1:260, y = sfm.frp.med, type = "l", col = alpha("red", 0.7), lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable")
lines(x = 1:260, y = atlm.frp.med, type = "l", col = alpha("blue", 0.7), 
      lwd = 2)
legend("bottomright", legend = c("San Francisco", "Atlanta"), 
       col = c("red", "blue"), lty = 1)

# SF - Race ids

sfm.b.med <- apply(sfm.frp[, sf.b], 1, mean)/10000
sfm.w.med <- apply(sfm.frp[, sf.w], 1, mean)/10000

plot(x = 1:260, y = sfm.b.med, type = "l", col = alpha("red", 0.5), lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable")
lines(x = 1:260, y = sfm.w.med, type = "l", col = alpha("blue", 0.5), 
      lwd = 2)
legend("bottomright", legend = c("Black", "White"), col = c("red", "blue"), 
       lty = 1)

# ATL - Race ids (add race to same plot, like above)

atlm.b.med <- apply(atlm.frp[, atl.b], 1, mean)/10000
atlm.w.med <- apply(atlm.frp[, atl.w], 1, mean)/10000

lines(x = 1:260, y = atlm.b.med, type = "l", col = alpha("red", 0.5), lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable")
lines(x = 1:260, y = atlm.w.med, type = "l", col = alpha("blue", 0.5), 
      lwd = 2)
legend("bottomright", legend = c("Black", "White"), col = c("red", "blue"), 
       lty = 1)


# SF - Age ids

sfm.24.med <- apply(sfm.frp[, sf.24], 1, mean)/10000
sfm.34.med <- apply(sfm.frp[, sf.34], 1, mean)/10000
sfm.44.med <- apply(sfm.frp[, sf.44], 1, mean)/10000
sfm.54.med <- apply(sfm.frp[, sf.54], 1, mean)/10000
sfm.64.med <- apply(sfm.frp[, sf.64], 1, mean)/10000

par(mfrow = c(1,2))
plot(x = 1:260, y = sfm.24.med, type = "l", col = pal[1], lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable ")
lines(x = 1:260, y = sfm.34.med, type = "l", col = pal[2], 
      lwd = 2)
lines(x = 1:260, y = sfm.44.med, type = "l", col = pal[3], 
      lwd = 2)
lines(x = 1:260, y = sfm.54.med, type = "l", col = pal[4], 
      lwd = 2)
lines(x = 1:260, y = sfm.64.med, type = "l", col = pal[5], 
      lwd = 2)
legend("topleft", legend = c("15-24", "25-34", "35-44", "45-54", "55-64"), 
       col = pal, lty = 1, cex = 0.8)


# ATL - Age ids

atlm.24.med <- apply(atlm.frp[, atl.24], 1, mean)/10000
atlm.34.med <- apply(atlm.frp[, atl.34], 1, mean)/10000
atlm.44.med <- apply(atlm.frp[, atl.44], 1, mean)/10000
atlm.54.med <- apply(atlm.frp[, atl.54], 1, mean)/10000
atlm.64.med <- apply(atlm.frp[, atl.64], 1, mean)/10000

plot(x = 1:260, y = atlm.24.med, type = "l", col = pal[1], lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable ")
lines(x = 1:260, y = atlm.34.med, type = "l", col = pal[2], 
      lwd = 2)
lines(x = 1:260, y = atlm.44.med, type = "l", col = pal[3], 
      lwd = 2)
lines(x = 1:260, y = atlm.54.med, type = "l", col = pal[4], 
      lwd = 2)
lines(x = 1:260, y = atlm.64.med, type = "l", col = pal[5], 
      lwd = 2)
legend("topleft", legend = c("15-24", "25-34", "35-44", "45-54", "55-64"), 
       col = pal, lty = 1)

# experiment with box plots
df <- t(atlm.frp[52*c(0.5, 1, 2, 4, 5), atl.24])
boxplot(df)
boxplot(df, outline = FALSE)


## Casual (Table 4)

sfc.frp.med <- apply(sfc.frp, 1, mean)/10000
atlc.frp.med <- apply(atlc.frp, 1, mean)/10000

plot(x = 1:260, y = sfc.frp.med, type = "l", col = alpha("red", 0.7), lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable")
lines(x = 1:260, y = atlc.frp.med, type = "l", col = alpha("blue", 0.7), 
      lwd = 2)
legend("topleft", legend = c("San Francisco", "Atlanta"), 
       col = c("red", "blue"), lty = 1)

atlc.frp[1:52, 1:25]
apply(atlc.frp, 1, mean)



# EA: continue conversion from here


# SF - Race ids

sfc.b.med <- apply(sfc.frp[sf.b]/10000, 1, median)
sfc.w.med <- apply(sfc.frp[sf.w]/10000, 1, median)

plot(x = 1:260, y = sfc.b.med, type = "l", col = alpha("red", 0.5), lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable")
lines(x = 1:260, y = sfc.w.med, type = "l", col = alpha("blue", 0.5), 
      lwd = 2)
legend("bottomright", legend = c("Black", "White"), col = c("red", "blue"), 
       lty = 1)

# SF - Age ids

sfc.24.med <- apply(sfc.frp[sf.24]/10000, 1, median)
sfc.34.med <- apply(sfc.frp[sf.34]/10000, 1, median)
sfc.44.med <- apply(sfc.frp[sf.44]/10000, 1, median)
sfc.54.med <- apply(sfc.frp[sf.54]/10000, 1, median)
sfc.64.med <- apply(sfc.frp[sf.64]/10000, 1, median)

plot(x = 1:260, y = sfc.24.med, type = "l", col = alpha("red", 0.7), lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable ")
lines(x = 1:260, y = sfc.34.med, type = "l", col = alpha("blue", 0.7), 
      lwd = 2)
lines(x = 1:260, y = sfc.44.med, type = "l", col = alpha("yellow", 0.7), 
      lwd = 2)
lines(x = 1:260, y = sfc.54.med, type = "l", col = alpha("green", 0.7), 
      lwd = 2)
lines(x = 1:260, y = sfc.64.med, type = "l", col = alpha("purple", 0.7), 
      lwd = 2)
legend("topleft", legend = c("15-24", "25-34", "35-44", "45-54", "55-64"), 
       col = c("red", "blue", "yellow", "green", "purple"), lty = 1)

# ATL - Race ids

atlc.b.med <- apply(atlc.frp[atl.b]/10000, 1, median)
atlc.w.med <- apply(atlc.frp[atl.w]/10000, 1, median)

plot(x = 1:260, y = atlc.b.med, type = "l", col = alpha("red", 0.5), lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable")
lines(x = 1:260, y = atlc.w.med, type = "l", col = alpha("blue", 0.5), 
      lwd = 2)
legend("bottomright", legend = c("Black", "White"), col = c("red", "blue"), 
       lty = 1)

# ATL - Age ids

atlc.24.med <- apply(atlc.frp[atl.24]/10000, 1, median)
atlc.34.med <- apply(atlc.frp[atl.34]/10000, 1, median)
atlc.44.med <- apply(atlc.frp[atl.44]/10000, 1, median)
atlc.54.med <- apply(atlc.frp[atl.54]/10000, 1, median)
atlc.64.med <- apply(atlc.frp[atl.64]/10000, 1, median)

plot(x = 1:260, y = atlc.24.med, type = "l", col = alpha("red", 0.7), lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable ")
lines(x = 1:260, y = atlc.34.med, type = "l", col = alpha("blue", 0.7), 
      lwd = 2)
lines(x = 1:260, y = atlc.44.med, type = "l", col = alpha("yellow", 0.7), 
      lwd = 2)
lines(x = 1:260, y = atlc.54.med, type = "l", col = alpha("green", 0.7), 
      lwd = 2)
lines(x = 1:260, y = atlc.64.med, type = "l", col = alpha("purple", 0.7), 
      lwd = 2)
legend("topleft", legend = c("15-24", "25-34", "35-44", "45-54", "55-64"), 
       col = c("red", "blue", "yellow", "green", "purple"), lty = 1)


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


