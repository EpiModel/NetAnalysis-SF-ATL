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

sim.sf <- readRDS("artnet.NetSim.SanFrancisco.rda")
sim.atl <- readRDS("artnet.NetSim.Atlanta.rda")

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
  x <- as.data.frame(x)
}

# All partnership types
sf.all <- load_data("sfo.all.1.rda")
atl.all <- load_data("atl.all.1.rda")

# Main
sf.main <- load_data("sfo.main.1.rda")
atl.main <- load_data("atl.main.1.rda")

# Casual
sf.casl <- load_data("sfo.casl.1.rda")
atl.casl <- load_data("atl.casl.1.rda")

# Inst
sf.inst <- load_data("sfo.inst.1.rda")
atl.inst <- load_data("atl.inst.1.rda")



# 2.Vertex IDs ------------------------------------------------------------

## Race

# Black
sf.b <- which(get.vertex.attribute(net.sfm, "race") == "0")
atl.b <- which(get.vertex.attribute(net.atlm, "race") == "0")

# White
sf.w <- which(get.vertex.attribute(net.sfm, "race") == "1")
atl.w <- which(get.vertex.attribute(net.atlm, "race") == "1")


## Age

# 15-24
sf.24 <- which(get.vertex.attribute(net.sfm, "age.grp") == "1")
atl.24 <- which(get.vertex.attribute(net.atlm, "age.grp") == "1")

# 25-34
sf.34 <- which(get.vertex.attribute(net.sfm, "age.grp") == "2")
atl.34 <- which(get.vertex.attribute(net.atlm, "age.grp") == "2")

# 35-44
sf.44 <- which(get.vertex.attribute(net.sfm, "age.grp") == "3")
atl.44 <- which(get.vertex.attribute(net.atlm, "age.grp") == "3")

# 45-54
sf.54 <- which(get.vertex.attribute(net.sfm, "age.grp") == "4")
atl.54 <- which(get.vertex.attribute(net.atlm, "age.grp") == "4")

# 55-64
sf.64 <- which(get.vertex.attribute(net.sfm, "age.grp") == "5")
atl.64 <- which(get.vertex.attribute(net.atlm, "age.grp") == "5")


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



# 3.FRP Analysis ----------------------------------------------------------


## Subset df to outcome of interest

# All partnerships
sfa.frp <- subset(sf.all, select = frp.1:frp.10000)
atla.frp <- subset(atl.all, select = frp.1:frp.10000)

# Main
sfm.frp <- subset(sf.main, select = frp.1:frp.10000)
atlm.frp <- subset(atl.main, select = frp.1:frp.10000)

# Casual
sfc.frp <- subset(sf.casl, select = frp.1:frp.10000)
atlc.frp <- subset(atl.casl, select = frp.1:frp.10000)

# Inst
sfi.frp <- subset(sf.inst, select = frp.1:frp.10000)
atli.frp <- subset(atl.inst, select = frp.1:frp.10000)


# 4.FRP Summary -----------------------------------------------------------


## SF

# All ids
sfa.sum1 <- summary(t(sfa.frp/10000))
sfm.sum1 <- summary(t(sfm.frp))
sfc.sum1 <- summary(t(sfc.frp))
sfi.sum1 <- summary(t(sfi.frp))

# Race ids
# Black
sfa.sum1b <- summary(t(sfa.frp[sf.b]/10000))
sfm.sum1b <- summary(t(sfm.frp[sf.b]))
sfc.sum1b <- summary(t(sfc.frp[sf.b]))
sfi.sum1b <- summary(t(sfi.frp[sf.b]))

# White
sfa.sum1w <- summary(t(sfa.frp[sf.w]/10000))
sfm.sum1w <- summary(t(sfm.frp[sf.w]))
sfc.sum1w <- summary(t(sfc.frp[sf.w]))
sfi.sum1w <- summary(t(sfi.frp[sf.w]))

# Age ids
# 15-24
sfa.sum124 <- summary(t(sfa.frp[sf.24]/10000))
sfm.sum124 <- summary(t(sfm.frp[sf.24]))
sfc.sum124 <- summary(t(sfc.frp[sf.24]))
sfi.sum124 <- summary(t(sfi.frp[sf.24]))

# 25-34
sfa.sum134 <- summary(t(sfa.frp[sf.34]/10000))
sfm.sum134 <- summary(t(sfm.frp[sf.34]))
sfc.sum134 <- summary(t(sfc.frp[sf.34]))
sfi.sum134 <- summary(t(sfi.frp[sf.34]))

# 35-44
sfa.sum144 <- summary(t(sfa.frp[sf.44]/10000))
sfm.sum144 <- summary(t(sfm.frp[sf.44]))
sfc.sum144 <- summary(t(sfc.frp[sf.44]))
sfi.sum144 <- summary(t(sfi.frp[sf.44]))

# 45-54
sfa.sum154 <- summary(t(sfa.frp[sf.54]/10000))
sfm.sum154 <- summary(t(sfm.frp[sf.54]))
sfc.sum154 <- summary(t(sfc.frp[sf.54]))
sfi.sum154 <- summary(t(sfi.frp[sf.54]))

# 55-64
sfa.sum164 <- summary(t(sfa.frp[sf.64]/10000))
sfm.sum164 <- summary(t(sfm.frp[sf.64]))
sfc.sum164 <- summary(t(sfc.frp[sf.64]))
sfi.sum164 <- summary(t(sfi.frp[sf.64]))


## ATL

# All ids
atla.sum1 <- summary(t(atla.frp))
atlm.sum1 <- summary(t(atlm.frp))
atlc.sum1 <- summary(t(atlc.frp))
atli.sum1 <- summary(t(atli.frp))

# Race ids
# Black
atla.sum1b <- summary(t(atla.frp[atl.b]))
atlm.sum1b <- summary(t(atlm.frp[atl.b]))
atlc.sum1b <- summary(t(atlc.frp[atl.b]))
atli.sum1b <- summary(t(atli.frp[atl.b]))

# White
atla.sum1w <- summary(t(atla.frp[atl.w]))
atlm.sum1w <- summary(t(atlm.frp[atl.w]))
atlc.sum1w <- summary(t(atlc.frp[atl.w]))
atli.sum1w <- summary(t(atli.frp[atl.w]))

# Age ids
# 15-24
atla.sum124 <- summary(t(atla.frp[atl.24]))
atlm.sum124 <- summary(t(atlm.frp[atl.24]))
atlc.sum124 <- summary(t(atlc.frp[atl.24]))
atli.sum124 <- summary(t(atli.frp[atl.24]))

# 25-34
atla.sum134 <- summary(t(atla.frp[atl.34]))
atlm.sum134 <- summary(t(atlm.frp[atl.34]))
atlc.sum134 <- summary(t(atlc.frp[atl.34]))
atli.sum134 <- summary(t(atli.frp[atl.34]))

# 35-44
atla.sum144 <- summary(t(atla.frp[atl.44]))
atlm.sum144 <- summary(t(atlm.frp[atl.44]))
atlc.sum144 <- summary(t(atlc.frp[atl.44]))
atli.sum144 <- summary(t(atli.frp[atl.44]))

# 45-54
atla.sum154 <- summary(t(atla.frp[atl.54]))
atlm.sum154 <- summary(t(atlm.frp[atl.54]))
atlc.sum154 <- summary(t(atlc.frp[atl.54]))
atli.sum154 <- summary(t(atli.frp[atl.54]))

# 55-64
atla.sum164 <- summary(t(atla.frp[atl.64]))
atlm.sum164 <- summary(t(atlm.frp[atl.64]))
atlc.sum164 <- summary(t(atlc.frp[atl.64]))
atli.sum164 <- summary(t(atli.frp[atl.64]))


# 5.FRP Median Plots ------------------------------------------------------

## All partnerships

sfa.frp.med <- apply(sfa.frp/10000, 1, median)
atla.frp.med <- apply(atla.frp/10000, 1, median)

plot(x = 1:260, y = sfa.frp.med, type = "l", col = alpha("red", 0.7), lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable")
lines(x = 1:260, y = atla.frp.med, type = "l", col = alpha("blue", 0.7), 
      lwd = 2)
legend("bottomright", legend = c("San Francisco", "Atlanta"), 
       col = c("red", "blue"), lty = 1)


## Main

sfm.frp.med <- apply(sfm.frp/10000, 1, median)
atlm.frp.med <- apply(atlm.frp/10000, 1, median)

plot(x = 1:260, y = sfm.frp.med, type = "l", col = alpha("red", 0.7), lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable")
lines(x = 1:260, y = atlm.frp.med, type = "l", col = alpha("blue", 0.7), 
      lwd = 2)
legend("bottomright", legend = c("San Francisco", "Atlanta"), 
       col = c("red", "blue"), lty = 1)


## Casual


## Inst


# END ---------------------------------------------------------------------


