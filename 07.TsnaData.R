
## 
## Outcome data for tsna analysis of San Francisco & Atlanta sexual networks
## 

## Packages ##
library("networkDynamicData")

## Set seed for analysis ##
set.seed(803)


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

## Setup output list ##
out <- list()
out$demog <- list()

## Extract vertex ids for age & race categories ##

## Race

# Black
out$demog$sf.b <- which(get.vertex.attribute(net.sfm, "race") == 0)
out$demog$atl.b <- which(get.vertex.attribute(net.atlm, "race") == 0)

# White
out$demog$sf.w <- which(get.vertex.attribute(net.sfm, "race") == 1)
out$demog$atl.w <- which(get.vertex.attribute(net.atlm, "race") == 1)


## Age

# 15-24
out$demog$sf.24 <- which(get.vertex.attribute(net.sfm, "age.grp") == 1)
out$demog$atl.24 <- which(get.vertex.attribute(net.atlm, "age.grp") == 1)

# 25-34
out$demog$sf.34 <- which(get.vertex.attribute(net.sfm, "age.grp") == 2)
out$demog$atl.34 <- which(get.vertex.attribute(net.atlm, "age.grp") == 2)

# 35-44
out$demog$sf.44 <- which(get.vertex.attribute(net.sfm, "age.grp") == 3)
out$demog$atl.44 <- which(get.vertex.attribute(net.atlm, "age.grp") == 3)

# 45-54
out$demog$sf.54 <- which(get.vertex.attribute(net.sfm, "age.grp") == 4)
out$demog$atl.54 <- which(get.vertex.attribute(net.atlm, "age.grp") == 4)

# 55-64
out$demog$sf.64 <- which(get.vertex.attribute(net.sfm, "age.grp") == 5)
out$demog$atl.64 <- which(get.vertex.attribute(net.atlm, "age.grp") == 5)


# 4. Extract FRP ----------------------------------------------------------

## Setup output list ##
out$frp <- list()

## Subset outcome data frame by FRP ##

# All partnerships
out$frp$sfa.frp <- sf.all$frp
out$frp$atla.frp <- atl.all$frp

# Main
out$frp$sfm.frp <- sf.main$frp
out$frp$atlm.frp <- atl.main$frp

# Casual
out$frp$sfc.frp <- sf.casl$frp
out$frp$atlc.frp <- atl.casl$frp

# Inst
out$frp$sfi.frp <- sf.inst$frp
out$frp$atli.frp <- atl.inst$frp


# Save out file -----------------------------------------------------------

fn <- "data/artnet.TsnaData.rda"
saveRDS(out, file = fn)


# END ---------------------------------------------------------------------














