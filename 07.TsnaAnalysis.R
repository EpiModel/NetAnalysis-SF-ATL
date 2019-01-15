## TSNA Analysis
## 9/23/2018

library("tsna")
library("networkDynamicData")
suppressMessages(library("EpiModel"))
# library("sna")
# library("ggplot2")

set.seed(803)


# 0. Netowrk data ---------------------------------------------------------

### Read in sims ###

sim.sf <- readRDS("artnet.NetSim.SanFrancisco.rda")
sim.atl <- readRDS("artnet.NetSim.Atlanta.rda")

# Extract each partner type network

## SF

sf.m <- sim.sf[[1]] # main
sf.c <- sim.sf[[2]] # casual
sf.i <- sim.sf[[3]] # inst

## ATL

atl.m <- sim.atl[[1]] # main
atl.c <- sim.atl[[2]] # casual
atl.i <- sim.atl[[3]] # inst


# 1. Outcome Data ---------------------------------------------------------


### ALL ptypes ###

# sf.all <- load(file = "sfo.all.1.rda")
# sf.all <- out
# sf.all <- as.data.frame(sf.all)
# 
# atl.all <- load(file = "atl.all.1.rda")
# atl.all <- out
# atl.all <- as.data.frame(atl.all)


### Main ###

## SF
# Load in data
sf.main <- load(file = "sfo.main.1.rda")
sf.main <- out
sf.main <- as.data.frame(sf.main)

# Subset by outcome
sf.m.frp <- subset(sf.main, select = frp.1:frp.10000)
sf.m.tdist <- subset(sf.main, select = medtdist.1:medtdist.10000)
sf.m.gstep <- subset(sf.main, select = medgeod.1:medgeod.10000)
sf.m.deg <- subset(sf.main, select = degree.1:degree.10000)
sf.m.cumldeg <- subset(sf.main, select = cumldegree.1:cumldegree.10000)
sf.m.bcent <- subset(sf.main, select = bcent.1:bcent.10000)

write.csv(sf.m.tdist, file = "sfm_tdist.csv")

## ATL
# Load in data
atl.main <- load(file = "atl.main.1.rda")
atl.main <- out
atl.main <- as.data.frame(atl.main)

# Subset by outcome
atl.m.frp <- subset(atl.main, select = frp.1:frp.10000)
atl.m.tdist <- subset(atl.main, select = medtdist.1:medtdist.10000)
atl.m.gstep <- subset(atl.main, select = medgeod.1:medgeod.10000)
atl.m.deg <- subset(atl.main, select = degree.1:degree.10000)
atl.m.cumldeg <- subset(atl.main, select = cumldegree.1:cumldegree.10000)
atl.m.bcent <- subset(atl.main, select = bcent.1:bcent.10000)


### CASUAL ###
 
## SF
# Load in data
sf.casl <- load(file = "sfo.casl.1.rda")
sf.casl <- out
sf.casl <- as.data.frame(sf.casl)

# Subset by outcome
sf.c.frp <- subset(sf.casl, select = frp.1:frp.10000)
sf.c.tdist <- subset(sf.casl, select = medtdist.1:medtdist.10000)
sf.c.gstep <- subset(sf.casl, select = medgeod.1:medgeod.10000)
sf.c.deg <- subset(sf.casl, select = degree.1:degree.10000)
sf.c.cumldeg <- subset(sf.casl, select = cumldegree.1:cumldegree.10000)
sf.c.bcent <- subset(sf.casl, select = bcent.1:bcent.10000)

## ATL
# Load in data
atl.casl <- load(file = "atl.casl.1.rda")
atl.casl <- out
atl.casl <- as.data.frame(atl.casl)

# Subset by outcome
atl.c.frp <- subset(atl.casl, select = frp.1:frp.10000)
atl.c.tdist <- subset(atl.casl, select = medtdist.1:medtdist.10000)
atl.c.gstep <- subset(atl.casl, select = medgeod.1:medgeod.10000)
atl.c.deg <- subset(atl.casl, select = degree.1:degree.10000)
atl.c.cumldeg <- subset(atl.casl, select = cumldegree.1:cumldegree.10000)
atl.c.bcent <- subset(atl.casl, select = bcent.1:bcent.10000)


### INST ###

## SF
# Load in data
sf.inst <- load(file = "sfo.inst.1.rda")
sf.inst <- out
sf.inst <- as.data.frame(sf.inst)

# Subset by outcome
sf.i.frp <- subset(sf.inst, select = frp.1:frp.10000)
sf.i.tdist <- subset(sf.inst, select = medtdist.1:medtdist.10000)
sf.i.gstep <- subset(sf.inst, select = medgeod.1:medgeod.10000)
sf.i.deg <- subset(sf.inst, select = degree.1:degree.10000)
sf.i.cumldeg <- subset(sf.inst, select = cumldegree.1:cumldegree.10000)
sf.i.bcent <- subset(sf.inst, select = bcent.1:bcent.10000)

## ATL
# Load in data
atl.inst <- load(file = "atl.inst.1.rda")
atl.inst <- out
atl.inst <- as.data.frame(atl.inst)

# Subset by outcome
atl.i.frp <- subset(atl.inst, select = frp.1:frp.10000)
atl.i.tdist <- subset(atl.inst, select = medtdist.1:medtdist.10000)
atl.i.gstep <- subset(atl.inst, select = medgeod.1:medgeod.10000)
atl.i.deg <- subset(atl.inst, select = degree.1:degree.10000)
atl.i.cumldeg <- subset(atl.inst, select = cumldegree.1:cumldegree.10000)
atl.i.bcent <- subset(atl.inst, select = bcent.1:bcent.10000)

# df <- as.data.frame(names(sf.main))


# 2. Validation -----------------------------------------------------------

## Checking degree in simulations
EpiModel::get_degree(network.collapse(sf.m, at = 1))
EpiModel::get_degree(network.collapse(sf.c, at = 1))
EpiModel::get_degree(network.collapse(sf.i, at = 1))
EpiModel::get_degree(network.collapse(atl.m, at = 1))
EpiModel::get_degree(network.collapse(atl.c, at = 1))
EpiModel::get_degree(network.collapse(atl.i, at = 1))

## Checking degree in outcome data

# Main
test <- subset(sf.main, select = degree.1:degree.10000)
test <- subset(atl.main, select = degree.1:degree.10000)

# Casual
test <- subset(sf.casl, select = degree.1:degree.10000)
test <- subset(atl.casl, select = degree.1:degree.10000)

# Instantaneous
test <- subset(sf.inst, select = degree.1:degree.10000)
test <- subset(atl.inst, select = degree.1:degree.10000)


# 3a. Extract IDs: Race --------------------------------------------------


### Black ###

## SF
sf.id.b <- which(get.vertex.attribute(sf.m, "race") == "B")

# Main
sfm.frp.b <- sf.m.frp[sf.id.b]
sfm.tdist.b <- sf.m.tdist[sf.id.b]
sfm.gstep.b <- sf.m.gstep[sf.id.b]
sfm.deg.b <- sf.m.deg[sf.id.b]
sfm.cumldeg.b <- sf.m.cumldeg[sf.id.b]
sfm.bcent.b <- sf.m.bcent[sf.id.b]

# Casual
sfc.frp.b <- sf.c.frp[sf.id.b]
sfc.tdist.b <- sf.c.tdist[sf.id.b]
sfc.gstep.b <- sf.c.gstep[sf.id.b]
sfc.deg.b <- sf.c.deg[sf.id.b]
sfc.cumldeg.b <- sf.c.cumldeg[sf.id.b]
sfc.bcent.b <- sf.c.bcent[sf.id.b]

# Inst
sfi.frp.b <- sf.i.frp[sf.id.b]
sfi.tdist.b <- sf.i.tdist[sf.id.b]
sfi.gstep.b <- sf.i.gstep[sf.id.b]
sfi.deg.b <- sf.i.deg[sf.id.b]
sfi.cumldeg.b <- sf.i.cumldeg[sf.id.b]
sfi.bcent.b <- sf.i.bcent[sf.id.b]


## ATL
atl.id.b <- which(get.vertex.attribute(atl.m, "race") == "B")

# Main
atlm.frp.b <- atl.m.frp[atl.id.b]
atlm.tdist.b <- atl.m.tdist[atl.id.b]
atlm.gstep.b <- atl.m.gstep[atl.id.b]
atlm.deg.b <- atl.m.deg[atl.id.b]
atlm.cumldeg.b <- atl.m.cumldeg[atl.id.b]
atlm.bcent.b <- atl.m.bcent[atl.id.b]

# Casual
atlc.frp.b <- atl.c.frp[atl.id.b]
atlc.tdist.b <- atl.c.tdist[atl.id.b]
atlc.gstep.b <- atl.c.gstep[atl.id.b]
atlc.deg.b <- atl.c.deg[atl.id.b]
atlc.cumldeg.b <- atl.c.cumldeg[atl.id.b]
atlc.bcent.b <- atl.c.bcent[atl.id.b]

# Inst
atli.frp.b <- atl.i.frp[atl.id.b]
atli.tdist.b <- atl.i.tdist[atl.id.b]
atli.gstep.b <- atl.i.gstep[atl.id.b]
atli.deg.b <- atl.i.deg[atl.id.b]
atli.cumldeg.b <- atl.i.cumldeg[atl.id.b]
atli.bcent.b <- atl.i.bcent[atl.id.b]


### White ###

## SF
sf.id.w <- which(get.vertex.attribute(sf.m, "race") == "W")

# Main
sfm.frp.w <- sf.m.frp[sf.id.w]
sfm.tdist.w <- sf.m.tdist[sf.id.w]
sfm.gstep.w <- sf.m.gstep[sf.id.w]
sfm.deg.w <- sf.m.deg[sf.id.w]
sfm.cumldeg.w <- sf.m.cumldeg[sf.id.w]
sfm.bcent.w <- sf.m.bcent[sf.id.w]

# Casual
sfc.frp.w <- sf.c.frp[sf.id.w]
sfc.tdist.w <- sf.c.tdist[sf.id.w]
sfc.gstep.w <- sf.c.gstep[sf.id.w]
sfc.deg.w <- sf.c.deg[sf.id.w]
sfc.cumldeg.w <- sf.c.cumldeg[sf.id.w]
sfc.bcent.w <- sf.c.bcent[sf.id.w]

# Inst
sfi.frp.w <- sf.i.frp[sf.id.w]
sfi.tdist.w <- sf.i.tdist[sf.id.w]
sfi.gstep.w <- sf.i.gstep[sf.id.w]
sfi.deg.w <- sf.i.deg[sf.id.w]
sfi.cumldeg.w <- sf.i.cumldeg[sf.id.w]
sfi.bcent.w <- sf.i.bcent[sf.id.w]


## ATL
atl.id.w <- which(get.vertex.attribute(atl.m, "race") == "W")

# Main
atlm.frp.w <- atl.m.frp[atl.id.w]
atlm.tdist.w <- atl.m.tdist[atl.id.w]
atlm.gstep.w <- atl.m.gstep[atl.id.w]
atlm.deg.w <- atl.m.deg[atl.id.w]
atlm.cumldeg.w <- atl.m.cumldeg[atl.id.w]
atlm.bcent.w <- atl.m.bcent[atl.id.w]

# Casual
atlc.frp.w <- atl.c.frp[atl.id.w]
atlc.tdist.w <- atl.c.tdist[atl.id.w]
atlc.gstep.w <- atl.c.gstep[atl.id.w]
atlc.deg.w <- atl.c.deg[atl.id.w]
atlc.cumldeg.w <- atl.c.cumldeg[atl.id.w]
atlc.bcent.w <- atl.c.bcent[atl.id.w]

# Inst
atli.frp.w <- atl.i.frp[atl.id.w]
atli.tdist.w <- atl.i.tdist[atl.id.w]
atli.gstep.w <- atl.i.gstep[atl.id.w]
atli.deg.w <- atl.i.deg[atl.id.w]
atli.cumldeg.w <- atl.i.cumldeg[atl.id.w]
atli.bcent.w <- atl.i.bcent[atl.id.w]



# 3b. Extract IDs: Age ----------------------------------------------------


## Age 0-24

## SF
sf.id.24 <- which(get.vertex.attribute(sf.m, "age.grp") == "1")

# Main
sfm.frp.24 <- sf.m.frp[sf.id.24]
sfm.tdist.24 <- sf.m.tdist[sf.id.24]
sfm.gstep.24 <- sf.m.gstep[sf.id.24]
sfm.deg.24 <- sf.m.deg[sf.id.24]
sfm.cumldeg.24 <- sf.m.cumldeg[sf.id.24]
sfm.bcent.24 <- sf.m.bcent[sf.id.24]

# Casual
sfc.frp.24 <- sf.c.frp[sf.id.24]
sfc.tdist.24 <- sf.c.tdist[sf.id.24]
sfc.gstep.24 <- sf.c.gstep[sf.id.24]
sfc.deg.24 <- sf.c.deg[sf.id.24]
sfc.cumldeg.24 <- sf.c.cumldeg[sf.id.24]
sfc.bcent.24 <- sf.c.bcent[sf.id.24]

# Inst
sfi.frp.24 <- sf.i.frp[sf.id.24]
sfi.tdist.24 <- sf.i.tdist[sf.id.24]
sfi.gstep.24 <- sf.i.gstep[sf.id.24]
sfi.deg.24 <- sf.i.deg[sf.id.24]
sfi.cumldeg.24 <- sf.i.cumldeg[sf.id.24]
sfi.bcent.24 <- sf.i.bcent[sf.id.24]


## ATL
atl.id.24 <- which(get.vertex.attribute(atl.m, "age.grp") == "1")

# Main
atlm.frp.24 <- atl.m.frp[atl.id.24]
atlm.tdist.24 <- atl.m.tdist[atl.id.24]
atlm.gstep.24 <- atl.m.gstep[atl.id.24]
atlm.deg.24 <- atl.m.deg[atl.id.24]
atlm.cumldeg.24 <- atl.m.cumldeg[atl.id.24]
atlm.bcent.24 <- atl.m.bcent[atl.id.24]

# Casual
atlc.frp.24 <- atl.c.frp[atl.id.24]
atlc.tdist.24 <- atl.c.tdist[atl.id.24]
atlc.gstep.24 <- atl.c.gstep[atl.id.24]
atlc.deg.24 <- atl.c.deg[atl.id.24]
atlc.cumldeg.24 <- atl.c.cumldeg[atl.id.24]
atlc.bcent.24 <- atl.c.bcent[atl.id.24]

# Inst
atli.frp.24 <- atl.i.frp[atl.id.24]
atli.tdist.24 <- atl.i.tdist[atl.id.24]
atli.gstep.24 <- atl.i.gstep[atl.id.24]
atli.deg.24 <- atl.i.deg[atl.id.24]
atli.cumldeg.24 <- atl.i.cumldeg[atl.id.24]
atli.bcent.24 <- atl.i.bcent[atl.id.24]


## Age 25-34

## SF
sf.id.34 <- which(get.vertex.attribute(sf.m, "age.grp") == "2")

# Main
sfm.frp.34 <- sf.m.frp[sf.id.34]
sfm.tdist.34 <- sf.m.tdist[sf.id.34]
sfm.gstep.34 <- sf.m.gstep[sf.id.34]
sfm.deg.34 <- sf.m.deg[sf.id.34]
sfm.cumldeg.34 <- sf.m.cumldeg[sf.id.34]
sfm.bcent.34 <- sf.m.bcent[sf.id.34]

# Casual
sfc.frp.34 <- sf.c.frp[sf.id.34]
sfc.tdist.34 <- sf.c.tdist[sf.id.34]
sfc.gstep.34 <- sf.c.gstep[sf.id.34]
sfc.deg.34 <- sf.c.deg[sf.id.34]
sfc.cumldeg.34 <- sf.c.cumldeg[sf.id.34]
sfc.bcent.34 <- sf.c.bcent[sf.id.34]

# Inst
sfi.frp.34 <- sf.i.frp[sf.id.34]
sfi.tdist.34 <- sf.i.tdist[sf.id.34]
sfi.gstep.34 <- sf.i.gstep[sf.id.34]
sfi.deg.34 <- sf.i.deg[sf.id.34]
sfi.cumldeg.34 <- sf.i.cumldeg[sf.id.34]
sfi.bcent.34 <- sf.i.bcent[sf.id.34]


## ATL
atl.id.34 <- which(get.vertex.attribute(atl.m, "age.grp") == "2")

# Main
atlm.frp.34 <- atl.m.frp[atl.id.34]
atlm.tdist.34 <- atl.m.tdist[atl.id.34]
atlm.gstep.34 <- atl.m.gstep[atl.id.34]
atlm.deg.34 <- atl.m.deg[atl.id.34]
atlm.cumldeg.34 <- atl.m.cumldeg[atl.id.34]
atlm.bcent.34 <- atl.m.bcent[atl.id.34]

# Casual
atlc.frp.34 <- atl.c.frp[atl.id.34]
atlc.tdist.34 <- atl.c.tdist[atl.id.34]
atlc.gstep.34 <- atl.c.gstep[atl.id.34]
atlc.deg.34 <- atl.c.deg[atl.id.34]
atlc.cumldeg.34 <- atl.c.cumldeg[atl.id.34]
atlc.bcent.34 <- atl.c.bcent[atl.id.34]

# Inst
atli.frp.34 <- atl.i.frp[atl.id.34]
atli.tdist.34 <- atl.i.tdist[atl.id.34]
atli.gstep.34 <- atl.i.gstep[atl.id.34]
atli.deg.34 <- atl.i.deg[atl.id.34]
atli.cumldeg.34 <- atl.i.cumldeg[atl.id.34]
atli.bcent.34 <- atl.i.bcent[atl.id.34]


## Age 35-44

## SF
sf.id.44 <- which(get.vertex.attribute(sf.m, "age.grp") == "3")

# Main
sfm.frp.44 <- sf.m.frp[sf.id.44]
sfm.tdist.44 <- sf.m.tdist[sf.id.44]
sfm.gstep.44 <- sf.m.gstep[sf.id.44]
sfm.deg.44 <- sf.m.deg[sf.id.44]
sfm.cumldeg.44 <- sf.m.cumldeg[sf.id.44]
sfm.bcent.44 <- sf.m.bcent[sf.id.44]

# Casual
sfc.frp.44 <- sf.c.frp[sf.id.44]
sfc.tdist.44 <- sf.c.tdist[sf.id.44]
sfc.gstep.44 <- sf.c.gstep[sf.id.44]
sfc.deg.44 <- sf.c.deg[sf.id.44]
sfc.cumldeg.44 <- sf.c.cumldeg[sf.id.44]
sfc.bcent.44 <- sf.c.bcent[sf.id.44]

# Inst
sfi.frp.44 <- sf.i.frp[sf.id.44]
sfi.tdist.44 <- sf.i.tdist[sf.id.44]
sfi.gstep.44 <- sf.i.gstep[sf.id.44]
sfi.deg.44 <- sf.i.deg[sf.id.44]
sfi.cumldeg.44 <- sf.i.cumldeg[sf.id.44]
sfi.bcent.44 <- sf.i.bcent[sf.id.44]


## ATL
atl.id.44 <- which(get.vertex.attribute(atl.m, "age.grp") == "3")

# Main
atlm.frp.44 <- atl.m.frp[atl.id.44]
atlm.tdist.44 <- atl.m.tdist[atl.id.44]
atlm.gstep.44 <- atl.m.gstep[atl.id.44]
atlm.deg.44 <- atl.m.deg[atl.id.44]
atlm.cumldeg.44 <- atl.m.cumldeg[atl.id.44]
atlm.bcent.44 <- atl.m.bcent[atl.id.44]

# Casual
atlc.frp.44 <- atl.c.frp[atl.id.44]
atlc.tdist.44 <- atl.c.tdist[atl.id.44]
atlc.gstep.44 <- atl.c.gstep[atl.id.44]
atlc.deg.44 <- atl.c.deg[atl.id.44]
atlc.cumldeg.44 <- atl.c.cumldeg[atl.id.44]
atlc.bcent.44 <- atl.c.bcent[atl.id.44]

# Inst
atli.frp.44 <- atl.c.frp[atl.id.44]
atli.tdist.44 <- atl.c.tdist[atl.id.44]
atli.gstep.44 <- atl.c.gstep[atl.id.44]
atli.deg.44 <- atl.c.deg[atl.id.44]
atli.cumldeg.44 <- atl.c.cumldeg[atl.id.44]
atli.bcent.44 <- atl.c.bcent[atl.id.44]


## Age 45-54

## SF
sf.id.54 <- which(get.vertex.attribute(sf.m, "age.grp") == "4")

# Main
sfm.frp.54 <- sf.m.frp[sf.id.54]
sfm.tdist.54 <- sf.m.tdist[sf.id.54]
sfm.gstep.54 <- sf.m.gstep[sf.id.54]
sfm.deg.54 <- sf.m.deg[sf.id.54]
sfm.cumldeg.54 <- sf.m.cumldeg[sf.id.54]
sfm.bcent.54 <- sf.m.bcent[sf.id.54]

# Casual
sfc.frp.54 <- sf.c.frp[sf.id.54]
sfc.tdist.54 <- sf.c.tdist[sf.id.54]
sfc.gstep.54 <- sf.c.gstep[sf.id.54]
sfc.deg.54 <- sf.c.deg[sf.id.54]
sfc.cumldeg.54 <- sf.c.cumldeg[sf.id.54]
sfc.bcent.54 <- sf.c.bcent[sf.id.54]

# Inst
sfi.frp.54 <- sf.i.frp[sf.id.54]
sfi.tdist.54 <- sf.i.tdist[sf.id.54]
sfi.gstep.54 <- sf.i.gstep[sf.id.54]
sfi.deg.54 <- sf.i.deg[sf.id.54]
sfi.cumldeg.54 <- sf.i.cumldeg[sf.id.54]
sfi.bcent.54 <- sf.i.bcent[sf.id.54]


## ATL
atl.id.54 <- which(get.vertex.attribute(atl.m, "age.grp") == "4")

# Main
atlm.frp.54 <- atl.m.frp[atl.id.54]
atlm.tdist.54 <- atl.m.tdist[atl.id.54]
atlm.gstep.54 <- atl.m.gstep[atl.id.54]
atlm.deg.54 <- atl.m.deg[atl.id.54]
atlm.cumldeg.54 <- atl.m.cumldeg[atl.id.54]
atlm.bcent.54 <- atl.m.bcent[atl.id.54]

# Casual
atlc.frp.54 <- atl.c.frp[atl.id.54]
atlc.tdist.54 <- atl.c.tdist[atl.id.54]
atlc.gstep.54 <- atl.c.gstep[atl.id.54]
atlc.deg.54 <- atl.c.deg[atl.id.54]
atlc.cumldeg.54 <- atl.c.cumldeg[atl.id.54]
atlc.bcent.54 <- atl.c.bcent[atl.id.54]

# Inst
atli.frp.54 <- atl.i.frp[atl.id.54]
atli.tdist.54 <- atl.i.tdist[atl.id.54]
atli.gstep.54 <- atl.i.gstep[atl.id.54]
atli.deg.54 <- atl.i.deg[atl.id.54]
atli.cumldeg.54 <- atl.i.cumldeg[atl.id.54]
atli.bcent.54 <- atl.i.bcent[atl.id.54]


## Age 55+

## SF
sf.id.64 <- which(get.vertex.attribute(sf.m, "age.grp") == "5")

# Main
sfm.frp.64 <- sf.m.frp[sf.id.64]
sfm.tdist.64 <- sf.m.tdist[sf.id.64]
sfm.gstep.64 <- sf.m.gstep[sf.id.64]
sfm.deg.64 <- sf.m.deg[sf.id.64]
sfm.cumldeg.64 <- sf.m.cumldeg[sf.id.64]
sfm.bcent.64 <- sf.m.bcent[sf.id.64]

# Casual
sfc.frp.64 <- sf.c.frp[sf.id.64]
sfc.tdist.64 <- sf.c.tdist[sf.id.64]
sfc.gstep.64 <- sf.c.gstep[sf.id.64]
sfc.deg.64 <- sf.c.deg[sf.id.64]
sfc.cumldeg.64 <- sf.c.cumldeg[sf.id.64]
sfc.bcent.64 <- sf.c.bcent[sf.id.64]

# Inst
sfi.frp.64 <- sf.i.frp[sf.id.64]
sfi.tdist.64 <- sf.i.tdist[sf.id.64]
sfi.gstep.64 <- sf.i.gstep[sf.id.64]
sfi.deg.64 <- sf.i.deg[sf.id.64]
sfi.cumldeg.64 <- sf.i.cumldeg[sf.id.64]
sfi.bcent.64 <- sf.i.bcent[sf.id.64]


## ATL
atl.id.64 <- which(get.vertex.attribute(atl.m, "age.grp") == "5")

# Main
atlm.frp.64 <- atl.m.frp[atl.id.64]
atlm.tdist.64 <- atl.m.tdist[atl.id.64]
atlm.gstep.64 <- atl.m.gstep[atl.id.64]
atlm.deg.64 <- atl.m.deg[atl.id.64]
atlm.cumldeg.64 <- atl.m.cumldeg[atl.id.64]
atlm.bcent.64 <- atl.m.bcent[atl.id.64]

# Casual
atlc.frp.64 <- atl.c.frp[atl.id.64]
atlc.tdist.64 <- atl.c.tdist[atl.id.64]
atlc.gstep.64 <- atl.c.gstep[atl.id.64]
atlc.deg.64 <- atl.c.deg[atl.id.64]
atlc.cumldeg.64 <- atl.c.cumldeg[atl.id.64]
atlc.bcent.64 <- atl.c.bcent[atl.id.64]

# Inst
atli.frp.64 <- atl.i.frp[atl.id.64]
atli.tdist.64 <- atl.i.tdist[atl.id.64]
atli.gstep.64 <- atl.i.gstep[atl.id.64]
atli.deg.64 <- atl.i.deg[atl.id.64]
atli.cumldeg.64 <- atl.i.cumldeg[atl.id.64]
atli.bcent.64 <- atl.i.bcent[atl.id.64]


# 3c. Extract IDs: Race & Age ---------------------------------------------

## Black 0-24

## SF
sf.id.b24 <- which(get.vertex.attribute(sf.m, "race") == "B" & 
                     get.vertex.attribute(sf.m, "age.grp") == "1")

# Main
sfm.frp.b24 <- sf.m.frp[sf.id.b24]
sfm.tdist.b24 <- sf.m.tdist[sf.id.b24]
sfm.gstep.b24 <- sf.m.gstep[sf.id.b24]
sfm.deg.b24 <- sf.m.deg[sf.id.b24]
sfm.cumldeg.b24 <- sf.m.cumldeg[sf.id.b24]
sfm.bcent.b24 <- sf.m.bcent[sf.id.b24]

# Casual
sfc.frp.b24 <- sf.c.frp[sf.id.b24]
sfc.tdist.b24 <- sf.c.tdist[sf.id.b24]
sfc.gstep.b24 <- sf.c.gstep[sf.id.b24]
sfc.deg.b24 <- sf.c.deg[sf.id.b24]
sfc.cumldeg.b24 <- sf.c.cumldeg[sf.id.b24]
sfc.bcent.b24 <- sf.c.bcent[sf.id.b24]

# Inst
sfi.frp.b24 <- sf.i.frp[sf.id.b24]
sfi.tdist.b24 <- sf.i.tdist[sf.id.b24]
sfi.gstep.b24 <- sf.i.gstep[sf.id.b24]
sfi.deg.b24 <- sf.i.deg[sf.id.b24]
sfi.cumldeg.b24 <- sf.i.cumldeg[sf.id.b24]
sfi.bcent.b24 <- sf.i.bcent[sf.id.b24]


## ATL
atl.id.b24 <- which(get.vertex.attribute(atl.m, "race") == "B" & 
                      get.vertex.attribute(atl.m, "age.grp") == "1")

# Main
atlm.frp.b24 <- atl.m.frp[atl.id.b24]
atlm.tdist.b24 <- atl.m.tdist[atl.id.b24]
atlm.gstep.b24 <- atl.m.gstep[atl.id.b24]
atlm.deg.b24 <- atl.m.deg[atl.id.b24]
atlm.cumldeg.b24 <- atl.m.cumldeg[atl.id.b24]
atlm.bcent.b24 <- atl.m.bcent[atl.id.b24]

# Casual
atlc.frp.b24 <- atl.c.frp[atl.id.b24]
atlc.tdist.b24 <- atl.c.tdist[atl.id.b24]
atlc.gstep.b24 <- atl.c.gstep[atl.id.b24]
atlc.deg.b24 <- atl.c.deg[atl.id.b24]
atlc.cumldeg.b24 <- atl.c.cumldeg[atl.id.b24]
atlc.bcent.b24 <- atl.c.bcent[atl.id.b24]

# Inst
atli.frp.b24 <- atl.i.frp[atl.id.b24]
atli.tdist.b24 <- atl.i.tdist[atl.id.b24]
atli.gstep.b24 <- atl.i.gstep[atl.id.b24]
atli.deg.b24 <- atl.i.deg[atl.id.b24]
atli.cumldeg.b24 <- atl.i.cumldeg[atl.id.b24]
atli.bcent.b24 <- atl.i.bcent[atl.id.b24]


## White 0-24

## SF
sf.id.w24 <- which(get.vertex.attribute(sf.m, "race") == "W" & 
                     get.vertex.attribute(sf.m, "age.grp") == "1")

# Main
sfm.frp.w24 <- sf.m.frp[sf.id.w24]
sfm.tdist.w24 <- sf.m.tdist[sf.id.w24]
sfm.gstep.w24 <- sf.m.gstep[sf.id.w24]
sfm.deg.w24 <- sf.m.deg[sf.id.w24]
sfm.cumldeg.w24 <- sf.m.cumldeg[sf.id.w24]
sfm.bcent.w24 <- sf.m.bcent[sf.id.w24]

# Casual
sfc.frp.w24 <- sf.c.frp[sf.id.w24]
sfc.tdist.w24 <- sf.c.tdist[sf.id.w24]
sfc.gstep.w24 <- sf.c.gstep[sf.id.w24]
sfc.deg.w24 <- sf.c.deg[sf.id.w24]
sfc.cumldeg.w24 <- sf.c.cumldeg[sf.id.w24]
sfc.bcent.w24 <- sf.c.bcent[sf.id.w24]

# Inst
sfi.frp.w24 <- sf.i.frp[sf.id.w24]
sfi.tdist.w24 <- sf.i.tdist[sf.id.w24]
sfi.gstep.w24 <- sf.i.gstep[sf.id.w24]
sfi.deg.w24 <- sf.i.deg[sf.id.w24]
sfi.cumldeg.w24 <- sf.i.cumldeg[sf.id.w24]
sfi.bcent.w24 <- sf.i.bcent[sf.id.w24]


## ATL
atl.id.w24 <- which(get.vertex.attribute(atl.m, "race") == "W" & 
                      get.vertex.attribute(atl.m, "age.grp") == "1")

# Main
atlm.frp.w24 <- atl.m.frp[atl.id.w24]
atlm.tdist.w24 <- atl.m.tdist[atl.id.w24]
atlm.gstep.w24 <- atl.m.gstep[atl.id.w24]
atlm.deg.w24 <- atl.m.deg[atl.id.w24]
atlm.cumldeg.w24 <- atl.m.cumldeg[atl.id.w24]
atlm.bcent.w24 <- atl.m.bcent[atl.id.w24]

# Casual
atlc.frp.w24 <- atl.c.frp[atl.id.w24]
atlc.tdist.w24 <- atl.c.tdist[atl.id.w24]
atlc.gstep.w24 <- atl.c.gstep[atl.id.w24]
atlc.deg.w24 <- atl.c.deg[atl.id.w24]
atlc.cumldeg.w24 <- atl.c.cumldeg[atl.id.w24]
atlc.bcent.w24 <- atl.c.bcent[atl.id.w24]

# Inst
atli.frp.w24 <- atl.i.frp[atl.id.w24]
atli.tdist.w24 <- atl.i.tdist[atl.id.w24]
atli.gstep.w24 <- atl.i.gstep[atl.id.w24]
atli.deg.w24 <- atl.i.deg[atl.id.w24]
atli.cumldeg.w24 <- atl.i.cumldeg[atl.id.w24]
atli.bcent.w24 <- atl.i.bcent[atl.id.w24]


## Black 25-34

## SF
sf.id.b34 <- which(get.vertex.attribute(sf.m, "race") == "B" & 
                     get.vertex.attribute(sf.m, "age.grp") == "2")

# Main
sfm.frp.b34 <- sf.m.frp[sf.id.b34]
sfm.tdist.b34 <- sf.m.tdist[sf.id.b34]
sfm.gstep.b34 <- sf.m.gstep[sf.id.b34]
sfm.deg.b34 <- sf.m.deg[sf.id.b34]
sfm.cumldeg.b34 <- sf.m.cumldeg[sf.id.b34]
sfm.bcent.b34 <- sf.m.bcent[sf.id.b34]

# Casual
sfc.frp.b34 <- sf.c.frp[sf.id.b34]
sfc.tdist.b34 <- sf.c.tdist[sf.id.b34]
sfc.gstep.b34 <- sf.c.gstep[sf.id.b34]
sfc.deg.b34 <- sf.c.deg[sf.id.b34]
sfc.cumldeg.b34 <- sf.c.cumldeg[sf.id.b34]
sfc.bcent.b34 <- sf.c.bcent[sf.id.b34]

# Inst
sfi.frp.b34 <- sf.i.frp[sf.id.b34]
sfi.tdist.b34 <- sf.i.tdist[sf.id.b34]
sfi.gstep.b34 <- sf.i.gstep[sf.id.b34]
sfi.deg.b34 <- sf.i.deg[sf.id.b34]
sfi.cumldeg.b34 <- sf.i.cumldeg[sf.id.b34]
sfi.bcent.b34 <- sf.i.bcent[sf.id.b34]


## ATL
atl.id.b34 <- which(get.vertex.attribute(atl.m, "race") == "B" & 
                      get.vertex.attribute(atl.m, "age.grp") == "2")

# Main
atlm.frp.b34 <- atl.m.frp[atl.id.b34]
atlm.tdist.b34 <- atl.m.tdist[atl.id.b34]
atlm.gstep.b34 <- atl.m.gstep[atl.id.b34]
atlm.deg.b34 <- atl.m.deg[atl.id.b34]
atlm.cumldeg.b34 <- atl.m.cumldeg[atl.id.b34]
atlm.bcent.b34 <- atl.m.bcent[atl.id.b34]

# Casual
atlc.frp.b34 <- atl.c.frp[atl.id.b34]
atlc.tdist.b34 <- atl.c.tdist[atl.id.b34]
atlc.gstep.b34 <- atl.c.gstep[atl.id.b34]
atlc.deg.b34 <- atl.c.deg[atl.id.b34]
atlc.cumldeg.b34 <- atl.c.cumldeg[atl.id.b34]
atlc.bcent.b34 <- atl.c.bcent[atl.id.b34]

# Inst
atli.frp.b34 <- atl.i.frp[atl.id.b34]
atli.tdist.b34 <- atl.i.tdist[atl.id.b34]
atli.gstep.b34 <- atl.i.gstep[atl.id.b34]
atli.deg.b34 <- atl.i.deg[atl.id.b34]
atli.cumldeg.b34 <- atl.i.cumldeg[atl.id.b34]
atli.bcent.b34 <- atl.i.bcent[atl.id.b34]


## White 25-34

## SF
sf.id.w34 <- which(get.vertex.attribute(sf.m, "race") == "W" & 
                     get.vertex.attribute(sf.m, "age.grp") == "2")

# Main
sfm.frp.w34 <- sf.m.frp[sf.id.w34]
sfm.tdist.w34 <- sf.m.tdist[sf.id.w34]
sfm.gstep.w34 <- sf.m.gstep[sf.id.w34]
sfm.deg.w34 <- sf.m.deg[sf.id.w34]
sfm.cumldeg.w34 <- sf.m.cumldeg[sf.id.w34]
sfm.bcent.w34 <- sf.m.bcent[sf.id.w34]

# Casual
sfc.frp.w34 <- sf.c.frp[sf.id.w34]
sfc.tdist.w34 <- sf.c.tdist[sf.id.w34]
sfc.gstep.w34 <- sf.c.gstep[sf.id.w34]
sfc.deg.w34 <- sf.c.deg[sf.id.w34]
sfc.cumldeg.w34 <- sf.c.cumldeg[sf.id.w34]
sfc.bcent.w34 <- sf.c.bcent[sf.id.w34]

# Inst
sfi.frp.w34 <- sf.i.frp[sf.id.w34]
sfi.tdist.w34 <- sf.i.tdist[sf.id.w34]
sfi.gstep.w34 <- sf.i.gstep[sf.id.w34]
sfi.deg.w34 <- sf.i.deg[sf.id.w34]
sfi.cumldeg.w34 <- sf.i.cumldeg[sf.id.w34]
sfi.bcent.w34 <- sf.i.bcent[sf.id.w34]


## ATL
atl.id.w34 <- which(get.vertex.attribute(atl.m, "race") == "W" & 
                      get.vertex.attribute(atl.m, "age.grp") == "2")

# Main
atlm.frp.w34 <- atl.m.frp[atl.id.w34]
atlm.tdist.w34 <- atl.m.tdist[atl.id.w34]
atlm.gstep.w34 <- atl.m.gstep[atl.id.w34]
atlm.deg.w34 <- atl.m.deg[atl.id.w34]
atlm.cumldeg.w34 <- atl.m.cumldeg[atl.id.w34]
atlm.bcent.w34 <- atl.m.bcent[atl.id.w34]

# Casual
atlc.frp.w34 <- atl.c.frp[atl.id.w34]
atlc.tdist.w34 <- atl.c.tdist[atl.id.w34]
atlc.gstep.w34 <- atl.c.gstep[atl.id.w34]
atlc.deg.w34 <- atl.c.deg[atl.id.w34]
atlc.cumldeg.w34 <- atl.c.cumldeg[atl.id.w34]
atlc.bcent.w34 <- atl.c.bcent[atl.id.w34]

# Inst
atli.frp.w34 <- atl.i.frp[atl.id.w34]
atli.tdist.w34 <- atl.i.tdist[atl.id.w34]
atli.gstep.w34 <- atl.i.gstep[atl.id.w34]
atli.deg.w34 <- atl.i.deg[atl.id.w34]
atli.cumldeg.w34 <- atl.i.cumldeg[atl.id.w34]
atli.bcent.w34 <- atl.i.bcent[atl.id.w34]


## Black 35-44

## SF
sf.id.b44 <- which(get.vertex.attribute(sf.m, "race") == "B" & 
                     get.vertex.attribute(sf.m, "age.grp") == "3")

# Main
sfm.frp.b44 <- sf.m.frp[sf.id.b44]
sfm.tdist.b44 <- sf.m.tdist[sf.id.b44]
sfm.gstep.b44 <- sf.m.gstep[sf.id.b44]
sfm.deg.b44 <- sf.m.deg[sf.id.b44]
sfm.cumldeg.b44 <- sf.m.cumldeg[sf.id.b44]
sfm.bcent.b44 <- sf.m.bcent[sf.id.b44]

# Casual
sfc.frp.b44 <- sf.c.frp[sf.id.b44]
sfc.tdist.b44 <- sf.c.tdist[sf.id.b44]
sfc.gstep.b44 <- sf.c.gstep[sf.id.b44]
sfc.deg.b44 <- sf.c.deg[sf.id.b44]
sfc.cumldeg.b44 <- sf.c.cumldeg[sf.id.b44]
sfc.bcent.b44 <- sf.c.bcent[sf.id.b44]

# Inst
sfi.frp.b44 <- sf.i.frp[sf.id.b44]
sfi.tdist.b44 <- sf.i.tdist[sf.id.b44]
sfi.gstep.b44 <- sf.i.gstep[sf.id.b44]
sfi.deg.b44 <- sf.i.deg[sf.id.b44]
sfi.cumldeg.b44 <- sf.i.cumldeg[sf.id.b44]
sfi.bcent.b44 <- sf.i.bcent[sf.id.b44]


## ATL
atl.id.b44 <- which(get.vertex.attribute(atl.m, "race") == "B" & 
                      get.vertex.attribute(atl.m, "age.grp") == "3")

# Main
atlm.frp.b44 <- atl.m.frp[atl.id.b44]
atlm.tdist.b44 <- atl.m.tdist[atl.id.b44]
atlm.gstep.b44 <- atl.m.gstep[atl.id.b44]
atlm.deg.b44 <- atl.m.deg[atl.id.b44]
atlm.cumldeg.b44 <- atl.m.cumldeg[atl.id.b44]
atlm.bcent.b44 <- atl.m.bcent[atl.id.b44]

# Casual
atlc.frp.b44 <- atl.c.frp[atl.id.b44]
atlc.tdist.b44 <- atl.c.tdist[atl.id.b44]
atlc.gstep.b44 <- atl.c.gstep[atl.id.b44]
atlc.deg.b44 <- atl.c.deg[atl.id.b44]
atlc.cumldeg.b44 <- atl.c.cumldeg[atl.id.b44]
atlc.bcent.b44 <- atl.c.bcent[atl.id.b44]

# Inst
atli.frp.b44 <- atl.i.frp[atl.id.b44]
atli.tdist.b44 <- atl.i.tdist[atl.id.b44]
atli.gstep.b44 <- atl.i.gstep[atl.id.b44]
atli.deg.b44 <- atl.i.deg[atl.id.b44]
atli.cumldeg.b44 <- atl.i.cumldeg[atl.id.b44]
atli.bcent.b44 <- atl.i.bcent[atl.id.b44]


## White 35-44

## SF
sf.id.w44 <- which(get.vertex.attribute(sf.m, "race") == "W" & 
                     get.vertex.attribute(sf.m, "age.grp") == "3")

# Main
sfm.frp.w44 <- sf.m.frp[sf.id.w44]
sfm.tdist.w44 <- sf.m.tdist[sf.id.w44]
sfm.gstep.w44 <- sf.m.gstep[sf.id.w44]
sfm.deg.w44 <- sf.m.deg[sf.id.w44]
sfm.cumldeg.w44 <- sf.m.cumldeg[sf.id.w44]
sfm.bcent.w44 <- sf.m.bcent[sf.id.w44]

# Casual
sfc.frp.w44 <- sf.c.frp[sf.id.w44]
sfc.tdist.w44 <- sf.c.tdist[sf.id.w44]
sfc.gstep.w44 <- sf.c.gstep[sf.id.w44]
sfc.deg.w44 <- sf.c.deg[sf.id.w44]
sfc.cumldeg.w44 <- sf.c.cumldeg[sf.id.w44]
sfc.bcent.w44 <- sf.c.bcent[sf.id.w44]

# Inst
sfi.frp.w44 <- sf.i.frp[sf.id.w44]
sfi.tdist.w44 <- sf.i.tdist[sf.id.w44]
sfi.gstep.w44 <- sf.i.gstep[sf.id.w44]
sfi.deg.w44 <- sf.i.deg[sf.id.w44]
sfi.cumldeg.w44 <- sf.i.cumldeg[sf.id.w44]
sfi.bcent.w44 <- sf.i.bcent[sf.id.w44]

# ATL
atl.id.w44 <- which(get.vertex.attribute(atl.m, "race") == "W" & 
                      get.vertex.attribute(atl.m, "age.grp") == "3")

# Main
atlm.frp.w44 <- atl.m.frp[atl.id.w44]
atlm.tdist.w44 <- atl.m.tdist[atl.id.w44]
atlm.gstep.w44 <- atl.m.gstep[atl.id.w44]
atlm.deg.w44 <- atl.m.deg[atl.id.w44]
atlm.cumldeg.w44 <- atl.m.cumldeg[atl.id.w44]
atlm.bcent.w44 <- atl.m.bcent[atl.id.w44]

# Casual
atlc.frp.w44 <- atl.c.frp[atl.id.w44]
atlc.tdist.w44 <- atl.c.tdist[atl.id.w44]
atlc.gstep.w44 <- atl.c.gstep[atl.id.w44]
atlc.deg.w44 <- atl.c.deg[atl.id.w44]
atlc.cumldeg.w44 <- atl.c.cumldeg[atl.id.w44]
atlc.bcent.w44 <- atl.c.bcent[atl.id.w44]

# Inst
atli.frp.w44 <- atl.i.frp[atl.id.w44]
atli.tdist.w44 <- atl.i.tdist[atl.id.w44]
atli.gstep.w44 <- atl.i.gstep[atl.id.w44]
atli.deg.w44 <- atl.i.deg[atl.id.w44]
atli.cumldeg.w44 <- atl.i.cumldeg[atl.id.w44]
atli.bcent.w44 <- atl.i.bcent[atl.id.w44]


## Black 45-54

## SF
sf.id.b54 <- which(get.vertex.attribute(sf.m, "race") == "B" & 
                     get.vertex.attribute(sf.m, "age.grp") == "4")

# Main
sfm.frp.b54 <- sf.m.frp[sf.id.b54]
sfm.tdist.b54 <- sf.m.tdist[sf.id.b54]
sfm.gstep.b54 <- sf.m.gstep[sf.id.b54]
sfm.deg.b54 <- sf.m.deg[sf.id.b54]
sfm.cumldeg.b54 <- sf.m.cumldeg[sf.id.b54]
sfm.bcent.b54 <- sf.m.bcent[sf.id.b54]

# Casual
sfc.frp.b54 <- sf.c.frp[sf.id.b54]
sfc.tdist.b54 <- sf.c.tdist[sf.id.b54]
sfc.gstep.b54 <- sf.c.gstep[sf.id.b54]
sfc.deg.b54 <- sf.c.deg[sf.id.b54]
sfc.cumldeg.b54 <- sf.c.cumldeg[sf.id.b54]
sfc.bcent.b54 <- sf.c.bcent[sf.id.b54]

# Inst
sfi.frp.b54 <- sf.i.frp[sf.id.b54]
sfi.tdist.b54 <- sf.i.tdist[sf.id.b54]
sfi.gstep.b54 <- sf.i.gstep[sf.id.b54]
sfi.deg.b54 <- sf.i.deg[sf.id.b54]
sfi.cumldeg.b54 <- sf.i.cumldeg[sf.id.b54]
sfi.bcent.b54 <- sf.i.bcent[sf.id.b54]


## ATL
atl.id.b54 <- which(get.vertex.attribute(atl.m, "race") == "B" & 
                      get.vertex.attribute(atl.m, "age.grp") == "4")

# Main
atlm.frp.b54 <- atl.m.frp[atl.id.b54]
atlm.tdist.b54 <- atl.m.tdist[atl.id.b54]
atlm.gstep.b54 <- atl.m.gstep[atl.id.b54]
atlm.deg.b54 <- atl.m.deg[atl.id.b54]
atlm.cumldeg.b54 <- atl.m.cumldeg[atl.id.b54]
atlm.bcent.b54 <- atl.m.bcent[atl.id.b54]

# Casual
atlc.frp.b54 <- atl.c.frp[atl.id.b54]
atlc.tdist.b54 <- atl.c.tdist[atl.id.b54]
atlc.gstep.b54 <- atl.c.gstep[atl.id.b54]
atlc.deg.b54 <- atl.c.deg[atl.id.b54]
atlc.cumldeg.b54 <- atl.c.cumldeg[atl.id.b54]
atlc.bcent.b54 <- atl.c.bcent[atl.id.b54]

# Inst
atli.frp.b54 <- atl.i.frp[atl.id.b54]
atli.tdist.b54 <- atl.i.tdist[atl.id.b54]
atli.gstep.b54 <- atl.i.gstep[atl.id.b54]
atli.deg.b54 <- atl.i.deg[atl.id.b54]
atli.cumldeg.b54 <- atl.i.cumldeg[atl.id.b54]
atli.bcent.b54 <- atl.i.bcent[atl.id.b54]


## White 45-54

## SF
sf.id.w54 <- which(get.vertex.attribute(sf.m, "race") == "W" & 
                     get.vertex.attribute(sf.m, "age.grp") == "4")

# Main
sfm.frp.w54 <- sf.m.frp[sf.id.w54]
sfm.tdist.w54 <- sf.m.tdist[sf.id.w54]
sfm.gstep.w54 <- sf.m.gstep[sf.id.w54]
sfm.deg.w54 <- sf.m.deg[sf.id.w54]
sfm.cumldeg.w54 <- sf.m.cumldeg[sf.id.w54]
sfm.bcent.w54 <- sf.m.bcent[sf.id.w54]

# Casual
sfc.frp.w54 <- sf.c.frp[sf.id.w54]
sfc.tdist.w54 <- sf.c.tdist[sf.id.w54]
sfc.gstep.w54 <- sf.c.gstep[sf.id.w54]
sfc.deg.w54 <- sf.c.deg[sf.id.w54]
sfc.cumldeg.w54 <- sf.c.cumldeg[sf.id.w54]
sfc.bcent.w54 <- sf.c.bcent[sf.id.w54]

# Inst
sfi.frp.w54 <- sf.i.frp[sf.id.w54]
sfi.tdist.w54 <- sf.i.tdist[sf.id.w54]
sfi.gstep.w54 <- sf.i.gstep[sf.id.w54]
sfi.deg.w54 <- sf.i.deg[sf.id.w54]
sfi.cumldeg.w54 <- sf.i.cumldeg[sf.id.w54]
sfi.bcent.w54 <- sf.i.bcent[sf.id.w54]


## ATL
atl.id.w54 <- which(get.vertex.attribute(atl.m, "race") == "W" & 
                      get.vertex.attribute(atl.m, "age.grp") == "4")

# Main
atlm.frp.w54 <- atl.m.frp[atl.id.w54]
atlm.tdist.w54 <- atl.m.tdist[atl.id.w54]
atlm.gstep.w54 <- atl.m.gstep[atl.id.w54]
atlm.deg.w54 <- atl.m.deg[atl.id.w54]
atlm.cumldeg.w54 <- atl.m.cumldeg[atl.id.w54]
atlm.bcent.w54 <- atl.m.bcent[atl.id.w54]

# Casual
atlc.frp.w54 <- atl.c.frp[atl.id.w54]
atlc.tdist.w54 <- atl.c.tdist[atl.id.w54]
atlc.gstep.w54 <- atl.c.gstep[atl.id.w54]
atlc.deg.w54 <- atl.c.deg[atl.id.w54]
atlc.cumldeg.w54 <- atl.c.cumldeg[atl.id.w54]
atlc.bcent.w54 <- atl.c.bcent[atl.id.w54]

# Inst
atli.frp.w54 <- atl.i.frp[atl.id.w54]
atli.tdist.w54 <- atl.i.tdist[atl.id.w54]
atli.gstep.w54 <- atl.i.gstep[atl.id.w54]
atli.deg.w54 <- atl.i.deg[atl.id.w54]
atli.cumldeg.w54 <- atl.i.cumldeg[atl.id.w54]
atli.bcent.w54 <- atl.i.bcent[atl.id.w54]


## Black 55+

## SF
sf.id.b64 <- which(get.vertex.attribute(sf.m, "race") == "B" & 
                     get.vertex.attribute(sf.m, "age.grp") == "5")

# Main
sfm.frp.b64 <- sf.m.frp[sf.id.b64]
sfm.tdist.b64 <- sf.m.tdist[sf.id.b64]
sfm.gstep.b64 <- sf.m.gstep[sf.id.b64]
sfm.deg.b64 <- sf.m.deg[sf.id.b64]
sfm.cumldeg.b64 <- sf.m.cumldeg[sf.id.b64]
sfm.bcent.b64 <- sf.m.bcent[sf.id.b64]

# Casual
sfc.frp.b64 <- sf.c.frp[sf.id.b64]
sfc.tdist.b64 <- sf.c.tdist[sf.id.b64]
sfc.gstep.b64 <- sf.c.gstep[sf.id.b64]
sfc.deg.b64 <- sf.c.deg[sf.id.b64]
sfc.cumldeg.b64 <- sf.c.cumldeg[sf.id.b64]
sfc.bcent.b64 <- sf.c.bcent[sf.id.b64]

# Inst
sfi.frp.b64 <- sf.i.frp[sf.id.b64]
sfi.tdist.b64 <- sf.i.tdist[sf.id.b64]
sfi.gstep.b64 <- sf.i.gstep[sf.id.b64]
sfi.deg.b64 <- sf.i.deg[sf.id.b64]
sfi.cumldeg.b64 <- sf.i.cumldeg[sf.id.b64]
sfi.bcent.b64 <- sf.i.bcent[sf.id.b64]


## ATL
atl.id.b64 <- which(get.vertex.attribute(atl.m, "race") == "B" & 
                      get.vertex.attribute(atl.m, "age.grp") == "5")

# Main
atlm.frp.b64 <- atl.m.frp[atl.id.b64]
atlm.tdist.b64 <- atl.m.tdist[atl.id.b64]
atlm.gstep.b64 <- atl.m.gstep[atl.id.b64]
atlm.deg.b64 <- atl.m.deg[atl.id.b64]
atlm.cumldeg.b64 <- atl.m.cumldeg[atl.id.b64]
atlm.bcent.b64 <- atl.m.bcent[atl.id.b64]

# Casual
atlc.frp.b64 <- atl.c.frp[atl.id.b64]
atlc.tdist.b64 <- atl.c.tdist[atl.id.b64]
atlc.gstep.b64 <- atl.c.gstep[atl.id.b64]
atlc.deg.b64 <- atl.c.deg[atl.id.b64]
atlc.cumldeg.b64 <- atl.c.cumldeg[atl.id.b64]
atlc.bcent.b64 <- atl.c.bcent[atl.id.b64]

# Inst
atli.frp.b64 <- atl.i.frp[atl.id.b64]
atli.tdist.b64 <- atl.i.tdist[atl.id.b64]
atli.gstep.b64 <- atl.i.gstep[atl.id.b64]
atli.deg.b64 <- atl.i.deg[atl.id.b64]
atli.cumldeg.b64 <- atl.i.cumldeg[atl.id.b64]
atli.bcent.b64 <- atl.i.bcent[atl.id.b64]


## White 55+

## SF
sf.id.w64 <- which(get.vertex.attribute(sf.m, "race") == "W" & 
                     get.vertex.attribute(sf.m, "age.grp") == "5")

# Main
sfm.frp.w64 <- sf.m.frp[sf.id.w64]
sfm.tdist.w64 <- sf.m.tdist[sf.id.w64]
sfm.gstep.w64 <- sf.m.gstep[sf.id.w64]
sfm.deg.w64 <- sf.m.deg[sf.id.w64]
sfm.cumldeg.w64 <- sf.m.cumldeg[sf.id.w64]
sfm.bcent.w64 <- sf.m.bcent[sf.id.w64]

# Casual
sfc.frp.w64 <- sf.c.frp[sf.id.w64]
sfc.tdist.w64 <- sf.c.tdist[sf.id.w64]
sfc.gstep.w64 <- sf.c.gstep[sf.id.w64]
sfc.deg.w64 <- sf.c.deg[sf.id.w64]
sfc.cumldeg.w64 <- sf.c.cumldeg[sf.id.w64]
sfc.bcent.w64 <- sf.c.bcent[sf.id.w64]

# Inst
sfi.frp.w64 <- sf.i.frp[sf.id.w64]
sfi.tdist.w64 <- sf.i.tdist[sf.id.w64]
sfi.gstep.w64 <- sf.i.gstep[sf.id.w64]
sfi.deg.w64 <- sf.i.deg[sf.id.w64]
sfi.cumldeg.w64 <- sf.i.cumldeg[sf.id.w64]
sfi.bcent.w64 <- sf.i.bcent[sf.id.w64]


## ATL
atl.id.w64 <- which(get.vertex.attribute(atl.m, "race") == "W" & 
                      get.vertex.attribute(atl.m, "age.grp") == "5")

# Main
atlm.frp.w64 <- atl.m.frp[atl.id.w64]
atlm.tdist.w64 <- atl.m.tdist[atl.id.w64]
atlm.gstep.w64 <- atl.m.gstep[atl.id.w64]
atlm.deg.w64 <- atl.m.deg[atl.id.w64]
atlm.cumldeg.w64 <- atl.m.cumldeg[atl.id.w64]
atlm.bcent.w64 <- atl.m.bcent[atl.id.w64]

# Casual
atlc.frp.w64 <- atl.c.frp[atl.id.w64]
atlc.tdist.w64 <- atl.c.tdist[atl.id.w64]
atlc.gstep.w64 <- atl.c.gstep[atl.id.w64]
atlc.deg.w64 <- atl.c.deg[atl.id.w64]
atlc.cumldeg.w64 <- atl.c.cumldeg[atl.id.w64]
atlc.bcent.w64 <- atl.c.bcent[atl.id.w64]

# Inst
atli.frp.w64 <- atl.i.frp[atl.id.w64]
atli.tdist.w64 <- atl.i.tdist[atl.id.w64]
atli.gstep.w64 <- atl.i.gstep[atl.id.w64]
atli.deg.w64 <- atl.i.deg[atl.id.w64]
atli.cumldeg.w64 <- atl.i.cumldeg[atl.id.w64]
atli.bcent.w64 <- atl.i.bcent[atl.id.w64]

# 4a. Analysis: FRP -------------------------------------------------------

### Plots ###


par(mfrow = c(2, 3), oma = c(2, 0, 2, 0), xpd = NA)

## Overall
# SF
matplot(sf.m.frp, type = "l", xlab = "Week", ylab = "FRP", main = "SF Main")
matplot(sf.c.frp, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "SF Casual")
matplot(sf.i.frp, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "SF One-Time")

# ATL
matplot(atl.m.frp, type = "l", ylim = c(0, 20), xlab = "Week", ylab = "FRP", main = "ATL Main")
matplot(atl.c.frp, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "ATL Casual")
matplot(atl.i.frp, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "ATL One-Time")

title("Distribution of 5-Year Foward Reachable Paths by Partnership Type", outer = TRUE)
title()

## Race - BLACK
# SF
matplot(sfm.frp.b, type = "l", xlab = "Week", ylab = "FRP", main = "SF Main")
matplot(sfc.frp.b, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "SF Casual")
matplot(sfi.frp.b, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "SF One-Time")

# ATL
matplot(atlm.frp.b, type = "l", ylim = c(0, 20), xlab = "Week", ylab = "FRP", main = "ATL Main")
matplot(atlc.frp.b, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "ATL Casual")
matplot(atli.frp.b, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "ATL One-Time")

title("Distribution of 5-Year Foward Reachable Paths by Partnership Type Among Black MSM", outer = TRUE)
title()

## Race - WHITE
# SF
matplot(sfm.frp.w, type = "l", xlab = "Week", ylab = "FRP", main = "SF Main")
matplot(sfc.frp.w, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "SF Casual")
matplot(sfi.frp.w, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "SF One-Time")

# ATL
matplot(atlm.frp.w, type = "l", ylim = c(0, 20), xlab = "Week", ylab = "FRP", main = "ATL Main")
matplot(atlc.frp.w, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "ATL Casual")
matplot(atli.frp.w, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "ATL One-Time")

title("Distribution of 5-Year Foward Reachable Paths by Partnership Type Among White MSM", outer = TRUE)
title()

## Age - 15-24
# SF
matplot(sfm.frp.24, type = "l", xlab = "Week", ylab = "FRP", main = "SF Main")
matplot(sfc.frp.24, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "SF Casual")
matplot(sfi.frp.24, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "SF One-Time")

# ATL
matplot(atlm.frp.24, type = "l", ylim = c(0, 20), xlab = "Week", ylab = "FRP", main = "ATL Main")
matplot(atlc.frp.24, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "ATL Casual")
matplot(atli.frp.24, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "ATL One-Time")

title("Distribution of 5-Year Foward Reachable Paths by Partnership Type Among MSM Aged 15-24", outer = TRUE)
title()

## Age - 25-34
# SF
matplot(sfm.frp.34, type = "l", ylim = c(0, 20), xlab = "Week", ylab = "FRP", main = "SF Main")
matplot(sfc.frp.34, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "SF Casual")
matplot(sfi.frp.34, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "SF One-Time")

# ATL
matplot(atlm.frp.34, type = "l", ylim = c(0, 20), xlab = "Week", ylab = "FRP", main = "ATL Main")
matplot(atlc.frp.34, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "ATL Casual")
matplot(atli.frp.34, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "ATL One-Time")

title("Distribution of 5-Year Foward Reachable Paths by Partnership Type Among MSM Aged 25-34", outer = TRUE)
title()

## Age - 35-44
# SF
matplot(sfm.frp.44, type = "l", ylim = c(0, 20), xlab = "Week", ylab = "FRP", main = "SF Main")
matplot(sfc.frp.44, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "SF Casual")
matplot(sfi.frp.44, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "SF One-Time")

# ATL
matplot(atlm.frp.44, type = "l", ylim = c(0, 20), xlab = "Week", ylab = "FRP", main = "ATL Main")
matplot(atlc.frp.44, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "ATL Casual")
matplot(atli.frp.44, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "ATL One-Time")

title("Distribution of 5-Year Foward Reachable Paths by Partnership Type Among MSM Aged 35-44", outer = TRUE)
title()

## Age - 45-54
# SF
matplot(sfm.frp.54, type = "l", ylim = c(0, 20), xlab = "Week", ylab = "FRP", main = "SF Main")
matplot(sfc.frp.54, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "SF Casual")
matplot(sfi.frp.54, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "SF One-Time")

# ATL
matplot(atlm.frp.54, type = "l", ylim = c(0, 20), xlab = "Week", ylab = "FRP", main = "ATL Main")
matplot(atlc.frp.54, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "ATL Casual")
matplot(atli.frp.54, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "ATL One-Time")

title("Distribution of 5-Year Foward Reachable Paths by Partnership Type Among MSM Aged 45-54", outer = TRUE)
title()

## Age - 55-64
# SF
matplot(sfm.frp.64, type = "l", ylim = c(0, 20), xlab = "Week", ylab = "FRP", main = "SF Main")
matplot(sfc.frp.64, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "SF Casual")
matplot(sfi.frp.64, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "SF One-Time")

# ATL
matplot(atlm.frp.64, type = "l", ylim = c(0, 20), xlab = "Week", ylab = "FRP", main = "ATL Main")
matplot(atlc.frp.64, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "ATL Casual")
matplot(atli.frp.64, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "ATL One-Time")

title("Distribution of 5-Year Foward Reachable Paths by Partnership Type Among MSM Aged 55-64", outer = TRUE)
title()

## Race - BLACK, Age - 15-24
# SF
matplot(sfm.frp.b24, type = "l", xlab = "Week", ylab = "FRP", main = "SF Main")
matplot(sfc.frp.b24, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "SF Casual")
matplot(sfi.frp.b24, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "SF One-Time")

# ATL
matplot(atlm.frp.b24, type = "l", ylim = c(0, 20), xlab = "Week", ylab = "FRP", main = "ATL Main")
matplot(atlc.frp.b24, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "ATL Casual")
matplot(atli.frp.b24, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "ATL One-Time")

title("Distribution of 5-Year Foward Reachable Paths by Partnership Type Among Black MSM Aged 15-24", outer = TRUE)
title()

## Race - WHITE, Age - 15-24
# SF
matplot(sfm.frp.w24, type = "l", xlab = "Week", ylab = "FRP", main = "SF Main")
matplot(sfc.frp.w24, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "SF Casual")
matplot(sfi.frp.w24, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "SF One-Time")

# ATL
matplot(atlm.frp.w24, type = "l", ylim = c(0, 20), xlab = "Week", ylab = "FRP", main = "ATL Main")
matplot(atlc.frp.w24, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "ATL Casual")
matplot(atli.frp.w24, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "ATL One-Time")

title("Distribution of 5-Year Foward Reachable Paths by Partnership Type Among White MSM Aged 15-24", outer = TRUE)
title()

## Race - BLACK, Age - 25-34
# SF
matplot(sfm.frp.b34, type = "l", ylim = c(0, 20), xlab = "Week", ylab = "FRP", main = "SF Main")
matplot(sfc.frp.b34, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "SF Casual")
matplot(sfi.frp.b34, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "SF One-Time")

# ATL
matplot(atlm.frp.b34, type = "l", ylim = c(0, 20), xlab = "Week", ylab = "FRP", main = "ATL Main")
matplot(atlc.frp.b34, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "ATL Casual")
matplot(atli.frp.b34, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "ATL One-Time")

title("Distribution of 5-Year Foward Reachable Paths by Partnership Type Among Black MSM Aged 25-34", outer = TRUE)
title()

## Race - WHITE, Age - 25-34
# SF
matplot(sfm.frp.w34, type = "l", ylim = c(0, 20), xlab = "Week", ylab = "FRP", main = "SF Main")
matplot(sfc.frp.w34, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "SF Casual")
matplot(sfi.frp.w34, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "SF One-Time")

# ATL
matplot(atlm.frp.w34, type = "l", ylim = c(0, 20), xlab = "Week", ylab = "FRP", main = "ATL Main")
matplot(atlc.frp.w34, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "ATL Casual")
matplot(atli.frp.w34, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "ATL One-Time")

title("Distribution of 5-Year Foward Reachable Paths by Partnership Type Among White MSM Aged 25-34", outer = TRUE)
title()

## Race - BLACK, Age - 35-44
# SF
matplot(sfm.frp.b44, type = "l", ylim = c(0, 20), xlab = "Week", ylab = "FRP", main = "SF Main")
matplot(sfc.frp.b44, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "SF Casual")
matplot(sfi.frp.b44, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "SF One-Time")

# ATL
matplot(atlm.frp.b44, type = "l", ylim = c(0, 20), xlab = "Week", ylab = "FRP", main = "ATL Main")
matplot(atlc.frp.b44, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "ATL Casual")
matplot(atli.frp.b44, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "ATL One-Time")

title("Distribution of 5-Year Foward Reachable Paths by Partnership Type Among Black MSM Aged 35-44", outer = TRUE)
title()

## Race - WHITE, Age - 35-44
# SF
matplot(sfm.frp.w44, type = "l", ylim = c(0, 20), xlab = "Week", ylab = "FRP", main = "SF Main")
matplot(sfc.frp.w44, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "SF Casual")
matplot(sfi.frp.w44, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "SF One-Time")

# ATL
matplot(atlm.frp.w44, type = "l", ylim = c(0, 20), xlab = "Week", ylab = "FRP", main = "ATL Main")
matplot(atlc.frp.w44, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "ATL Casual")
matplot(atli.frp.w44, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "ATL One-Time")

title("Distribution of 5-Year Foward Reachable Paths by Partnership Type Among White MSM Aged 35-44", outer = TRUE)
title()

## Race - BLACK, Age - 45-54
# SF
matplot(sfm.frp.b54, type = "l", ylim = c(0, 20), xlab = "Week", ylab = "FRP", main = "SF Main")
matplot(sfc.frp.b54, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "SF Casual")
matplot(sfi.frp.b54, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "SF One-Time")

# ATL
matplot(atlm.frp.b54, type = "l", ylim = c(0, 20), xlab = "Week", ylab = "FRP", main = "ATL Main")
matplot(atlc.frp.b54, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "ATL Casual")
matplot(atli.frp.b54, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "ATL One-Time")

title("Distribution of 5-Year Foward Reachable Paths by Partnership Type Among Black MSM Aged 45-54", outer = TRUE)
title()

## Race - WHITE, Age - 45-54
# SF
matplot(sfm.frp.w54, type = "l", ylim = c(0, 20), xlab = "Week", ylab = "FRP", main = "SF Main")
matplot(sfc.frp.w54, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "SF Casual")
matplot(sfi.frp.w54, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "SF One-Time")

# ATL
matplot(atlm.frp.w54, type = "l", ylim = c(0, 20), xlab = "Week", ylab = "FRP", main = "ATL Main")
matplot(atlc.frp.w54, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "ATL Casual")
matplot(atli.frp.w54, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "ATL One-Time")

title("Distribution of 5-Year Foward Reachable Paths by Partnership Type Among White MSM Aged 45-54", outer = TRUE)
title()

## Race - BLACK, Age - 55-64
# SF
matplot(sfm.frp.b64, type = "l", ylim = c(0, 20), xlab = "Week", ylab = "FRP", main = "SF Main")
matplot(sfc.frp.b64, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "SF Casual")
matplot(sfi.frp.b64, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "SF One-Time")

# ATL
matplot(atlm.frp.b64, type = "l", ylim = c(0, 20), xlab = "Week", ylab = "FRP", main = "ATL Main")
matplot(atlc.frp.b64, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "ATL Casual")
matplot(atli.frp.b64, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "ATL One-Time")

title("Distribution of 5-Year Foward Reachable Paths by Partnership Type Among Black MSM Aged 55-64", outer = TRUE)
title()

## Race - WHITE, Age - 55-64
# SF
matplot(sfm.frp.w64, type = "l", ylim = c(0, 20), xlab = "Week", ylab = "FRP", main = "SF Main")
matplot(sfc.frp.w64, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "SF Casual")
matplot(sfi.frp.w64, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "SF One-Time")

# ATL
matplot(atlm.frp.w64, type = "l", ylim = c(0, 20), xlab = "Week", ylab = "FRP", main = "ATL Main")
matplot(atlc.frp.w64, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "ATL Casual")
matplot(atli.frp.w64, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "ATL One-Time")

title("Distribution of 5-Year Foward Reachable Paths by Partnership Type Among White MSM Aged 55-64", outer = TRUE)
title()


### Summary Stats ###



## Overall
# SF
matplot(sf.m.frp, type = "l", xlab = "Week", ylab = "FRP", main = "SF Main")
matplot(sf.c.frp, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "SF Casual")
matplot(sf.i.frp, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "SF One-Time")

# ATL
matplot(atl.m.frp, type = "l", ylim = c(0, 20), xlab = "Week", ylab = "FRP", main = "ATL Main")
matplot(atl.c.frp, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "ATL Casual")
matplot(atl.i.frp, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "ATL One-Time")

title("Distribution of 5-Year Foward Reachable Paths by Partnership Type", outer = TRUE)
title()

# START HERE --------------------------------------------------------------



# 4b. Analysis: tdist -----------------------------------------------------

# OLD CODE ----------------------------------------------------------------


# Overall
sf.sum.all <- summary(t(sf.all))
write.csv(sf.sum.main, file = "SF_Main_Summary.csv")

atl.sum.all <- summary(t(atl.all))
write.csv(atl.sum.main, file = "ATL_Main_Summary.csv")


# Overall
sf.avg.main <- rowMeans(sf.main)
sf.sum.main <- summary(t(sf.main))
sf.sd.main <- apply(sf.main, 1, sd)

write.csv(sf.sum.main, file = "SF_Main_Summary.csv")
























