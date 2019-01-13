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


# 3. Extract Vertex IDs ------------------------------------------------------

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


### Age ###
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
atlc.frp.44 <- atl.c.frp[atl.id.44]
atlc.tdist.44 <- atl.c.tdist[atl.id.44]
atlc.gstep.44 <- atl.c.gstep[atl.id.44]
atlc.deg.44 <- atl.c.deg[atl.id.44]
atlc.cumldeg.44 <- atl.c.cumldeg[atl.id.44]
atlc.bcent.44 <- atl.c.bcent[atl.id.44]


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


### Race & Age
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
                      get.vertex.attribute(atl.m, "age.grp") == "2")s

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

sfm.frp.w44 <- sf.m.frp[sf.id.w44]
sfm.tdist.w44 <- sf.m.tdist[sf.id.w44]
sfm.gstep.w44 <- sf.m.gstep[sf.id.w44]
sfm.deg.w44 <- sf.m.deg[sf.id.w44]
sfm.cumldeg.w44 <- sf.m.cumldeg[sf.id.w44]
sfm.bcent.w44 <- sf.m.bcent[sf.id.w44]

# ATL
atl.id.w44 <- which(get.vertex.attribute(atl.m, "race") == "W" & 
                      get.vertex.attribute(atl.m, "age.grp") == "3")

atlm.frp.w44 <- atl.m.frp[atl.id.w44]
atlm.tdist.w44 <- atl.m.tdist[atl.id.w44]
atlm.gstep.w44 <- atl.m.gstep[atl.id.w44]
atlm.deg.w44 <- atl.m.deg[atl.id.w44]
atlm.cumldeg.w44 <- atl.m.cumldeg[atl.id.w44]
atlm.bcent.w44 <- atl.m.bcent[atl.id.w44]


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



# 4. Analysis -------------------------------------------------------------


par(mfrow = c(2, 3), oma = c(2, 0, 2, 0), xpd = NA)

### FRP ####

## Overall
# SF
matplot(sf.m.frp , type = "l", xlab = "Week", ylab = "FRP", main = "SF Main")
matplot(sf.c.frp , type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "SF Casual")
matplot(sf.i.frp , type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "SF One-Time")

# ATL
matplot(atl.m.frp, type = "l", ylim = c(0, 20), xlab = "Week", ylab = "FRP", main = "ATL Main")
matplot(atl.c.frp, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "ATL Casual")
matplot(atl.i.frp, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "ATL One-Time")

title("Distribution of 5-Year Foward Reachable Paths by Partnership Type", outer = TRUE)
title()

## Race - BLACK
# SF
matplot(sfm.frp.b , type = "l", xlab = "Week", ylab = "FRP", main = "SF Main")
matplot(sfc.frp.b , type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "SF Casual")
matplot(sfi.frp.b , type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "SF One-Time")

# ATL
matplot(atlm.frp.b, type = "l", ylim = c(0, 20), xlab = "Week", ylab = "FRP", main = "ATL Main")
matplot(atlc.frp.b, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "ATL Casual")
matplot(atli.frp.b, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "ATL One-Time")

title("Distribution of 5-Year Foward Reachable Paths by Partnership Type Among Black MSM", outer = TRUE)
title()

## Race - WHITE
# SF
matplot(sfm.frp.w , type = "l", xlab = "Week", ylab = "FRP", main = "SF Main")
matplot(sfc.frp.w , type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "SF Casual")
matplot(sfi.frp.w , type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "SF One-Time")

# ATL
matplot(atlm.frp.w, type = "l", ylim = c(0, 20), xlab = "Week", ylab = "FRP", main = "ATL Main")
matplot(atlc.frp.w, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "ATL Casual")
matplot(atli.frp.w, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "FRP", main = "ATL One-Time")

title("Distribution of 5-Year Foward Reachable Paths by Partnership Type Among White MSM", outer = TRUE)
title()

## Age


# START HERE --------------------------------------------------------------


## Plots for CROI

par(mfrow = c(2, 3), oma = c(2, 0, 2, 0), xpd = NA)

# Main SF
matplot(x = seq(1, 260, 8), samp.sf.main, type = "l", lty = 1, xlab = "Week",
        ylab = "Foward Reachable Path", main = "San Francisco Main Partnerships")

# Casual SF
matplot(x = seq(1, 260, 8), samp.sf.casl, type = "l", lty = 1, ylim = c(0, 10000), xlab = "Week",
        ylab = "Foward Reachable Path", main = "San Francisco Casual Partnerships")

# Inst SF
matplot(x = seq(1, 260, 8), samp.sf.inst, type = "l", lty = 1, ylim = c(0, 10000), xlab = "Week",
        ylab = "Foward Reachable Path", main = "San Francisco One-Time Partnerships")

# Main ATL
matplot(x = seq(1, 260, 8), samp.atl.main, type = "l",  lty = 1, ylim = c(0, 20), xlab = "Week",
        ylab = "Foward Reachable Path", main = "Atlanta Main Partnerships")

# Casual ATL
matplot(x = seq(1, 260, 8), samp.atl.casl, type = "l", lty = 1, ylim = c(0, 10000), xlab = "Week",
        ylab = "Foward Reachable Path", main = "Atlanta Casual Partnerships")

# Inst ATl
matplot(x = seq(1, 260, 8), samp.atl.inst, type = "l", lty = 1, ylim = c(0, 10000), xlab = "Week",
        ylab = "Foward Reachable Path", main = "Atlanta One-Time Partnerships")

title("Distribution of 5-Year Foward Reachable Paths by Partnership Type", outer = TRUE)

#legend(x = -0.5,  y = 3.5, c("Forward reachable Paths city by partnership type"), xpd = TRUE, horiz = TRUE,
#       inset = 0, bty = "n", cex = 1.2)

#legend("center", c("IM", "IBD", "1R", "2R"), xpd = TRUE, horiz = TRUE, inset = c(0,
#             0), bty = "n", pch = c(4, 2, 15, 19), col = 1:4, cex = 2)



# OLD CODE ----------------------------------------------------------------


# Overall
sf.sum.all <- summary(t(sf.all))
write.csv(sf.sum.main, file = "SF_Main_Summary.csv")

atl.sum.all <- summary(t(atl.all))
write.csv(atl.sum.main, file = "ATL_Main_Summary.csv")


### Main partnerships ###


## Plots

# Overall
matplot(samp.sf.main, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Main")
matplot(samp.atl.main, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Main")

# By race

# Black
matplot(sf.main.B, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Main - Black")
matplot(atl.main.B, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Main - Black")

# White
matplot(sf.main.W, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Main - White")
matplot(atl.main.W, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Main - White")

# By age

# 0-24
matplot(sf.main.24, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Main - <25 years")
matplot(atl.main.24, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Main - <25 years")

# 25-34
matplot(sf.main.34, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Main - 25-34 years")
matplot(atl.main.34, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Main - 25-34 years")

# 35-44
matplot(sf.main.44, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Main - 35-44 years")
matplot(atl.main.44, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Main - 35-44 years")

# 45-54
matplot(sf.main.54, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Main - 45-54 years")
matplot(atl.main.54, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Main - 45-54 years")

# 55+
matplot(sf.main.64, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Main - 55+ years")
matplot(atl.main.64, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Main - 55+ years")

# By race & age

# Black 0-24
matplot(sf.main.B24, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Main - Black & <25 years")
matplot(atl.main.B24, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Main - Black & <25 years")

# White 0-24
matplot(sf.main.W24, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Main - White & <25 years")
matplot(atl.main.W24, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Main - White & <25 years")

# Black 25-34
matplot(sf.main.B34, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Main - Black & 25-34 years")
matplot(atl.main.B34, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Main - Black & 25-34 years")

# White 25-34
matplot(sf.main.W34, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Main - White & 25-34 years")
matplot(atl.main.W34, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Main - White & 25-34 years")

# Black 35-44
matplot(sf.main.B44, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Main - Black & 35-44 years")
matplot(atl.main.B44, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Main - Black & 35-44 years")

# White 35-44
matplot(sf.main.W44, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Main - White & 35-44 years")
matplot(atl.main.W44, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Main - White & 35-44 years")

# Black 45-54
matplot(sf.main.B54, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Main - Black & 45-54 years")
matplot(atl.main.B54, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Main - Black & 45-54 years")

# White 45-54
matplot(sf.main.W54, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Main - White & 45-54 years")
matplot(atl.main.W54, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Main - White & 45-54 years")

# Black 55+
matplot(sf.main.B64, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Main - Black & 55+ years")
matplot(atl.main.B64, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Main - Black & 55+ years")

# White 55+
matplot(sf.main.W64, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Main - White & 55+ years")
matplot(atl.main.W64, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Main - White & 55+ years")


## Summary

# Overall
sf.avg.main <- rowMeans(sf.main)
sf.sum.main <- summary(t(sf.main))
sf.sd.main <- apply(sf.main, 1, sd)

write.csv(sf.sum.main, file = "SF_Main_Summary.csv")

atl.avg.main <- rowMeans(atl.main)
atl.sum.main <- summary(t(atl.main))
atl.sd.main <- apply(atl.main, 1, sd)

write.csv(atl.sum.main, file = "ATL_Main_Summary.csv")

# By race

# Black
sf.avg.mainB <- rowMeans(sf.main.B)
sf.sum.mainB <- summary(t(sf.main.B))
sf.sd.mainB <- apply(sf.main.B, 1, sd)

write.csv(sf.sum.mainB, file = "SF_Main_Black_Summary.csv")

atl.avg.mainB <- rowMeans(atl.main.B)
atl.sum.mainB <- summary(t(atl.main.B))
atl.sd.mainB <- apply(atl.main.B, 1, sd)

write.csv(atl.sum.mainB, file = "ATL_Main_Black_Summary.csv")

# White
sf.avg.mainW <- rowMeans(sf.main.W)
sf.sum.mainW <- summary(t(sf.main.W))
sf.sd.mainW <- apply(sf.main.W, 1, sd)

write.csv(sf.sum.mainW, file = "SF_Main_White_Summary.csv")

atl.avg.mainW <- rowMeans(atl.main.W)
atl.sum.mainW <- summary(t(atl.main.W))
atl.sd.mainW <- apply(atl.main.W, 1, sd)

write.csv(atl.sum.mainW, file = "ATL_Main_White_Summary.csv")

# By age

# 0-24
sf.avg.main24 <- rowMeans(sf.main.24)
sf.sum.main24 <- summary(t(sf.main.24))
sf.sd.main24 <- apply(sf.main.24, 1, sd)

write.csv(sf.sum.main24, file = "SF_Main_<25_Summary.csv")

atl.avg.main24 <- rowMeans(atl.main.24)
atl.sum.main24 <- summary(t(atl.main.24))
atl.sd.main24 <- apply(atl.main.24, 1, sd)

write.csv(atl.sum.main24, file = "ATL_Main_<25_Summary.csv")

# 25-34
sf.avg.main34 <- rowMeans(sf.main.34)
sf.sum.main34 <- summary(t(sf.main.34))
sf.sd.main34 <- apply(sf.main.34, 1, sd)

write.csv(sf.sum.main34, file = "SF_Main_25-34_Summary.csv")

atl.avg.main34 <- rowMeans(atl.main.34)
atl.sum.main34 <- summary(t(atl.main.34))
atl.sd.main34 <- apply(atl.main.34, 1, sd)

# 35-44
sf.avg.main44 <- rowMeans(sf.main.44)
sf.sum.main44 <- summary(t(sf.main.44))
sf.sd.main44 <- apply(sf.main.44, 1, sd)

atl.avg.main44 <- rowMeans(atl.main.44)
atl.sum.main44 <- summary(t(atl.main.44))
atl.sd.main44 <- apply(atl.main.44, 1, sd)

# 45-54
sf.avg.main54 <- rowMeans(sf.main.54)
sf.sum.main54 <- summary(t(sf.main.54))
sf.sd.main54 <- apply(sf.main.54, 1, sd)

atl.avg.main54 <- rowMeans(atl.main.54)
atl.sum.main54 <- summary(t(atl.main.54))
atl.sd.main54 <- apply(atl.main.54, 1, sd)

# 55+
sf.avg.main64 <- rowMeans(sf.main.64)
sf.sum.main64 <- summary(t(sf.main.64))
sf.sd.main64 <- apply(sf.main.64, 1, sd)

atl.avg.main64 <- rowMeans(atl.main.64)
atl.sum.main64 <- summary(t(atl.main.64))
atl.sd.main64 <- apply(atl.main.64, 1, sd)

# By race & age

# Black 0-24
sf.avg.mainB24 <- rowMeans(sf.main.B24)
sf.sum.mainB24 <- summary(t(sf.main.B24))
sf.sd.mainB24 <- apply(sf.main.B24, 1, sd)

atl.avg.mainB24 <- rowMeans(atl.main.B24)
atl.sum.mainB24 <- summary(t(atl.main.B24))
atl.sd.mainB24 <- apply(atl.main.B24, 1, sd)

# White 0-24
sf.avg.mainW24 <- rowMeans(sf.main.W24)
sf.sum.mainW24 <- summary(t(sf.main.W24))
sf.sd.mainW24 <- apply(sf.main.W24, 1, sd)

atl.avg.mainW24 <- rowMeans(atl.main.W24)
atl.sum.mainW24 <- summary(t(atl.main.W24))
atl.sd.mainW24 <- apply(atl.main.W24, 1, sd)

# Black 25-34
sf.avg.mainB34 <- rowMeans(sf.main.B34)
sf.sum.mainB34 <- summary(t(sf.main.B34))
sf.sd.mainB34 <- apply(sf.main.B34, 1, sd)

atl.avg.mainB34 <- rowMeans(atl.main.B34)
atl.sum.mainB34 <- summary(t(atl.main.B34))
atl.sd.mainB34 <- apply(atl.main.B34, 1, sd)

# White 25-34
sf.avg.mainW34 <- rowMeans(sf.main.W34)
sf.sum.mainW34 <- summary(t(sf.main.W34))
sf.sd.mainW34 <- apply(sf.main.W34, 1, sd)

atl.avg.mainW34 <- rowMeans(atl.main.W34)
atl.sum.mainW34 <- summary(t(atl.main.W34))
atl.sd.mainW34 <- apply(atl.main.W34, 1, sd)

# Black 35-44
sf.avg.mainB44 <- rowMeans(sf.main.B44)
sf.sum.mainB44 <- summary(t(sf.main.B44))
sf.sd.mainB44 <- apply(sf.main.B44, 1, sd)

atl.avg.mainB44 <- rowMeans(atl.main.B44)
atl.sum.mainB44 <- summary(t(atl.main.B44))
atl.sd.mainB44 <- apply(atl.main.B44, 1, sd)

# White 35-44
sf.avg.mainW44 <- rowMeans(sf.main.W44)
sf.sum.mainW44 <- summary(t(sf.main.W44))
sf.sd.mainW44 <- apply(sf.main.W44, 1, sd)

atl.avg.mainW44 <- rowMeans(atl.main.W44)
atl.sum.mainW44 <- summary(t(atl.main.W44))
atl.sd.mainW44 <- apply(atl.main.W44, 1, sd)

# Black 45-54
sf.avg.mainB54 <- rowMeans(sf.main.B54)
sf.sum.mainB54 <- summary(t(sf.main.B54))
sf.sd.mainB54 <- apply(sf.main.B54, 1, sd)

atl.avg.mainB54 <- rowMeans(atl.main.B54)
atl.sum.mainB54 <- summary(t(atl.main.B54))
atl.sd.mainB54 <- apply(atl.main.B54, 1, sd)

# White 45-54
sf.avg.mainW54 <- rowMeans(sf.main.W54)
sf.sum.mainW54 <- summary(t(sf.main.W54))
sf.sd.mainW54 <- apply(sf.main.W54, 1, sd)

atl.avg.mainW54 <- rowMeans(atl.main.W54)
atl.sum.mainW54 <- summary(t(atl.main.W54))
atl.sd.mainW54 <- apply(atl.main.W54, 1, sd)

# Black 55+
sf.avg.mainB64 <- rowMeans(sf.main.B64)
sf.sum.mainB64 <- summary(t(sf.main.B64))
sf.sd.mainB64 <- apply(sf.main.B64, 1, sd)

atl.avg.mainB64 <- rowMeans(atl.main.B64)
atl.sum.mainB64 <- summary(t(atl.main.B64))
atl.sd.mainB64 <- apply(atl.main.B64, 1, sd)



### Casual partnerships ###

# By race

# Black
matplot(sf.casl.B, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Casual - Black")
matplot(atl.casl.B, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Casual - Black")

# White
matplot(sf.casl.W, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Casual - White")
matplot(atl.casl.W, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Casual - White")

# By age

# 0-24
matplot(sf.casl.24, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Casual - <25 years")
matplot(atl.casl.24, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Casual - <25 years")

# 25-34
matplot(sf.casl.34, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Casual - 25-34 years")
matplot(atl.casl.34, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Casual - 25-34 years")

# 35-44
matplot(sf.casl.44, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Casual - 35-44 years")
matplot(atl.casl.44, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Casual - 35-44 years")

# 45-54
matplot(sf.casl.54, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Casual - 45-54 years")
matplot(atl.casl.54, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Casual - 45-54 years")

# 55+
matplot(sf.casl.64, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Casual - 55+ years")
matplot(atl.casl.64, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Casual - 55+ years")

# By race & age

# Black 0-24
matplot(sf.casl.B24, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Casual - Black & <25 years")
matplot(atl.casl.B24, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Casual - Black & <25 years")

# White 0-24
matplot(sf.casl.W24, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Casual - White & <25 years")
matplot(atl.casl.W24, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Casual - White & <25 years")

# Black 25-34
matplot(sf.casl.B34, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Casual - Black & 25-34 years")
matplot(atl.casl.B34, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Casual - Black & 25-34 years")

# White 25-34
matplot(sf.casl.W34, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Casual - White & 25-34 years")
matplot(atl.casl.W34, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Casual - White & 25-34 years")

# Black 35-44
matplot(sf.casl.B44, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Casual - Black & 35-44 years")
matplot(atl.casl.B44, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Casual - Black & 35-44 years")

# White 35-44
matplot(sf.casl.W44, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Casual - White & 35-44 years")
matplot(atl.casl.W44, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Casual - White & 35-44 years")

# Black 45-54
matplot(sf.casl.B54, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Casual - Black & 45-54 years")
matplot(atl.casl.B54, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Casual - Black & 45-54 years")

# White 45-54
matplot(sf.casl.W54, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Casual - White & 45-54 years")
matplot(atl.casl.W54, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Casual - White & 45-54 years")

# Black 55+
matplot(sf.casl.B64, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Casual - Black & 55+ years")
matplot(atl.casl.B64, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Casual - Black & 55+ years")

# White 55+
matplot(sf.casl.W64, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Casual - White & 55+ years")
matplot(atl.casl.W64, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Casual - White & 55+ years")


## Summary

# Overall
sf.casl.mean <- rowMeans(sf.casl)
sf.casl.sum <- summary(t(sf.casl))
sf.casl.sd <- apply(sf.casl, 1, sd)

write.csv(sf.casl.sum, file = "SF_Casual_Summary.csv")

atl.casl.mean <- rowMeans(atl.casl)
atl.casl.sum <- summary(t(atl.casl))
atl.casl.sd <- apply(atl.casl, 1, sd)

write.csv(atl.casl.sum, file = "ATL_Casual_Summary.csv")

# By race

# Black
sf.avg.caslB <- rowMeans(sf.casl.B)
sf.sum.caslB <- summary(t(sf.casl.B))
sf.sd.caslB <- apply(sf.casl.B, 1, sd)

write.csv(sf.sum.caslB, file = "SF_Casual_Black_Summary.csv")

atl.avg.caslB <- rowMeans(atl.casl.B)
atl.sum.caslB <- summary(t(atl.casl.B))
atl.sd.caslB <- apply(atl.casl.B, 1, sd)

write.csv(atl.sum.caslB, file = "ATL_Casual_Black_Summary.csv")

# White
sf.avg.caslW <- rowMeans(sf.casl.W)
sf.sum.caslW <- summary(t(sf.casl.W))
sf.sd.caslW <- apply(sf.casl.W, 1, sd)

write.csv(sf.sum.caslW, file = "SF_Casual_White_Summary.csv")

atl.avg.caslW <- rowMeans(atl.casl.W)
atl.sum.caslW <- summary(t(atl.casl.W))
atl.sd.caslW <- apply(atl.casl.W, 1, sd)

write.csv(atl.sum.caslW, file = "ATL_Casual_White_Summary.csv")

# By age

# 0-24
sf.avg.casl24 <- rowMeans(sf.casl.24)
sf.sum.casl24 <- summary(t(sf.casl.24))
sf.sd.casl24 <- apply(sf.casl.24, 1, sd)

write.csv(sf.sum.casl24, file = "SF_Casual_>25_Summary.csv")

atl.avg.casl24 <- rowMeans(atl.casl.24)
atl.sum.casl24 <- summary(t(atl.casl.24))
atl.sd.casl24 <- apply(atl.casl.24, 1, sd)

write.csv(atl.sum.casl24, file = "ATL_Casual_>25_Summary.csv")

# 25-34
sf.avg.casl34 <- rowMeans(sf.casl.34)
sf.sum.casl34 <- summary(t(sf.casl.34))
sf.sd.casl34 <- apply(sf.casl.34, 1, sd)

write.csv(sf.sum.casl34, file = "SF_Casual_24-34_Summary.csv")

atl.avg.casl34 <- rowMeans(atl.casl.34)
atl.sum.casl34 <- summary(t(atl.casl.34))
atl.sd.casl34 <- apply(atl.casl.34, 1, sd)

write.csv(atl.sum.casl34, file = "ATL_Casual_24-34_Summary.csv")

# 35-44
sf.avg.casl44 <- rowMeans(sf.casl.44)
sf.sum.casl44 <- summary(t(sf.casl.44))
sf.sd.casl44 <- apply(sf.casl.44, 1, sd)

write.csv(sf.sum.casl44, file = "SF_Casual_35-44_Summary.csv")

atl.avg.casl44 <- rowMeans(atl.casl.44)
atl.sum.casl44 <- summary(t(atl.casl.44))
atl.sd.casl44 <- apply(atl.casl.44, 1, sd)

write.csv(atl.sum.casl44, file = "ATL_Casual_35-44_Summary.csv")

# 45-54
sf.avg.casl54 <- rowMeans(sf.casl.54)
sf.sum.casl54 <- summary(t(sf.casl.54))
sf.sd.casl54 <- apply(sf.casl.54, 1, sd)

write.csv(sf.sum.casl54, file = "SF_Casual_45-54_Summary.csv")

atl.avg.casl54 <- rowMeans(atl.casl.54)
atl.sum.casl54 <- summary(t(atl.casl.54))
atl.sd.casl54 <- apply(atl.casl.54, 1, sd)

write.csv(atl.sum.casl54, file = "ATL_Casual_45-54_Summary.csv")

# 55+
sf.avg.casl65 <- rowMeans(sf.casl.64)
sf.sum.casl64 <- summary(t(sf.casl.64))
sf.sd.casl64 <- apply(sf.casl.64, 1, sd)

write.csv(sf.sum.casl64, file = "SF_Casual_>55_Summary.csv")

atl.avg.casl64 <- rowMeans(atl.casl.64)
atl.sum.casl64 <- summary(t(atl.casl.64))
atl.sd.casl64 <- apply(atl.casl.64, 1, sd)

write.csv(atl.sum.casl64, file = "ATL_Casual_>55_Summary.csv")

# By race & age

# Black 0-24
sf.avg.caslB24 <- rowMeans(sf.casl.B24)
sf.sum.caslB24 <- summary(t(sf.casl.B24))
sf.sd.caslB24 <- apply(sf.casl.B24, 1, sd)

write.csv(sf.sum.caslB24, file = "SF_Casual_Black_<25_Summary.csv")

atl.avg.caslB24 <- rowMeans(atl.casl.B24)
atl.sum.caslB24 <- summary(t(atl.casl.B24))
atl.sd.caslB24 <- apply(atl.casl.B24, 1, sd)

write.csv(atl.sum.caslB24, file = "ATL_Casual_Black_<25_Summary.csv")

# White 0-24
sf.avg.caslW24 <- rowMeans(sf.casl.W24)
sf.sum.caslW24 <- summary(t(sf.casl.W24))
sf.sd.caslW24 <- apply(sf.casl.W24, 1, sd)

write.csv(sf.sum.caslW24, file = "SF_Casual_White_<25_Summary.csv")

atl.avg.caslW24 <- rowMeans(atl.casl.W24)
atl.sum.caslW24 <- summary(t(atl.casl.W24))
atl.sd.caslW24 <- apply(atl.casl.W24, 1, sd)

write.csv(atl.sum.caslW24, file = "ATL_Casual_White_<25_Summary.csv")

# Black 25-34
sf.avg.caslB34 <- rowMeans(sf.casl.B34)
sf.sum.caslB34 <- summary(t(sf.casl.B34))
sf.sd.caslB34 <- apply(sf.casl.B34, 1, sd)

write.csv(sf.sum.caslB34, file = "SF_Casual_Black_25-34_Summary.csv")

atl.avg.caslB34 <- rowMeans(atl.casl.B34)
atl.sum.caslB34 <- summary(t(atl.casl.B34))
atl.sd.caslB34 <- apply(atl.casl.B34, 1, sd)

write.csv(atl.sum.caslB34, file = "ATL_Casual_Black_25-34_Summary.csv")

# White 25-34
sf.avg.caslW34 <- rowMeans(sf.casl.W34)
sf.sum.caslW34 <- summary(t(sf.casl.W34))
sf.sd.caslW34 <- apply(sf.casl.W34, 1, sd)

write.csv(sf.sum.caslW34, file = "SF_Casual_White_25-34_Summary.csv")

atl.avg.caslW34 <- rowMeans(atl.casl.W34)
atl.sum.caslW34 <- summary(t(atl.casl.W34))
atl.sd.caslW34 <- apply(atl.casl.W34, 1, sd)

write.csv(atl.sum.caslW34, file = "ATL_Casual_White_25-34_Summary.csv")

# Black 35-44
sf.avg.caslB44 <- rowMeans(sf.casl.B44)
sf.sum.caslB44 <- summary(t(sf.casl.B44))
sf.sd.caslB44 <- apply(sf.casl.B44, 1, sd)

write.csv(sf.sum.caslB44, file = "SF_Casual_Black_35-44_Summary.csv")

atl.avg.caslB44 <- rowMeans(atl.casl.B44)
atl.sum.caslB44 <- summary(t(atl.casl.B44))
atl.sd.caslB44 <- apply(atl.casl.B44, 1, sd)

write.csv(atl.sum.caslB44, file = "ATL_Casual_Black_35-44_Summary.csv")

# White 35-44
sf.avg.caslW44 <- rowMeans(sf.casl.W44)
sf.sum.caslW44 <- summary(t(sf.casl.W44))
sf.sd.caslW44 <- apply(sf.casl.W44, 1, sd)

write.csv(sf.sum.caslW44, file = "SF_Casual_White_35-44_Summary.csv")

atl.avg.caslW44 <- rowMeans(atl.casl.W44)
atl.sum.caslW44 <- summary(t(atl.casl.W44))
atl.sd.caslW44 <- apply(atl.casl.W44, 1, sd)

write.csv(atl.sum.caslW44, file = "ATL_Casual_White_35-44_Summary.csv")

# Black 45-54
sf.avg.caslB54 <- rowMeans(sf.casl.B54)
sf.sum.caslB54 <- summary(t(sf.casl.B54))
sf.sd.caslB54 <- apply(sf.casl.B54, 1, sd)

write.csv(sf.sum.caslB54, file = "SF_Casual_Black_45-54_Summary.csv")

atl.avg.caslB54 <- rowMeans(atl.casl.B54)
atl.sum.caslB54 <- summary(t(atl.casl.B54))
atl.sd.caslB54 <- apply(atl.casl.B54, 1, sd)

write.csv(atl.sum.caslB54, file = "ATL_Casual_Black_45-54_Summary.csv")

# White 45-54
sf.avg.caslW54 <- rowMeans(sf.casl.W54)
sf.sum.caslW54 <- summary(t(sf.casl.W54))
sf.sd.caslW54 <- apply(sf.casl.W54, 1, sd)

write.csv(sf.sum.caslW54, file = "SF_Casual_White_45-54_Summary.csv")

atl.avg.caslW54 <- rowMeans(atl.casl.W54)
atl.sum.caslW54 <- summary(t(atl.casl.W54))
atl.sd.caslW54 <- apply(atl.casl.W54, 1, sd)

write.csv(atl.sum.caslW54, file = "ATL_Casual_White_45-54_Summary.csv")

# Black 55+
sf.avg.caslB64 <- rowMeans(sf.casl.B64)
sf.sum.caslB64 <- summary(t(sf.casl.B64))
sf.sd.caslB64 <- apply(sf.casl.B64, 1, sd)

write.csv(sf.sum.caslB64, file = "SF_Casual_Black_>55_Summary.csv")

atl.avg.caslB64 <- rowMeans(atl.casl.B64)
atl.sum.caslB64 <- summary(t(atl.casl.B64))
atl.sd.caslB64 <- apply(atl.casl.B64, 1, sd)

write.csv(atl.sum.caslB64, file = "ATL_Casual_Black_>55_Summary.csv")

# White 55+
sf.avg.caslW64 <- rowMeans(sf.casl.W64)
sf.sum.caslW64 <- summary(t(sf.casl.W64))
sf.sd.caslW64 <- apply(sf.casl.W64, 1, sd)

write.csv(sf.sum.caslW64, file = "SF_Casual_White_>55_Summary.csv")

atl.avg.caslW64 <- rowMeans(atl.casl.W64)
atl.sum.caslW64 <- summary(t(atl.casl.W64))
atl.sd.caslW64 <- apply(atl.casl.W64, 1, sd)

write.csv(atl.sum.caslW64, file = "ATL_Casual_White_>55_Summary.csv")


### Instantaneous partnerships ###

## Plots

# Overall
matplot(samp.sf.inst, type = "l", ylim = c(0, 10000), xlab = "Time Period",
        ylab = "Forward Reachable Path", main = "San Francisco One-Off Partnerships")

matplot(samp.atl.inst, type = "l", ylim = c(0, 10000), xlab = "Time Period",
        ylab = "Forward Reachable Path", main = "Atlanta One-Off Partnerships")


# By race

# Black
matplot(sf.inst.B, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF One-Offs - Black")
matplot(atl.inst.B, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL One-Offs - Black")

# White
matplot(sf.inst.W, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF One-Offs - White")
matplot(atl.inst.W, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL One-Offs - White")

# By age

# 0-24
matplot(sf.inst.24, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Inst - <25 years")
matplot(atl.inst.24, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Inst - <25 years")

# 25-34
matplot(sf.inst.34, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Inst - 25-34 years")
matplot(atl.inst.34, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Inst - 25-34 years")

# 35-44
matplot(sf.inst.44, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Inst - 35-44 years")
matplot(atl.inst.44, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Inst - 35-44 years")

# 45-54
matplot(sf.inst.54, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Inst - 45-54 years")
matplot(atl.inst.54, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Inst - 45-54 years")

# 55+
matplot(sf.inst.64, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Inst - 55+ years")
matplot(atl.inst.64, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Inst - 55+ years")

# By age & race

# Black 0-24
matplot(sf.inst.B24, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Inst - Black & <25 years")
matplot(atl.inst.B24, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Inst - Black & <25 years")

# White 0-24
matplot(sf.inst.W24, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Inst - White & <25 years")
matplot(atl.inst.W24, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Inst - White & <25 years")

# Black 25-34
matplot(sf.inst.B34, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Inst - Black & 25-34 years")
matplot(atl.inst.B34, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Inst - Black & 25-34 years")

# White 25-34
matplot(sf.inst.W34, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Inst - White & 25-34 years")
matplot(atl.inst.W34, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Inst - White & 25-34 years")

# Black 35-44
matplot(sf.inst.B44, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Inst - Black & 35-44 years")
matplot(atl.inst.B44, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Inst - Black & 35-44 years")

# White 35-44
matplot(sf.inst.W44, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Inst - White & 35-44 years")
matplot(atl.inst.W44, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Inst - White & 35-44 years")

# Black 45-54
matplot(sf.inst.B54, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Inst - Black & 45-54 years")
matplot(atl.inst.B54, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Inst - Black & 45-54 years")

# White 45-54
matplot(sf.inst.W54, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Inst - White & 45-54 years")
matplot(atl.inst.W54, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Inst - White & 45-54 years")

# Black 55+
matplot(sf.inst.B64, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Inst - Black & 55+ years")
matplot(atl.inst.B64, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Inst - Black & 55+ years")

# White 55+
matplot(sf.inst.W64, type = "l", xlab = "time sequence", ylab = "FRP", main = "SF Inst - White & 55+ years")
matplot(atl.inst.W64, type = "l", xlab = "time sequence", ylab = "FRP", main = "ATL Inst - White & 55+ years")


## Summary

# Overall
sf.inst.mean <- rowMeans(sf.inst)
sf.inst.sum <- summary(t(sf.inst))
sf.inst.sd <- apply(sf.inst, 1, sd)

write.csv(sf.inst.sum, file = "SF_One-Offs_Summary.csv")

atl.inst.mean <- rowMeans(atl.inst)
atl.inst.sum <- summary(t(atl.inst))
atl.inst.sd <- apply(atl.inst, 1, sd)

write.csv(atl.inst.sum, file = "ATL_One-Offs_Summary.csv")

# By race

# Black
sf.avg.instB <- rowMeans(sf.inst.B)
sf.sum.instB <- summary(t(sf.inst.B))
sf.sd.instB <- apply(sf.inst.B, 1, sd)

write.csv(sf.sum.instB, file = "SF_One-Offs_Black_Summary.csv")

atl.avg.instB <- rowMeans(atl.inst.B)
atl.sum.instB <- summary(t(atl.inst.B))
atl.sd.instB <- apply(atl.inst.B, 1, sd)

write.csv(atl.sum.instB, file = "ATL_One-Offs_Black_Summary.csv")

# White
sf.avg.instW <- rowMeans(sf.inst.W)
sf.sum.instW <- summary(t(sf.inst.W))
sf.sd.instW <- apply(sf.inst.W, 1, sd)

write.csv(sf.sum.instW, file = "SF_One-Offs_White_Summary.csv")

atl.avg.instW <- rowMeans(atl.inst.W)
atl.sum.instW <- summary(t(atl.inst.W))
atl.sd.instW <- apply(atl.inst.W, 1, sd)

write.csv(atl.sum.instW, file = "ATL_One-Offs_White_Summary.csv")

# By age

# 0-24
sf.avg.inst24 <- rowMeans(sf.inst.24)
sf.sum.inst24 <- summary(t(sf.inst.24))
sf.sd.inst24 <- apply(sf.inst.24, 1, sd)

write.csv(sf.sum.inst24, file = "SF_One-Offs_<25_Summary.csv")

atl.avg.inst24 <- rowMeans(atl.inst.24)
atl.sum.inst24 <- summary(t(atl.inst.24))
atl.sd.inst24 <- apply(atl.inst.24, 1, sd)

write.csv(atl.sum.inst24, file = "ATL_One-Offs_<25_Summary.csv")

# 25-34
sf.avg.inst34 <- rowMeans(sf.inst.34)
sf.sum.inst34 <- summary(t(sf.inst.34))
sf.sd.inst34 <- apply(sf.inst.34, 1, sd)

write.csv(sf.sum.inst34, file = "SF_One-Offs_25-34_Summary.csv")

atl.avg.inst34 <- rowMeans(atl.inst.34)
atl.sum.inst34 <- summary(t(atl.inst.34))
atl.sd.inst34 <- apply(atl.inst.34, 1, sd)

write.csv(atl.sum.inst34, file = "ATL_One-Offs_25-34_Summary.csv")

# 35-44
sf.avg.inst44 <- rowMeans(sf.inst.44)
sf.sum.inst44 <- summary(t(sf.inst.44))
sf.sd.inst44 <- apply(sf.inst.44, 1, sd)

write.csv(sf.sum.inst44, file = "SF_One-Offs_35-44_Summary.csv")

atl.avg.inst44 <- rowMeans(atl.inst.44)
atl.sum.inst44 <- summary(t(atl.inst.44))
atl.sd.inst44 <- apply(atl.inst.44, 1, sd)

write.csv(atl.sum.inst44, file = "ATL_One-Offs_35-44_Summary.csv")

# 45-54
sf.avg.inst54 <- rowMeans(sf.inst.54)
sf.sum.inst54 <- summary(t(sf.inst.54))
sf.sd.inst54 <- apply(sf.inst.54, 1, sd)

write.csv(sf.sum.inst54, file = "SF_One-Offs_45-54_Summary.csv")

atl.avg.inst54 <- rowMeans(atl.inst.54)
atl.sum.inst54 <- summary(t(atl.inst.54))
atl.sd.inst54 <- apply(atl.inst.54, 1, sd)

write.csv(atl.sum.inst54, file = "ATL_One-Offs_45-54_Summary.csv")

# 55+
sf.avg.inst64 <- rowMeans(sf.inst.64)
sf.sum.inst64 <- summary(t(sf.inst.64))
sf.sd.inst64 <- apply(sf.inst.64, 1, sd)

write.csv(sf.sum.inst64, file = "SF_One-Offs_>55_Summary.csv")

atl.avg.inst64 <- rowMeans(atl.inst.64)
atl.sum.inst64 <- summary(t(atl.inst.64))
atl.sd.inst64 <- apply(atl.inst.64, 1, sd)

write.csv(atl.sum.inst64, file = "ATL_One-Offs_>55_Summary.csv")

# By race & age

# Black 0-24
sf.avg.instB24 <- rowMeans(sf.inst.B24)
sf.sum.insB24 <- summary(t(sf.inst.B24))
sf.sd.instB24 <- apply(sf.inst.B24, 1, sd)

write.csv(sf.sum.insB24, file = "SF_One-Offs_Black_<25_Summary.csv")

atl.avg.instB24 <- rowMeans(atl.inst.B24)
atl.sum.instB24 <- summary(t(atl.inst.B24))
atl.sd.instB24 <- apply(atl.inst.B24, 1, sd)

write.csv(atl.sum.instB24, file = "ATL_One-Offs_Black_<25_Summary.csv")

# White 0-24
sf.avg.instW24 <- rowMeans(sf.inst.W24)
sf.sum.insW24 <- summary(t(sf.inst.W24))
sf.sd.instW24 <- apply(sf.inst.W24, 1, sd)

write.csv(sf.sum.insW24, file = "SF_One-Offs_White_<25_Summary.csv")

atl.avg.instW24 <- rowMeans(atl.inst.W24)
atl.sum.instW24 <- summary(t(atl.inst.W24))
atl.sd.instW24 <- apply(atl.inst.W24, 1, sd)

write.csv(atl.sum.instW24, file = "ATL_One-Offs_White_<25_Summary.csv")

# Black 25-34
sf.avg.instB34 <- rowMeans(sf.inst.B34)
sf.sum.insB34 <- summary(t(sf.inst.B34))
sf.sd.instB34 <- apply(sf.inst.B34, 1, sd)

write.csv(sf.sum.insB34, file = "SF_One-Offs_Black_25-34_Summary.csv")

atl.avg.instB34 <- rowMeans(atl.inst.B34)
atl.sum.instB34 <- summary(t(atl.inst.B34))
atl.sd.instB34 <- apply(atl.inst.B34, 1, sd)

write.csv(atl.sum.instB34, file = "ATL_One-Offs_Black_25-34_Summary.csv")

# White 25-34
sf.avg.instW34 <- rowMeans(sf.inst.W34)
sf.sum.insW34 <- summary(t(sf.inst.W34))
sf.sd.instW34 <- apply(sf.inst.W34, 1, sd)

write.csv(sf.sum.insW34, file = "SF_One-Offs_White_25-34_Summary.csv")

atl.avg.instW34 <- rowMeans(atl.inst.W34)
atl.sum.instW34 <- summary(t(atl.inst.W34))
atl.sd.instW34 <- apply(atl.inst.W34, 1, sd)

write.csv(atl.sum.instW34, file = "ATL_One-Offs_White_25-34_Summary.csv")

# Black 35-44
sf.avg.instB44 <- rowMeans(sf.inst.B44)
sf.sum.insB44 <- summary(t(sf.inst.B44))
sf.sd.instB44 <- apply(sf.inst.B44, 1, sd)

write.csv(sf.sum.insB44, file = "SF_One-Offs_Black_35-44_Summary.csv")

atl.avg.instB44 <- rowMeans(atl.inst.B44)
atl.sum.instB44 <- summary(t(atl.inst.B44))
atl.sd.instB44 <- apply(atl.inst.B44, 1, sd)

write.csv(atl.sum.instB44, file = "ATL_One-Offs_Black_35-44_Summary.csv")

# White 35-44
sf.avg.instW44 <- rowMeans(sf.inst.W44)
sf.sum.insW44 <- summary(t(sf.inst.W44))
sf.sd.instW44 <- apply(sf.inst.W44, 1, sd)

write.csv(sf.sum.insW44, file = "SF_One-Offs_White_35-44_Summary.csv")

atl.avg.instW44 <- rowMeans(atl.inst.W44)
atl.sum.instW44 <- summary(t(atl.inst.W44))
atl.sd.instW44 <- apply(atl.inst.W44, 1, sd)

write.csv(atl.sum.instW44, file = "ATL_One-Offs_White_35-44_Summary.csv")

# Black 45-54
sf.avg.instB54 <- rowMeans(sf.inst.B54)
sf.sum.insB54 <- summary(t(sf.inst.B54))
sf.sd.instB54 <- apply(sf.inst.B54, 1, sd)

write.csv(sf.sum.insB54, file = "SF_One-Offs_Black_45-54_Summary.csv")

atl.avg.instB54 <- rowMeans(atl.inst.B54)
atl.sum.instB54 <- summary(t(atl.inst.B54))
atl.sd.instB54 <- apply(atl.inst.B54, 1, sd)

write.csv(atl.sum.instB54, file = "ATL_One-Offs_Black_45-54_Summary.csv")

# White 45-54
sf.avg.instW54 <- rowMeans(sf.inst.W54)
sf.sum.insW54 <- summary(t(sf.inst.W54))
sf.sd.instW54 <- apply(sf.inst.W54, 1, sd)

write.csv(sf.sum.insW54, file = "SF_One-Offs_White_45-54_Summary.csv")

atl.avg.instW54 <- rowMeans(atl.inst.W54)
atl.sum.instW54 <- summary(t(atl.inst.W54))
atl.sd.instW54 <- apply(atl.inst.W54, 1, sd)

write.csv(atl.sum.instW54, file = "ATL_One-Offs_White_45-54_Summary.csv")

# Black 55+
sf.avg.instB64 <- rowMeans(sf.inst.B64)
sf.sum.insB64 <- summary(t(sf.inst.B64))
sf.sd.instB64 <- apply(sf.inst.B64, 1, sd)

write.csv(sf.sum.insB64, file = "SF_One-Offs_Black_>55_Summary.csv")

atl.avg.instB64 <- rowMeans(atl.inst.B64)
atl.sum.instB64 <- summary(t(atl.inst.B64))
atl.sd.instB64 <- apply(atl.inst.B64, 1, sd)

write.csv(atl.sum.instB64, file = "ATL_One-Offs_Black_>55_Summary.csv")

# White 55+
sf.avg.instW64 <- rowMeans(sf.inst.W64)
sf.sum.insW64 <- summary(t(sf.inst.W64))
sf.sd.instW64 <- apply(sf.inst.W64, 1, sd)

write.csv(sf.sum.insW64, file = "SF_One-Offs_White_>55_Summary.csv")

atl.avg.instW64 <- rowMeans(atl.inst.W64)
atl.sum.instW64 <- summary(t(atl.inst.W64))
atl.sd.instW64 <- apply(atl.inst.W64, 1, sd)

write.csv(atl.sum.instW64, file = "ATL_One-Offs_White_>55_Summary.csv")

## Plots for CROI

par(mfrow = c(2, 3), oma = c(2, 0, 2, 0), xpd = NA)

# Main SF
matplot(x = seq(1, 260, 8), samp.sf.main, type = "l", lty = 1, xlab = "Week",
        ylab = "Foward Reachable Path", main = "San Francisco Main Partnerships")

# Casual SF
matplot(x = seq(1, 260, 8), samp.sf.casl, type = "l", lty = 1, ylim = c(0, 10000), xlab = "Week",
        ylab = "Foward Reachable Path", main = "San Francisco Casual Partnerships")

# Inst SF
matplot(x = seq(1, 260, 8), samp.sf.inst, type = "l", lty = 1, ylim = c(0, 10000), xlab = "Week",
        ylab = "Foward Reachable Path", main = "San Francisco One-Time Partnerships")

# Main ATL
matplot(x = seq(1, 260, 8), samp.atl.main, type = "l",  lty = 1, ylim = c(0, 20), xlab = "Week",
        ylab = "Foward Reachable Path", main = "Atlanta Main Partnerships")

# Casual ATL
matplot(x = seq(1, 260, 8), samp.atl.casl, type = "l", lty = 1, ylim = c(0, 10000), xlab = "Week",
        ylab = "Foward Reachable Path", main = "Atlanta Casual Partnerships")

# Inst ATl
matplot(x = seq(1, 260, 8), samp.atl.inst, type = "l", lty = 1, ylim = c(0, 10000), xlab = "Week",
        ylab = "Foward Reachable Path", main = "Atlanta One-Time Partnerships")

title("Distribution of 5-Year Foward Reachable Paths by Partnership Type", outer = TRUE)

#legend(x = -0.5,  y = 3.5, c("Forward reachable Paths city by partnership type"), xpd = TRUE, horiz = TRUE,
#       inset = 0, bty = "n", cex = 1.2)

#legend("center", c("IM", "IBD", "1R", "2R"), xpd = TRUE, horiz = TRUE, inset = c(0,
#             0), bty = "n", pch = c(4, 2, 15, 19), col = 1:4, cex = 2)




























