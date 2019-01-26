## TSNA Analysis
## 1/23/2019

library("tsna")
library("networkDynamicData")
suppressMessages(library("EpiModel"))
library("scales")
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



# 1. Extract IDs ----------------------------------------------------------

### Race ###

# Black
sf.id.b <- which(get.vertex.attribute(sf.m, "race") == "B")
atl.id.b <- which(get.vertex.attribute(atl.m, "race") == "B")

# White
sf.id.w <- which(get.vertex.attribute(sf.m, "race") == "W")
atl.id.w <- which(get.vertex.attribute(atl.m, "race") == "W")


### Age ###

# 15-24
sf.id.24 <- which(get.vertex.attribute(sf.m, "age.grp") == "1")
atl.id.24 <- which(get.vertex.attribute(atl.m, "age.grp") == "1")

# 25-34
sf.id.34 <- which(get.vertex.attribute(sf.m, "age.grp") == "2")
atl.id.34 <- which(get.vertex.attribute(atl.m, "age.grp") == "2")

# 35-44
sf.id.44 <- which(get.vertex.attribute(sf.m, "age.grp") == "3")
atl.id.44 <- which(get.vertex.attribute(atl.m, "age.grp") == "3")

# 45-54
sf.id.54 <- which(get.vertex.attribute(sf.m, "age.grp") == "4")
atl.id.54 <- which(get.vertex.attribute(atl.m, "age.grp") == "4")

# 55-64
sf.id.64 <- which(get.vertex.attribute(sf.m, "age.grp") == "5")
atl.id.64 <- which(get.vertex.attribute(atl.m, "age.grp") == "5")


### Race & Age ###

# Black 0-24
sf.id.b24 <- which(get.vertex.attribute(sf.m, "race") == "B" & 
                     get.vertex.attribute(sf.m, "age.grp") == "1")
atl.id.b24 <- which(get.vertex.attribute(atl.m, "race") == "B" & 
                      get.vertex.attribute(atl.m, "age.grp") == "1")

# White 0-24
sf.id.w24 <- which(get.vertex.attribute(sf.m, "race") == "W" & 
                     get.vertex.attribute(sf.m, "age.grp") == "1")
atl.id.w24 <- which(get.vertex.attribute(atl.m, "race") == "W" & 
                      get.vertex.attribute(atl.m, "age.grp") == "1")


# Black 25-34
sf.id.b34 <- which(get.vertex.attribute(sf.m, "race") == "B" & 
                     get.vertex.attribute(sf.m, "age.grp") == "2")
atl.id.b34 <- which(get.vertex.attribute(atl.m, "race") == "B" & 
                      get.vertex.attribute(atl.m, "age.grp") == "2")

# White 25-34
sf.id.w34 <- which(get.vertex.attribute(sf.m, "race") == "W" & 
                     get.vertex.attribute(sf.m, "age.grp") == "2")
atl.id.w34 <- which(get.vertex.attribute(atl.m, "race") == "W" & 
                      get.vertex.attribute(atl.m, "age.grp") == "2")

# Black 35-44
sf.id.b44 <- which(get.vertex.attribute(sf.m, "race") == "B" & 
                     get.vertex.attribute(sf.m, "age.grp") == "3")
atl.id.b44 <- which(get.vertex.attribute(atl.m, "race") == "B" & 
                      get.vertex.attribute(atl.m, "age.grp") == "3")

# White 35-44
sf.id.w44 <- which(get.vertex.attribute(sf.m, "race") == "W" & 
                     get.vertex.attribute(sf.m, "age.grp") == "3")
atl.id.w44 <- which(get.vertex.attribute(atl.m, "race") == "W" & 
                      get.vertex.attribute(atl.m, "age.grp") == "3")

# Black 45-54
sf.id.b54 <- which(get.vertex.attribute(sf.m, "race") == "B" & 
                     get.vertex.attribute(sf.m, "age.grp") == "4")
atl.id.b54 <- which(get.vertex.attribute(atl.m, "race") == "B" & 
                      get.vertex.attribute(atl.m, "age.grp") == "4")

# White 45-54
sf.id.w54 <- which(get.vertex.attribute(sf.m, "race") == "W" & 
                     get.vertex.attribute(sf.m, "age.grp") == "4")
atl.id.w54 <- which(get.vertex.attribute(atl.m, "race") == "W" & 
                      get.vertex.attribute(atl.m, "age.grp") == "4")

# Black 55+
sf.id.b64 <- which(get.vertex.attribute(sf.m, "race") == "B" & 
                     get.vertex.attribute(sf.m, "age.grp") == "5")
atl.id.b64 <- which(get.vertex.attribute(atl.m, "race") == "B" & 
                      get.vertex.attribute(atl.m, "age.grp") == "5")

# White 55+
sf.id.w64 <- which(get.vertex.attribute(sf.m, "race") == "W" & 
                     get.vertex.attribute(sf.m, "age.grp") == "5")
atl.id.w64 <- which(get.vertex.attribute(atl.m, "race") == "W" & 
                      get.vertex.attribute(atl.m, "age.grp") == "5")



# 2. Outcome Data ---------------------------------------------------------


### ALL ptypes ###

## SF
# Load in data
sf.all <- load(file = "sfo.all.1.rda")
sf.all <- out
sf.all <- as.data.frame(sf.all)

# Subset by outcome
sf.a.frp <- subset(sf.all, select = frp.1:frp.10000)
sf.a.tdist <- subset(sf.all, select = medtdist.1:medtdist.10000)
sf.a.gstep <- subset(sf.all, select = medgeod.1:medgeod.10000)
sf.a.deg <- subset(sf.all, select = degree.1:degree.10000)
sf.a.cumldeg <- subset(sf.all, select = cumldegree.1:cumldegree.10000)
# sf.a.bcent <- subset(sf.all, select = bcent.1:bcent.10000)

# proportion
sfa.frp.prop <- sf.a.frp/10000

## ATL
# Load in data
atl.all <- load(file = "atl.all.1.rda")
atl.all <- out
atl.all <- as.data.frame(atl.all)

# Subset by outcome
atl.a.frp <- subset(atl.all, select = frp.1:frp.10000)
atl.a.tdist <- subset(atl.all, select = medtdist.1:medtdist.10000)
atl.a.gstep <- subset(atl.all, select = medgeod.1:medgeod.10000)
atl.a.deg <- subset(atl.all, select = degree.1:degree.10000)
atl.a.cumldeg <- subset(atl.all, select = cumldegree.1:cumldegree.10000)
atl.a.bcent <- subset(atl.all, select = bcent.1:bcent.10000)

# proportion
atla.frp.prop <- atl.a.frp/10000

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

# proportion
sfm.frp.prop <- sf.m.frp/10000

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

# proportion
atlm.frp.prop <- atl.m.frp/10000

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

# proportion
sfc.frp.prop <- sf.c.frp/10000

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

# proportion
atlc.frp.prop <- atl.c.frp/10000

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

# proportion
sfi.frp.prop <- sf.i.frp/10000

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

# proportion
atli.frp.prop <- atl.i.frp/10000


# 3. Validation -----------------------------------------------------------

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


# 4a. Race Outcomes -------------------------------------------------------

### Black ###

## SF

# All ptypes
sfa.frp.b <- sf.a.frp[sf.id.b]
# proportion
sfa.frpb.prop <- sfa.frp.b/10000

# Main
sfm.frp.b <- sf.m.frp[sf.id.b]
sfm.tdist.b <- sf.m.tdist[sf.id.b]
sfm.gstep.b <- sf.m.gstep[sf.id.b]
sfm.deg.b <- sf.m.deg[sf.id.b]
sfm.cumldeg.b <- sf.m.cumldeg[sf.id.b]
sfm.bcent.b <- sf.m.bcent[sf.id.b]

# proportion
sfm.frpb.prop <- sfm.frp.b/10000

# Casual
sfc.frp.b <- sf.c.frp[sf.id.b]
sfc.tdist.b <- sf.c.tdist[sf.id.b]
sfc.gstep.b <- sf.c.gstep[sf.id.b]
sfc.deg.b <- sf.c.deg[sf.id.b]
sfc.cumldeg.b <- sf.c.cumldeg[sf.id.b]
sfc.bcent.b <- sf.c.bcent[sf.id.b]

# proportion
sfc.frpb.prop <- sfc.frp.b/10000

# Inst
sfi.frp.b <- sf.i.frp[sf.id.b]
sfi.tdist.b <- sf.i.tdist[sf.id.b]
sfi.gstep.b <- sf.i.gstep[sf.id.b]
sfi.deg.b <- sf.i.deg[sf.id.b]
sfi.cumldeg.b <- sf.i.cumldeg[sf.id.b]
sfi.bcent.b <- sf.i.bcent[sf.id.b]

# proportion
sfi.frpb.prop <- sfi.frp.b/10000

## ATL

# All ptypes
atla.frp.b <- atl.a.frp[atl.id.b]

# proportion
atla.frpb.prop <- atla.frp.b/10000

# Main
atlm.frp.b <- atl.m.frp[atl.id.b]
atlm.tdist.b <- atl.m.tdist[atl.id.b]
atlm.gstep.b <- atl.m.gstep[atl.id.b]
atlm.deg.b <- atl.m.deg[atl.id.b]
atlm.cumldeg.b <- atl.m.cumldeg[atl.id.b]
atlm.bcent.b <- atl.m.bcent[atl.id.b]

# proportion
atlm.frpb.prop <- atlm.frp.b/10000

# Casual
atlc.frp.b <- atl.c.frp[atl.id.b]
atlc.tdist.b <- atl.c.tdist[atl.id.b]
atlc.gstep.b <- atl.c.gstep[atl.id.b]
atlc.deg.b <- atl.c.deg[atl.id.b]
atlc.cumldeg.b <- atl.c.cumldeg[atl.id.b]
atlc.bcent.b <- atl.c.bcent[atl.id.b]

# proportion
atlc.frpb.prop <- atlc.frp.b/10000

# Inst
atli.frp.b <- atl.i.frp[atl.id.b]
atli.tdist.b <- atl.i.tdist[atl.id.b]
atli.gstep.b <- atl.i.gstep[atl.id.b]
atli.deg.b <- atl.i.deg[atl.id.b]
atli.cumldeg.b <- atl.i.cumldeg[atl.id.b]
atli.bcent.b <- atl.i.bcent[atl.id.b]

# proportion
atli.frpb.prop <- atli.frp.b/10000


### White ###

# All ptypes
sfa.frp.w <- sf.a.frp[sf.id.w]

# proportion
sfa.frpw.prop <- sfa.frp.w/10000

# Main
sfm.frp.w <- sf.m.frp[sf.id.w]
sfm.tdist.w <- sf.m.tdist[sf.id.w]
sfm.gstep.w <- sf.m.gstep[sf.id.w]
sfm.deg.w <- sf.m.deg[sf.id.w]
sfm.cumldeg.w <- sf.m.cumldeg[sf.id.w]
sfm.bcent.w <- sf.m.bcent[sf.id.w]

# proportion
sfm.frpw.prop <- sfm.frp.w/10000

# Casual
sfc.frp.w <- sf.c.frp[sf.id.w]
sfc.tdist.w <- sf.c.tdist[sf.id.w]
sfc.gstep.w <- sf.c.gstep[sf.id.w]
sfc.deg.w <- sf.c.deg[sf.id.w]
sfc.cumldeg.w <- sf.c.cumldeg[sf.id.w]
sfc.bcent.w <- sf.c.bcent[sf.id.w]

# proportion
sfc.frpw.prop <- sfc.frp.w/10000

# Inst
sfi.frp.w <- sf.i.frp[sf.id.w]
sfi.tdist.w <- sf.i.tdist[sf.id.w]
sfi.gstep.w <- sf.i.gstep[sf.id.w]
sfi.deg.w <- sf.i.deg[sf.id.w]
sfi.cumldeg.w <- sf.i.cumldeg[sf.id.w]
sfi.bcent.w <- sf.i.bcent[sf.id.w]

# proportion
sfi.frpw.prop <- sfi.frp.w/10000

## ATL

# All ptypes
atla.frp.w <- atl.a.frp[atl.id.w]

# proportion
atla.frpw.prop <- atla.frp.w/10000

# Main
atlm.frp.w <- atl.m.frp[atl.id.w]
atlm.tdist.w <- atl.m.tdist[atl.id.w]
atlm.gstep.w <- atl.m.gstep[atl.id.w]
atlm.deg.w <- atl.m.deg[atl.id.w]
atlm.cumldeg.w <- atl.m.cumldeg[atl.id.w]
atlm.bcent.w <- atl.m.bcent[atl.id.w]

# proportion
atlm.frpw.prop <- atlm.frp.w/10000

# Casual
atlc.frp.w <- atl.c.frp[atl.id.w]
atlc.tdist.w <- atl.c.tdist[atl.id.w]
atlc.gstep.w <- atl.c.gstep[atl.id.w]
atlc.deg.w <- atl.c.deg[atl.id.w]
atlc.cumldeg.w <- atl.c.cumldeg[atl.id.w]
atlc.bcent.w <- atl.c.bcent[atl.id.w]

# proportion
atlc.frpw.prop <- atlc.frp.w/10000

# Inst
atli.frp.w <- atl.i.frp[atl.id.w]
atli.tdist.w <- atl.i.tdist[atl.id.w]
atli.gstep.w <- atl.i.gstep[atl.id.w]
atli.deg.w <- atl.i.deg[atl.id.w]
atli.cumldeg.w <- atl.i.cumldeg[atl.id.w]
atli.bcent.w <- atl.i.bcent[atl.id.w]

# proportion
atli.frpw.prop <- atli.frp.w/10000


# 4b. Age Outcomes --------------------------------------------------------

### Age 0-24 ###

## SF

# All ptypes
sfa.frp.24 <- sf.a.frp[sf.id.24]

# proportion
sfa.frp24.prop <- sfa.frp.24/10000

# Main
sfm.frp.24 <- sf.m.frp[sf.id.24]
sfm.tdist.24 <- sf.m.tdist[sf.id.24]
sfm.gstep.24 <- sf.m.gstep[sf.id.24]
sfm.deg.24 <- sf.m.deg[sf.id.24]
sfm.cumldeg.24 <- sf.m.cumldeg[sf.id.24]
sfm.bcent.24 <- sf.m.bcent[sf.id.24]

# proportion
sfm.frp24.prop <- sfm.frp.24/10000

# Casual
sfc.frp.24 <- sf.c.frp[sf.id.24]
sfc.tdist.24 <- sf.c.tdist[sf.id.24]
sfc.gstep.24 <- sf.c.gstep[sf.id.24]
sfc.deg.24 <- sf.c.deg[sf.id.24]
sfc.cumldeg.24 <- sf.c.cumldeg[sf.id.24]
sfc.bcent.24 <- sf.c.bcent[sf.id.24]

# proportion
sfc.frp24.prop <- sfc.frp.24/10000

# Inst
sfi.frp.24 <- sf.i.frp[sf.id.24]
sfi.tdist.24 <- sf.i.tdist[sf.id.24]
sfi.gstep.24 <- sf.i.gstep[sf.id.24]
sfi.deg.24 <- sf.i.deg[sf.id.24]
sfi.cumldeg.24 <- sf.i.cumldeg[sf.id.24]
sfi.bcent.24 <- sf.i.bcent[sf.id.24]

# proportion
sfi.frp24.prop <- sfi.frp.24/10000

## ATL

# All ptypes
atla.frp.24 <- atl.a.frp[atl.id.24]

# proportion
atla.frp24.prop <- atla.frp.24/10000

# Main
atlm.frp.24 <- atl.m.frp[atl.id.24]
atlm.tdist.24 <- atl.m.tdist[atl.id.24]
atlm.gstep.24 <- atl.m.gstep[atl.id.24]
atlm.deg.24 <- atl.m.deg[atl.id.24]
atlm.cumldeg.24 <- atl.m.cumldeg[atl.id.24]
atlm.bcent.24 <- atl.m.bcent[atl.id.24]

# proportion
atlm.frp24.prop <- atlm.frp.24/10000

# Casual
atlc.frp.24 <- atl.c.frp[atl.id.24]
atlc.tdist.24 <- atl.c.tdist[atl.id.24]
atlc.gstep.24 <- atl.c.gstep[atl.id.24]
atlc.deg.24 <- atl.c.deg[atl.id.24]
atlc.cumldeg.24 <- atl.c.cumldeg[atl.id.24]
atlc.bcent.24 <- atl.c.bcent[atl.id.24]

# proportion
atlc.frp24.prop <- atlc.frp.24/10000

# Inst
atli.frp.24 <- atl.i.frp[atl.id.24]
atli.tdist.24 <- atl.i.tdist[atl.id.24]
atli.gstep.24 <- atl.i.gstep[atl.id.24]
atli.deg.24 <- atl.i.deg[atl.id.24]
atli.cumldeg.24 <- atl.i.cumldeg[atl.id.24]
atli.bcent.24 <- atl.i.bcent[atl.id.24]

# proportion
atli.frp24.prop <- atli.frp.24/10000


### Age 25-34 ###

## SF

# All ptypes
sfa.frp.34 <- sf.a.frp[sf.id.34]

# proportion
sfa.frp34.prop <- sfa.frp.34/10000

# Main
sfm.frp.34 <- sf.m.frp[sf.id.34]
sfm.tdist.34 <- sf.m.tdist[sf.id.34]
sfm.gstep.34 <- sf.m.gstep[sf.id.34]
sfm.deg.34 <- sf.m.deg[sf.id.34]
sfm.cumldeg.34 <- sf.m.cumldeg[sf.id.34]
sfm.bcent.34 <- sf.m.bcent[sf.id.34]

# proportion
sfm.frp34.prop <- sfm.frp.34/10000

# Casual
sfc.frp.34 <- sf.c.frp[sf.id.34]
sfc.tdist.34 <- sf.c.tdist[sf.id.34]
sfc.gstep.34 <- sf.c.gstep[sf.id.34]
sfc.deg.34 <- sf.c.deg[sf.id.34]
sfc.cumldeg.34 <- sf.c.cumldeg[sf.id.34]
sfc.bcent.34 <- sf.c.bcent[sf.id.34]

# proportion
sfc.frp34.prop <- sfc.frp.34/10000

# Inst
sfi.frp.34 <- sf.i.frp[sf.id.34]
sfi.tdist.34 <- sf.i.tdist[sf.id.34]
sfi.gstep.34 <- sf.i.gstep[sf.id.34]
sfi.deg.34 <- sf.i.deg[sf.id.34]
sfi.cumldeg.34 <- sf.i.cumldeg[sf.id.34]
sfi.bcent.34 <- sf.i.bcent[sf.id.34]

# proportion
sfi.frp34.prop <- sfi.frp.34/10000

## ATL

# All ptypes
atla.frp.34 <- atl.a.frp[atl.id.34]

# proportion
atla.frp34.prop <- atla.frp.34/10000

# Main
atlm.frp.34 <- atl.m.frp[atl.id.34]
atlm.tdist.34 <- atl.m.tdist[atl.id.34]
atlm.gstep.34 <- atl.m.gstep[atl.id.34]
atlm.deg.34 <- atl.m.deg[atl.id.34]
atlm.cumldeg.34 <- atl.m.cumldeg[atl.id.34]
atlm.bcent.34 <- atl.m.bcent[atl.id.34]

# proportion
atlm.frp34.prop <- atlm.frp.34/10000

# Casual
atlc.frp.34 <- atl.c.frp[atl.id.34]
atlc.tdist.34 <- atl.c.tdist[atl.id.34]
atlc.gstep.34 <- atl.c.gstep[atl.id.34]
atlc.deg.34 <- atl.c.deg[atl.id.34]
atlc.cumldeg.34 <- atl.c.cumldeg[atl.id.34]
atlc.bcent.34 <- atl.c.bcent[atl.id.34]

# proportion
atlc.frp34.prop <- atlc.frp.34/10000

# Inst
atli.frp.34 <- atl.i.frp[atl.id.34]
atli.tdist.34 <- atl.i.tdist[atl.id.34]
atli.gstep.34 <- atl.i.gstep[atl.id.34]
atli.deg.34 <- atl.i.deg[atl.id.34]
atli.cumldeg.34 <- atl.i.cumldeg[atl.id.34]
atli.bcent.34 <- atl.i.bcent[atl.id.34]

# proportion
atli.frp34.prop <- atli.frp.34/10000


### Age 35-44 ###

## SF

# All ptypes
sfa.frp.44 <- sf.a.frp[sf.id.44]

# proportion
sfa.frp44.prop <- sfa.frp.44/10000

# Main
sfm.frp.44 <- sf.m.frp[sf.id.44]
sfm.tdist.44 <- sf.m.tdist[sf.id.44]
sfm.gstep.44 <- sf.m.gstep[sf.id.44]
sfm.deg.44 <- sf.m.deg[sf.id.44]
sfm.cumldeg.44 <- sf.m.cumldeg[sf.id.44]
sfm.bcent.44 <- sf.m.bcent[sf.id.44]

# proportion
sfm.frp44.prop <- sfm.frp.44/10000

# Casual
sfc.frp.44 <- sf.c.frp[sf.id.44]
sfc.tdist.44 <- sf.c.tdist[sf.id.44]
sfc.gstep.44 <- sf.c.gstep[sf.id.44]
sfc.deg.44 <- sf.c.deg[sf.id.44]
sfc.cumldeg.44 <- sf.c.cumldeg[sf.id.44]
sfc.bcent.44 <- sf.c.bcent[sf.id.44]

# proportion
sfc.frp44.prop <- sfc.frp.44/10000

# Inst
sfi.frp.44 <- sf.i.frp[sf.id.44]
sfi.tdist.44 <- sf.i.tdist[sf.id.44]
sfi.gstep.44 <- sf.i.gstep[sf.id.44]
sfi.deg.44 <- sf.i.deg[sf.id.44]
sfi.cumldeg.44 <- sf.i.cumldeg[sf.id.44]
sfi.bcent.44 <- sf.i.bcent[sf.id.44]

# proportion
sfi.frp44.prop <- sfi.frp.44/10000

## ATL

# All ptypes
atla.frp.44 <- atl.a.frp[atl.id.44]

# proportion
atla.frp44.prop <- atla.frp.44/10000

# Main
atlm.frp.44 <- atl.m.frp[atl.id.44]
atlm.tdist.44 <- atl.m.tdist[atl.id.44]
atlm.gstep.44 <- atl.m.gstep[atl.id.44]
atlm.deg.44 <- atl.m.deg[atl.id.44]
atlm.cumldeg.44 <- atl.m.cumldeg[atl.id.44]
atlm.bcent.44 <- atl.m.bcent[atl.id.44]

# proportion
atlm.frp44.prop <- atlm.frp.44/10000

# Casual
atlc.frp.44 <- atl.c.frp[atl.id.44]
atlc.tdist.44 <- atl.c.tdist[atl.id.44]
atlc.gstep.44 <- atl.c.gstep[atl.id.44]
atlc.deg.44 <- atl.c.deg[atl.id.44]
atlc.cumldeg.44 <- atl.c.cumldeg[atl.id.44]
atlc.bcent.44 <- atl.c.bcent[atl.id.44]

# proportion
atlc.frp44.prop <- atlc.frp.44/10000

# Inst
atli.frp.44 <- atl.c.frp[atl.id.44]
atli.tdist.44 <- atl.c.tdist[atl.id.44]
atli.gstep.44 <- atl.c.gstep[atl.id.44]
atli.deg.44 <- atl.c.deg[atl.id.44]
atli.cumldeg.44 <- atl.c.cumldeg[atl.id.44]
atli.bcent.44 <- atl.c.bcent[atl.id.44]

# proportion
atli.frp44.prop <- atli.frp.44/10000


### Age 45-54 ###

## SF

# all ptypes
sfa.frp.54 <- sf.a.frp[sf.id.54]

# proportion
sfa.frp54.prop <- sfa.frp.54/10000

# Main
sfm.frp.54 <- sf.m.frp[sf.id.54]
sfm.tdist.54 <- sf.m.tdist[sf.id.54]
sfm.gstep.54 <- sf.m.gstep[sf.id.54]
sfm.deg.54 <- sf.m.deg[sf.id.54]
sfm.cumldeg.54 <- sf.m.cumldeg[sf.id.54]
sfm.bcent.54 <- sf.m.bcent[sf.id.54]

# proportion
sfm.frp54.prop <- sfm.frp.54/10000

# Casual
sfc.frp.54 <- sf.c.frp[sf.id.54]
sfc.tdist.54 <- sf.c.tdist[sf.id.54]
sfc.gstep.54 <- sf.c.gstep[sf.id.54]
sfc.deg.54 <- sf.c.deg[sf.id.54]
sfc.cumldeg.54 <- sf.c.cumldeg[sf.id.54]
sfc.bcent.54 <- sf.c.bcent[sf.id.54]

# proportion
sfc.frp54.prop <- sfc.frp.54/10000

# Inst
sfi.frp.54 <- sf.i.frp[sf.id.54]
sfi.tdist.54 <- sf.i.tdist[sf.id.54]
sfi.gstep.54 <- sf.i.gstep[sf.id.54]
sfi.deg.54 <- sf.i.deg[sf.id.54]
sfi.cumldeg.54 <- sf.i.cumldeg[sf.id.54]
sfi.bcent.54 <- sf.i.bcent[sf.id.54]

# proportion
sfi.frp54.prop <- sfi.frp.54/10000

## ATL

# All ptypes
atla.frp.54 <- atl.a.frp[atl.id.54]

# proportion
atla.frp54.prop <- atla.frp.54/10000

# Main
atlm.frp.54 <- atl.m.frp[atl.id.54]
atlm.tdist.54 <- atl.m.tdist[atl.id.54]
atlm.gstep.54 <- atl.m.gstep[atl.id.54]
atlm.deg.54 <- atl.m.deg[atl.id.54]
atlm.cumldeg.54 <- atl.m.cumldeg[atl.id.54]
atlm.bcent.54 <- atl.m.bcent[atl.id.54]

# proportion
atlm.frp54.prop <- atlm.frp.54/10000

# Casual
atlc.frp.54 <- atl.c.frp[atl.id.54]
atlc.tdist.54 <- atl.c.tdist[atl.id.54]
atlc.gstep.54 <- atl.c.gstep[atl.id.54]
atlc.deg.54 <- atl.c.deg[atl.id.54]
atlc.cumldeg.54 <- atl.c.cumldeg[atl.id.54]
atlc.bcent.54 <- atl.c.bcent[atl.id.54]

# proportion
atlc.frp54.prop <- atlc.frp.54/10000

# Inst
atli.frp.54 <- atl.i.frp[atl.id.54]
atli.tdist.54 <- atl.i.tdist[atl.id.54]
atli.gstep.54 <- atl.i.gstep[atl.id.54]
atli.deg.54 <- atl.i.deg[atl.id.54]
atli.cumldeg.54 <- atl.i.cumldeg[atl.id.54]
atli.bcent.54 <- atl.i.bcent[atl.id.54]

# proportion
atli.frp54.prop <- atli.frp.54/10000


### Age 55+ ###

## SF

# All ptypes
sfa.frp.64 <- sf.a.frp[sf.id.64]

# proportion
sfa.frp64.prop <- sfa.frp.64/10000

# Main
sfm.frp.64 <- sf.m.frp[sf.id.64]
sfm.tdist.64 <- sf.m.tdist[sf.id.64]
sfm.gstep.64 <- sf.m.gstep[sf.id.64]
sfm.deg.64 <- sf.m.deg[sf.id.64]
sfm.cumldeg.64 <- sf.m.cumldeg[sf.id.64]
sfm.bcent.64 <- sf.m.bcent[sf.id.64]

# proportion
sfm.frp64.prop <- sfm.frp.64/10000

# Casual
sfc.frp.64 <- sf.c.frp[sf.id.64]
sfc.tdist.64 <- sf.c.tdist[sf.id.64]
sfc.gstep.64 <- sf.c.gstep[sf.id.64]
sfc.deg.64 <- sf.c.deg[sf.id.64]
sfc.cumldeg.64 <- sf.c.cumldeg[sf.id.64]
sfc.bcent.64 <- sf.c.bcent[sf.id.64]

# proportion
sfc.frp64.prop <- sfc.frp.64/10000

# Inst
sfi.frp.64 <- sf.i.frp[sf.id.64]
sfi.tdist.64 <- sf.i.tdist[sf.id.64]
sfi.gstep.64 <- sf.i.gstep[sf.id.64]
sfi.deg.64 <- sf.i.deg[sf.id.64]
sfi.cumldeg.64 <- sf.i.cumldeg[sf.id.64]
sfi.bcent.64 <- sf.i.bcent[sf.id.64]

# proportion
sfi.frp64.prop <- sfi.frp.64/10000

## ATL

# All ptypes
atla.frp.64 <- atl.a.frp[atl.id.64]

# proportion
atla.frp64.prop <- atla.frp.64/10000

# Main
atlm.frp.64 <- atl.m.frp[atl.id.64]
atlm.tdist.64 <- atl.m.tdist[atl.id.64]
atlm.gstep.64 <- atl.m.gstep[atl.id.64]
atlm.deg.64 <- atl.m.deg[atl.id.64]
atlm.cumldeg.64 <- atl.m.cumldeg[atl.id.64]
atlm.bcent.64 <- atl.m.bcent[atl.id.64]

# proportion
atlm.frp64.prop <- atlm.frp.64/10000

# Casual
atlc.frp.64 <- atl.c.frp[atl.id.64]
atlc.tdist.64 <- atl.c.tdist[atl.id.64]
atlc.gstep.64 <- atl.c.gstep[atl.id.64]
atlc.deg.64 <- atl.c.deg[atl.id.64]
atlc.cumldeg.64 <- atl.c.cumldeg[atl.id.64]
atlc.bcent.64 <- atl.c.bcent[atl.id.64]

# proportion
atlc.frp64.prop <- atlc.frp.64/10000

# Inst
atli.frp.64 <- atl.i.frp[atl.id.64]
atli.tdist.64 <- atl.i.tdist[atl.id.64]
atli.gstep.64 <- atl.i.gstep[atl.id.64]
atli.deg.64 <- atl.i.deg[atl.id.64]
atli.cumldeg.64 <- atl.i.cumldeg[atl.id.64]
atli.bcent.64 <- atl.i.bcent[atl.id.64]

# proportion
atli.frp64.prop <- atli.frp.64/10000



# 4c. Race & Age Outcomes -------------------------------------------------

### Black 0-24 ###

## SF

# All ptypes
sfa.frp.b24 <- sf.a.frp[sf.id.b24]

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

# All ptypes
atla.frp.b24 <- atl.a.frp[atl.id.b24]

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


### White 0-24 ###

## SF

# All pytpes
sfa.frp.w24 <- sf.a.frp[sf.id.w24]

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

# All ptypes
atla.frp.w24 <- atl.a.frp[atl.id.w24]

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


### Black 25-34 ###

## SF

# All ptypes
sfa.frp.b34 <- sf.a.frp[sf.id.b34]

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

# All ptypes
atla.frp.b34 <- atl.a.frp[atl.id.b34]

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


### White 25-34 ###

## SF

# All ptypes
sfa.frp.w34 <- sf.a.frp[sf.id.w34]

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

# All ptypes
atla.frp.w34 <- atl.a.frp[atl.id.w34]

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


### Black 35-44 ###

## SF

# All ptypes
sfa.frp.b44 <- sf.a.frp[sf.id.b44]

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

# All ptypes
atla.frp.b44 <- atl.a.frp[atl.id.b44]

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


### White 35-44 ###

## SF

# All ptypes
sfa.frp.w44 <- sf.a.frp[sf.id.w44]

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

# All ptypes
atla.frp.w44 <- atl.a.frp[atl.id.w44]

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


### Black 45-54 ###

## SF

# All ptypes
sfa.frp.b54 <- sf.a.frp[sf.id.b54]

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

# All ptypes
atla.frp.b54 <- atl.a.frp[atl.id.b54]

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


### White 45-54 ###

## SF

# All ptypes
sfa.frp.w54 <- sf.a.frp[sf.id.w54]

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

# All ptypes
atla.frp.w54 <- atl.a.frp[atl.id.w54]

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


### Black 55+ ###

## SF

# All ptypes
sfa.frp.b64 <- sf.a.frp[sf.id.b64]

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

# All ptypes
atla.frp.b64 <- atl.a.frp[atl.id.b64]

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


### White 55+ ###

## SF

# All ptypes
sfa.frp.w64 <- sf.a.frp[sf.id.w64]

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

# All ptypes
atla.frp.w64 <- atl.a.frp[atl.id.w64]

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



# 5a. FRP Analysis --------------------------------------------------------

### Summary Stats ###

## By partnership type
# SF
sf.frpa.sum <- summary(t(sf.a.frp))
write.csv(sf.frpa.sum, file = "sf_frp_all.csv")

sf.frpm.sum <- summary(t(sf.m.frp))
write.csv(sf.frpm.sum, file = "sf_frp_main.csv")

sf.frpc.sum <- summary(t(sf.c.frp))
write.csv(sf.frpc.sum, file = "sf_frp_casl.csv")

sf.frpi.sum <- summary(t(sf.i.frp))
write.csv(sf.frpi.sum, file = "sf_frp_inst.csv")


# ATL
atl.frpa.sum <- summary(t(atl.a.frp))
write.csv(atl.frpa.sum, file = "atl_frp_all.csv")

atl.frpm.sum <- summary(t(atl.m.frp))
write.csv(atl.frpm.sum, file = "atl_frp_main.csv")

atl.frpc.sum <- summary(t(atl.c.frp))
write.csv(atl.frpc.sum, file = "atl_frp_casl.csv")

atl.frpi.sum <- summary(t(atl.i.frp))
write.csv(atl.frpi.sum, file = "atl_frp_inst.csv")


## Race - BLACK
# SF 
sf.frpa.sumb <- summary(t(sfa.frp.b))
write.csv(sf.frpa.sumb, file = "sf_frp_all_black.csv")

sf.frpm.sumb <- summary(t(sfm.frp.b))
write.csv(sf.frpm.sumb, file = "sf_frp_main_black.csv")

sf.frpc.sumb <- summary(t(sfc.frp.b))
write.csv(sf.frpc.sumb, file = "sf_frp_casl_black.csv")

sf.frpi.sumb <- summary(t(sfi.frp.b))
write.csv(sf.frpi.sumb, file = "sf_frp_inst_black.csv")

# ATL
atl.frpa.sumb <- summary(t(atla.frp.b))
write.csv(atl.frpa.sumb, file = "atl_frp_all_black.csv")

atl.frpm.sumb <- summary(t(atlm.frp.b))
write.csv(atl.frpm.sumb, file = "atl_frp_main_black.csv")

atl.frpc.sumb <- summary(t(atlc.frp.b))
write.csv(atl.frpc.sumb, file = "atl_frp_casl_black.csv")

atl.frpi.sumb <- summary(t(atli.frp.b))
write.csv(atl.frpi.sumb, file = "atl_frp_inst_black.csv")


## Race - WHITE
# SF
sf.frpa.sumw <- summary(t(sfa.frp.w))
write.csv(sf.frpa.sumw, file = "sf_frp_all_white.csv")

sf.frpm.sumw <- summary(t(sfm.frp.w))
write.csv(sf.frpm.sumw, file = "sf_frp_main_white.csv")

sf.frpc.sumw <- summary(t(sfc.frp.w))
write.csv(sf.frpc.sumw, file = "sf_frp_casl_white.csv")

sf.frpi.sumw <- summary(t(sfi.frp.w))
write.csv(sf.frpi.sumw, file = "sf_frp_inst_white.csv")

# ATL
atl.frpa.sumw <- summary(t(atla.frp.w))
write.csv(atl.frpa.sumw, file = "atl_frp_all_white.csv")

atl.frpm.sumw <- summary(t(atlm.frp.w))
write.csv(atl.frpm.sumw, file = "atl_frp_main_white.csv")

atl.frpc.sumw <- summary(t(atlc.frp.w))
write.csv(atl.frpc.sumw, file = "atl_frp_casl_white.csv")

atl.frpi.sumw <- summary(t(atli.frp.w))
write.csv(atl.frpi.sumw, file = "atl_frp_inst_white.csv")


## Age - 15-24
# SF 
sf.frpa.sum24 <- summary(t(sfa.frp.24))
write.csv(sf.frpa.sum24, file = "sf_frp_all_15-24.csv")

sf.frpm.sum24 <- summary(t(sfm.frp.24))
write.csv(sf.frpm.sum24, file = "sf_frp_main_15-24.csv")

sf.frpc.sum24 <- summary(t(sfc.frp.24))
write.csv(sf.frpc.sum24, file = "sf_frp_casl_15-24.csv")

sf.frpi.sum24 <- summary(t(sfi.frp.24))
write.csv(sf.frpi.sum24, file = "sf_frp_inst_15-24.csv")

# ATL
atl.frpa.sum24 <- summary(t(atla.frp.24))
write.csv(atl.frpa.sum24, file = "atl_frp_all_15-24.csv")

atl.frpm.sum24 <- summary(t(atlm.frp.24))
write.csv(atl.frpm.sum24, file = "atl_frp_main_15-24.csv")

atl.frpc.sum24 <- summary(t(atlc.frp.24))
write.csv(atl.frpc.sum24, file = "atl_frp_casl_15-24.csv")

atl.frpi.sum24 <- summary(t(atli.frp.24))
write.csv(atl.frpi.sum24, file = "atl_frp_inst_15-24.csv")


## Age - 25-34
# SF 
sf.frpa.sum34 <- summary(t(sfa.frp.34))
write.csv(sf.frpa.sum34, file = "sf_frp_all_25-34.csv")

sf.frpm.sum34 <- summary(t(sfm.frp.34))
write.csv(sf.frpm.sum34, file = "sf_frp_main_25-34.csv")

sf.frpc.sum34 <- summary(t(sfc.frp.34))
write.csv(sf.frpc.sum34, file = "sf_frp_casl_25-34.csv")

sf.frpi.sum34 <- summary(t(sfi.frp.34))
write.csv(sf.frpi.sum34, file = "sf_frp_inst_25-34.csv")

# ATL
atl.frpa.sum34 <- summary(t(atla.frp.34))
write.csv(atl.frpa.sum34, file = "atl_frp_all_25-34.csv")

atl.frpm.sum34 <- summary(t(atlm.frp.34))
write.csv(atl.frpm.sum34, file = "atl_frp_main_25-34.csv")

atl.frpc.sum34 <- summary(t(atlc.frp.34))
write.csv(atl.frpc.sum34, file = "atl_frp_casl_25-34.csv")

atl.frpi.sum34 <- summary(t(atli.frp.34))
write.csv(atl.frpi.sum34, file = "atl_frp_inst_25-34.csv")


## Age - 35-44
# SF 
sf.frpa.sum44 <- summary(t(sfa.frp.44))
write.csv(sf.frpa.sum44, file = "sf_frp_all_35-44.csv")

sf.frpm.sum44 <- summary(t(sfm.frp.44))
write.csv(sf.frpm.sum44, file = "sf_frp_main_35-44.csv")

sf.frpc.sum44 <- summary(t(sfc.frp.44))
write.csv(sf.frpc.sum44, file = "sf_frp_casl_35-44.csv")

sf.frpi.sum44 <- summary(t(sfi.frp.44))
write.csv(sf.frpi.sum44, file = "sf_frp_inst_35-44.csv")

# ATL
atl.frpa.sum44 <- summary(t(atla.frp.44))
write.csv(atl.frpa.sum44, file = "atl_frp_all_35-44.csv")

atl.frpm.sum44 <- summary(t(atlm.frp.44))
write.csv(atl.frpm.sum44, file = "atl_frp_main_35-44.csv")

atl.frpc.sum44 <- summary(t(atlc.frp.44))
write.csv(atl.frpc.sum44, file = "atl_frp_casl_35-44.csv")

atl.frpi.sum44 <- summary(t(atli.frp.44))
write.csv(atl.frpi.sum44, file = "atl_frp_inst_35-44.csv")


## Age - 45-54
# SF 
sf.frpa.sum54 <- summary(t(sfa.frp.54))
write.csv(sf.frpa.sum54, file = "sf_frp_all_45-54.csv")

sf.frpm.sum54 <- summary(t(sfm.frp.54))
write.csv(sf.frpm.sum54, file = "sf_frp_main_45-54.csv")

sf.frpc.sum54 <- summary(t(sfc.frp.54))
write.csv(sf.frpc.sum54, file = "sf_frp_casl_45-54.csv")

sf.frpi.sum54 <- summary(t(sfi.frp.54))
write.csv(sf.frpi.sum54, file = "sf_frp_inst_45-54.csv")

# ATL
atl.frpa.sum54 <- summary(t(atla.frp.54))
write.csv(atl.frpa.sum54, file = "atl_frp_all_45-54.csv")

atl.frpm.sum54 <- summary(t(atlm.frp.54))
write.csv(atl.frpm.sum54, file = "atl_frp_main_45-54.csv")

atl.frpc.sum54 <- summary(t(atlc.frp.54))
write.csv(atl.frpc.sum54, file = "atl_frp_casl_45-54.csv")

atl.frpi.sum54 <- summary(t(atli.frp.54))
write.csv(atl.frpi.sum54, file = "atl_frp_inst_45-54.csv")


## Age - 55-64
# SF 
sf.frpa.sum64 <- summary(t(sfa.frp.64))
write.csv(sf.frpa.sum64, file = "sf_frp_all_55-64.csv")

sf.frpm.sum64 <- summary(t(sfm.frp.64))
write.csv(sf.frpm.sum64, file = "sf_frp_main_55-64.csv")

sf.frpc.sum64 <- summary(t(sfc.frp.64))
write.csv(sf.frpc.sum64, file = "sf_frp_casl_55-64.csv")

sf.frpi.sum64 <- summary(t(sfi.frp.64))
write.csv(sf.frpi.sum64, file = "sf_frp_inst_55-64.csv")

# ATL
atl.frpa.sum64 <- summary(t(atla.frp.64))
write.csv(atl.frpa.sum64, file = "atl_frp_all_55-64.csv")

atl.frpm.sum64 <- summary(t(atlm.frp.64))
write.csv(atl.frpm.sum64, file = "atl_frp_main_55-64.csv")

atl.frpc.sum64 <- summary(t(atlc.frp.64))
write.csv(atl.frpc.sum64, file = "atl_frp_casl_55-64.csv")

atl.frpi.sum64 <- summary(t(atli.frp.64))
write.csv(atl.frpi.sum64, file = "atl_frp_inst_55-64.csv")



# 5a. FRP Plots -----------------------------------------------------------

### Plots of distribution ###

# matplot(sf.a.frp, type = "l", xlab = "Week", ylab = "FRP", main = "SF All Partnership Types")

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


### Mean & median plots ###

## Overall
# All ptypes
sfa.frp.mean <- as.data.frame(t(rowMeans(sfa.frp.prop)))
atla.frp.mean <- as.data.frame(t(rowMeans(atla.frp.prop)))

plot(x = 1:260, y = sfa.frp.mean, type = "l", col = alpha("red", 0.5), lwd = 2, 
     main = "Average Proportion of the Population Reachable over 5-Years in \nSexual Networks of MSM in San Francisco and Atlanta",
     xlab = "Week", ylab = "Forward Reachable Path")
lines(x = 1:260, y = atla.frp.mean, type = "l", col = alpha("blue", 0.5), lwd = 2)
legend("bottomright", legend = c("San Francisco", "Atlanta"), col = c("red", "blue"), lty = 1)


# Main
sfm.frp.mean <- as.data.frame(t(rowMeans(sfm.frp.prop)))
atlm.frp.mean <- as.data.frame(t(rowMeans(atlm.frp.prop)))

plot(x = 1:260, y = sfm.frp.mean, type = "l", col = alpha("red", 0.5), lwd = 2, 
     main = "Average Proportion of the Population Reachable over 5-Years in \nSexual Networks of Main Partnerships of MSM in San Francisco and Atlanta",
     xlab = "Week", ylab = "Forward Reachable Path")
lines(x = 1:260, y = atlm.frp.mean, type = "l", col = alpha("blue", 0.5), lwd = 2)
legend("bottomright", legend = c("San Francisco", "Atlanta"), col = c("red", "blue"), lty = 1)


# Casual
sfc.frp.mean <- as.data.frame(t(rowMeans(sfc.frp.prop)))
atlc.frp.mean <- as.data.frame(t(rowMeans(atlc.frp.prop)))

plot(x = 1:260, y = sfc.frp.mean, type = "l", col = alpha("red", 0.5), lwd = 2, 
     main = "Average Proportion of the Population Reachable over 5-Years in \nSexual Networks of Casual Partnerships of MSM in San Francisco and Atlanta",
     xlab = "Week", ylab = "Forward Reachable Path")
lines(x = 1:260, y = atlc.frp.mean, type = "l", col = alpha("blue", 0.5), lwd = 2)
legend("topleft", legend = c("San Francisco", "Atlanta"), col = c("red", "blue"), lty = 1)


# Inst
sfi.frp.mean <- as.data.frame(t(rowMeans(sfi.frp.prop)))
atli.frp.mean <- as.data.frame(t(rowMeans(atli.frp.prop)))

plot(x = 1:260, y = sfi.frp.mean, type = "l", col = alpha("red", 0.5), lwd = 2, 
     main = "Average Proportion of the Population Reachable over 5-Years in \nSexual Networks of One-Time Partnerships of MSM in San Francisco and Atlanta",
     xlab = "Week", ylab = "Forward Reachable Path")
lines(x = 1:260, y = atli.frp.mean, type = "l", col = alpha("blue", 0.5), lwd = 2)
legend("bottomright", legend = c("San Francisco", "Atlanta"), col = c("red", "blue"), lty = 1)

## By Race

######

# sfa.frpb.mean <- as.data.frame(t(rowMeans(sfa.frp.b)))
# atla.frpb.mean <- as.data.frame(t(rowMeans(atla.frp.b)))
# sfa.frpw.mean <- as.data.frame(t(rowMeans(sfa.frp.w)))
# atla.frpw.mean <- as.data.frame(t(rowMeans(atla.frp.w)))
#


# End ---------------------------------------------------------------------










