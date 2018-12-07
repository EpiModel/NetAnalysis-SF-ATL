## TSNA Analysis Test
## 9/23/2018

library("tsna")
library("networkDynamicData")
suppressMessages(library("EpiModel"))
# library("sna")
# library("ggplot2")

set.seed(803)


# 0. Netowrk data ---------------------------------------------------------


### Inputs ###
city_name <- "San Francisco"
nick_name <- "sfo"

### Load Sims ###
fn <- paste("artnet.NetSim", gsub(" ", "", city_name), "rda", sep = ".")
sim <- readRDS(file = fn)

# Extract each partner type network

sim.m <- sim[[1]] # main
sim.c <- sim[[2]] # casual
sim.i <- sim[[3]] # inst

# 1. Outcome Data ---------------------------------------------------------


### Load outcome data ###

fn <- paste(nick_name, gsub(" ", "", "all.1"), "rda", sep = ".")
out.all <- load(file = fn)
out.all <- as.data.frame(out)

fn <- paste(nick_name, gsub(" ", "", "main.1"), "rda", sep = ".")
out.main <- load(file = fn)
out.main <- as.data.frame(out)

fn <- paste(nick_name, gsub(" ", "", "casl.1"), "rda", sep = ".")
out.casl <- load(file = fn)
out.casl <- as.data.frame(out)

fn <- paste(nick_name, gsub(" ", "", "inst.1"), "rda", sep = ".")
out.inst <- load(file = fn)
out.inst <- as.data.frame(out)


### Subset by outcome ###

# Main
m.frp <- subset(out.main, select = frp.1:frp.10000)
m.tdist <- subset(out.main, select = medtdist.1:medtdist.10000)
m.gstep <- subset(out.main, select = medgeod.1:medgeod.10000)
m.deg <- subset(out.main, select = degree.1:degree.10000)
m.cumldeg <- subset(out.main, select = cumldegree.1:cumldegree.10000)
m.bcent <- subset(out.main, select = bcent.1:bcent.10000)

# Casual
c.frp <- subset(out.casl, select = frp.1:frp.10000)
c.tdist <- subset(out.casl, select = medtdist.1:medtdist.10000)
c.gstep <- subset(out.casl, select = medgeod.1:medgeod.10000)
c.deg <- subset(out.casl, select = degree.1:degree.10000)
c.cumldeg <- subset(out.casl, select = cumldegree.1:cumldegree.10000)
c.bcent <- subset(out.casl, select = bcent.1:bcent.10000)

# Inst
i.frp <- subset(out.casl, select = frp.1:frp.10000)
i.tdist <- subset(out.casl, select = medtdist.1:medtdist.10000)
i.gstep <- subset(out.casl, select = medgeod.1:medgeod.10000)
i.deg <- subset(out.casl, select = degree.1:degree.10000)
i.cumldeg <- subset(out.casl, select = cumldegree.1:cumldegree.10000)
i.bcent <- subset(out.casl, select = bcent.1:bcent.10000)


# 2. Validation -----------------------------------------------------------

## Checking degree in simulations

EpiModel::get_degree(network.collapse(sim.m, at = 1))
EpiModel::get_degree(network.collapse(sim.c, at = 1))
EpiModel::get_degree(network.collapse(sim.i, at = 1))

## Checking degree in outcome data

test <- subset(out.main, select = degree.1:degree.10000)
test <- subset(out.casl, select = degree.1:degree.10000)
test <- subset(out.inst, select = degree.1:degree.10000)


# 3. Extract Vertex IDs ------------------------------------------------------

### SF ###

# Black
blck.m <- which(get.vertex.attribute(sim.m, "race") == "B")

# Main
m.frp.blck <- m.frp[blck.m]
m.tdist <- subset(out.main, select = medtdist.1:medtdist.10000)
m.gstep <- subset(out.main, select = medgeod.1:medgeod.10000)
m.deg <- subset(out.main, select = degree.1:degree.10000)
m.cumldeg <- subset(out.main, select = cumldegree.1:cumldegree.10000)
m.bcent <- subset(out.main, select = bcent.1:bcent.10000)




