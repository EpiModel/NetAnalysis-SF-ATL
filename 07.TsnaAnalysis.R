## TSNA Analysis
## 9/23/2018

library("tsna")
library("networkDynamicData")
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

# Load in outcome data

### ALL ptypes ###

sf.all <- load(file = "sfo.all.4.rda")
sf.all <- out
sf.all <- as.data.frame(sf.all)

atl.all <- load(file = "atl.all.4.rda")
atl.all <- out
atl.all <- as.data.frame(atl.all)


### Main ###

sf.main <- load(file = "sfo.main.4.rda")
sf.main <- out
sf.main <- as.data.frame(sf.main)

atl.main <- load(file = "atl.main.4.rda")
atl.main <- out
atl.main <- as.data.frame(atl.main)


### CASUAL ###
 
sf.casl <- load(file = "sfo.casl.4.rda")
sf.casl <- out
sf.casl <- as.data.frame(sf.casl)

atl.casl <- load(file = "atl.casl.4.rda")
atl.casl <- out
atl.casl <- as.data.frame(atl.casl)


### INST ###

sf.inst <- load(file = "sf.inst.4.rda")
sf.inst <- tdf
sf.inst <- as.data.frame(sf.inst)

atl.inst <- load(file = "atl.inst.4.rda")
atl.inst <- tdf
atl.inst <- as.data.frame(atl.inst)


# 2. Extract Vertex IDs ------------------------------------------------------

### SF ###

# Black
sf.id.b <- which(get.vertex.attribute(sf.m, "race") == "B")

sf.all.b <- sf.all[sf.b.id]

# Extract IDs SF
s1_main_sf_B <- s1_main_sf[[3]][which(get.vertex.attribute(s1_main_sf, "race") == "B")]

sf.b.id <- rep("NA", length(s1_main_sf_B))

for (i in 1:length(s1_main_sf_B)) {
  sf.b.id[i] <- s1_main_sf_B[[i]][2]
}


sf.b.id <- paste0("V", sf.b.id)


sf.b.id <- paste0("V", sf.b.id)

# Data sets of only black IDs
sf.all.B <- sf.all[sf.b.id]
sf.main.B <- sf.main[sf.b.id]
sf.casl.B <- sf.casl[sf.b.id]
sf.inst.B <- sf.inst[sf.b.id]

## Black

# Extract IDs SF
s1_main_sf_B <- s1_main_sf[[3]][which(get.vertex.attribute(s1_main_sf, "race") == "B")]

sf.b.id <- rep("NA", length(s1_main_sf_B))

for (i in 1:length(s1_main_sf_B)) {
  sf.b.id[i] <- s1_main_sf_B[[i]][2]
}


sf.b.id <- paste0("V", sf.b.id)

# Data sets of only black IDs
sf.all.B <- sf.all[sf.b.id]
sf.main.B <- sf.main[sf.b.id]
sf.casl.B <- sf.casl[sf.b.id]
sf.inst.B <- sf.inst[sf.b.id]

# Extract IDs ATL
s1_main_atl_B <- s1_main_atl[[3]][which(get.vertex.attribute(s1_main_atl, "race") == "B")]

atl.b.id <- rep("NA", length(s1_main_atl_B))

for (i in 1:length(s1_main_atl_B)) {
  atl.b.id[i] <- s1_main_atl_B[[i]][2]
}

atl.b.id <- paste0("V", atl.b.id)

# Data sets of only black IDs
atl.all.B <- atl.all[atl.b.id]
atl.main.B <- atl.main[atl.b.id]
atl.casl.B <- atl.casl[atl.b.id]
atl.inst.B <- atl.inst[atl.b.id]


## White

# Extract IDs SF
s1_main_sf_W <- s1_main_sf[[3]][which(get.vertex.attribute(s1_main_sf, "race") == "W")]

sf.w.id <- rep("NA", length(s1_main_sf_W))

for (i in 1:length(s1_main_sf_W)) {
  sf.w.id[i] <- s1_main_sf_W[[i]][2]
}

sf.w.id <- paste0("V", sf.w.id)

# Data sets of only white IDs
sf.all.W <- sf.all[sf.w.id]
sf.main.W <- sf.main[sf.w.id]
sf.casl.W <- sf.casl[sf.w.id]
sf.inst.W <- sf.inst[sf.w.id]

# Extract IDs ATL
s1_main_atl_W <- s1_main_atl[[3]][which(get.vertex.attribute(s1_main_atl, "race") == "W")]

atl.w.id <- rep("NA", length(s1_main_atl_W))

for (i in 1:length(s1_main_atl_W)) {
  atl.w.id[i] <- s1_main_atl_W[[i]][2]
}

atl.w.id <- paste0("V", atl.w.id)

# Data sets of only black IDs
atl.all.W <- atl.all[atl.w.id]
atl.main.W <- atl.main[atl.w.id]
atl.casl.W <- atl.casl[atl.w.id]
atl.inst.W <- atl.inst[atl.w.id]


### Age

## Age 0-24

# SF
s1_main_sf_24 <- s1_main_sf[[3]][which(get.vertex.attribute(s1_main_sf, "age.grp") == "1")]

sf.24.id <- rep("NA", length(s1_main_sf_24))

for (i in 1:length(s1_main_sf_24)) {
  sf.24.id[i] <- s1_main_sf_24[[i]][2]
}

sf.24.id <- paste0("V", sf.24.id)

sf.all.24 <- sf.all[sf.24.id]
sf.main.24 <- sf.main[sf.24.id]
sf.casl.24 <- sf.casl[sf.24.id]
sf.inst.24 <- sf.inst[sf.24.id]

# ATL
s1_main_atl_24 <- s1_main_atl[[3]][which(get.vertex.attribute(s1_main_atl, "age.grp") == "1")]

atl.24.id <- rep("NA", length(s1_main_atl_24))

for (i in 1:length(s1_main_atl_24)) {
  atl.24.id[i] <- s1_main_atl_24[[i]][2]
}

atl.24.id <- paste0("V", atl.24.id)

atl.all.24 <- atl.all[atl.24.id]
atl.main.24 <- atl.main[atl.24.id]
atl.casl.24 <- atl.casl[atl.24.id]
atl.inst.24 <- atl.inst[atl.24.id]


# Age 25-34

# SF
s1_main_sf_34 <- s1_main_sf[[3]][which(get.vertex.attribute(s1_main_sf, "age.grp") == "2")]

sf.34.id <- rep("NA", length(s1_main_sf_34))

for (i in 1:length(s1_main_sf_34)) {
  sf.34.id[i] <- s1_main_sf_34[[i]][2]
}

sf.34.id <- paste0("V", sf.34.id)

sf.all.34 <- sf.all[sf.34.id]
sf.main.34 <- sf.main[sf.34.id]
sf.casl.34 <- sf.casl[sf.34.id]
sf.inst.34 <- sf.inst[sf.34.id]

# ATL
s1_main_atl_34 <- s1_main_atl[[3]][which(get.vertex.attribute(s1_main_atl, "age.grp") == "2")]

atl.34.id <- rep("NA", length(s1_main_atl_34))

for (i in 1:length(s1_main_atl_34)) {
  atl.34.id[i] <- s1_main_atl_34[[i]][2]
}

atl.34.id <- paste0("V", atl.34.id)

atl.all.34 <- atl.all[atl.34.id]
atl.main.34 <- atl.main[atl.34.id]
atl.casl.34 <- atl.casl[atl.34.id]
atl.inst.34 <- atl.inst[atl.34.id]


# Age 35-44

# SF
s1_main_sf_44 <- s1_main_sf[[3]][which(get.vertex.attribute(s1_main_sf, "age.grp") == "3")]

sf.44.id <- rep("NA", length(s1_main_sf_44))

for (i in 1:length(s1_main_sf_44)) {
  sf.44.id[i] <- s1_main_sf_44[[i]][2]
}

sf.44.id <- paste0("V", sf.44.id)

sf.all.44 <- sf.all[sf.44.id]
sf.main.44 <- sf.main[sf.44.id]
sf.casl.44 <- sf.casl[sf.44.id]
sf.inst.44 <- sf.inst[sf.44.id]

# ATL
s1_main_atl_44 <- s1_main_atl[[3]][which(get.vertex.attribute(s1_main_atl, "age.grp") == "3")]

atl.44.id <- rep("NA", length(s1_main_atl_44))

for (i in 1:length(s1_main_atl_44)) {
  atl.44.id[i] <- s1_main_atl_44[[i]][2]
}

atl.44.id <- paste0("V", atl.44.id)

atl.all.44 <- atl.all[atl.44.id]
atl.main.44 <- atl.main[atl.44.id]
atl.casl.44 <- atl.casl[atl.44.id]
atl.inst.44 <- atl.inst[atl.44.id]


# Age 45-54

# SF
s1_main_sf_54 <- s1_main_sf[[3]][which(get.vertex.attribute(s1_main_sf, "age.grp") == "4")]

sf.54.id <- rep("NA", length(s1_main_sf_54))

for (i in 1:length(s1_main_sf_54)) {
  sf.54.id[i] <- s1_main_sf_54[[i]][2]
}

sf.54.id <- paste0("V", sf.54.id)

sf.all.54 <- sf.all[sf.54.id]
sf.main.54 <- sf.main[sf.54.id]
sf.casl.54 <- sf.casl[sf.54.id]
sf.inst.54 <- sf.inst[sf.54.id]

# ATL
s1_main_atl_54 <- s1_main_atl[[3]][which(get.vertex.attribute(s1_main_atl, "age.grp") == "4")]

atl.54.id <- rep("NA", length(s1_main_atl_54))

for (i in 1:length(s1_main_atl_54)) {
  atl.54.id[i] <- s1_main_atl_54[[i]][2]
}

atl.54.id <- paste0("V", atl.54.id)

atl.all.54 <- atl.all[atl.54.id]
atl.main.54 <- atl.main[atl.54.id]
atl.casl.54 <- atl.casl[atl.54.id]
atl.inst.54 <- atl.inst[atl.54.id]

# Age 55+

# SF
s1_main_sf_64 <- s1_main_sf[[3]][which(get.vertex.attribute(s1_main_sf, "age.grp") == "5")]

sf.64.id <- rep("NA", length(s1_main_sf_64))

for (i in 1:length(s1_main_sf_64)) {
  sf.64.id[i] <- s1_main_sf_64[[i]][2]
}

sf.64.id <- paste0("V", sf.64.id)

sf.all.64 <- sf.all[sf.64.id]
sf.main.64 <- sf.main[sf.64.id]
sf.casl.64 <- sf.casl[sf.64.id]
sf.inst.64 <- sf.inst[sf.64.id]

# ATL
s1_main_atl_64 <- s1_main_atl[[3]][which(get.vertex.attribute(s1_main_atl, "age.grp") == "5")]

atl.64.id <- rep("NA", length(s1_main_atl_64))

for (i in 1:length(s1_main_atl_64)) {
  atl.64.id[i] <- s1_main_atl_64[[i]][2]
}

atl.64.id <- paste0("V", atl.64.id)

atl.all.64 <- atl.all[atl.64.id]
atl.main.64 <- atl.main[atl.64.id]
atl.casl.64 <- atl.casl[atl.64.id]
atl.inst.64 <- atl.inst[atl.64.id]


### Race & Age

# Black 0-24

# SF
s1_main_sf_B24 <- s1_main_sf[[3]][which(get.vertex.attribute(s1_main_sf, "race") == "B" &
                                          get.vertex.attribute(s1_main_sf, "age.grp") == "1")]

sf.b24.id <- rep("NA", length(s1_main_sf_B24))

for (i in 1:length(s1_main_sf_B24)) {
  sf.b24.id[i] <- s1_main_sf_B24[[i]][2]
}

sf.b24.id <- paste0("V", sf.b24.id)

# Data sets of only black IDs
sf.all.B24 <- sf.all[sf.b24.id]
sf.main.B24 <- sf.main[sf.b24.id]
sf.casl.B24 <- sf.casl[sf.b24.id]
sf.inst.B24 <- sf.inst[sf.b24.id]

# ATL
s1_main_atl_B24 <- s1_main_atl[[3]][which(get.vertex.attribute(s1_main_atl, "race") == "B" &
                                            get.vertex.attribute(s1_main_atl, "age.grp") == "1")]

atl.b24.id <- rep("NA", length(s1_main_atl_B24))

for (i in 1:length(s1_main_atl_B24)) {
  atl.b24.id[i] <- s1_main_atl_B24[[i]][2]
}

atl.b24.id <- paste0("V", atl.b24.id)

# Data sets of only black IDs
atl.all.B24 <- atl.all[atl.b24.id]
atl.main.B24 <- atl.main[atl.b24.id]
atl.casl.B24 <- atl.casl[atl.b24.id]
atl.inst.B24 <- atl.inst[atl.b24.id]

# White 0-24

# SF
s1_main_sf_W24 <- s1_main_sf[[3]][which(get.vertex.attribute(s1_main_sf, "race") == "W" &
                                          get.vertex.attribute(s1_main_sf, "age.grp") == "1")]

sf.w24.id <- rep("NA", length(s1_main_sf_W24))

for (i in 1:length(s1_main_sf_W24)) {
  sf.w24.id[i] <- s1_main_sf_W24[[i]][2]
}

sf.w24.id <- paste0("V", sf.w24.id)

# Data sets of only black IDs
sf.all.W24 <- sf.all[sf.w24.id]
sf.main.W24 <- sf.main[sf.w24.id]
sf.casl.W24 <- sf.casl[sf.w24.id]
sf.inst.W24 <- sf.inst[sf.w24.id]

# ATL
s1_main_atl_W24 <- s1_main_atl[[3]][which(get.vertex.attribute(s1_main_atl, "race") == "W" &
                                            get.vertex.attribute(s1_main_atl, "age.grp") == "1")]

atl.w24.id <- rep("NA", length(s1_main_atl_W24))

for (i in 1:length(s1_main_atl_W24)) {
  atl.w24.id[i] <- s1_main_atl_W24[[i]][2]
}

atl.w24.id <- paste0("V", atl.w24.id)

# Data sets of only black IDs
atl.all.W24 <- atl.all[atl.w24.id]
atl.main.W24 <- atl.main[atl.w24.id]
atl.casl.W24 <- atl.casl[atl.w24.id]
atl.inst.W24 <- atl.inst[atl.w24.id]

# Black 25-34

# SF
s1_main_sf_B34 <- s1_main_sf[[3]][which(get.vertex.attribute(s1_main_sf, "race") == "B" &
                                          get.vertex.attribute(s1_main_sf, "age.grp") == "2")]

sf.b34.id <- rep("NA", length(s1_main_sf_B34))

for (i in 1:length(s1_main_sf_B34)) {
  sf.b34.id[i] <- s1_main_sf_B34[[i]][2]
}

sf.b34.id <- paste0("V", sf.b34.id)

# Data sets of only black IDs
sf.all.B34 <- sf.all[sf.b34.id]
sf.main.B34 <- sf.main[sf.b34.id]
sf.casl.B34 <- sf.casl[sf.b34.id]
sf.inst.B34 <- sf.inst[sf.b34.id]

# ATL
s1_main_atl_B34 <- s1_main_atl[[3]][which(get.vertex.attribute(s1_main_atl, "race") == "B" &
                                            get.vertex.attribute(s1_main_atl, "age.grp") == "2")]

atl.b34.id <- rep("NA", length(s1_main_atl_B34))

for (i in 1:length(s1_main_atl_B34)) {
  atl.b34.id[i] <- s1_main_atl_B34[[i]][2]
}

atl.b34.id <- paste0("V", atl.b34.id)

# Data sets of only black IDs
atl.all.B34 <- atl.all[atl.b34.id]
atl.main.B34 <- atl.main[atl.b34.id]
atl.casl.B34 <- atl.casl[atl.b34.id]
atl.inst.B34 <- atl.inst[atl.b34.id]

# White 25-34

# SF
s1_main_sf_W34 <- s1_main_sf[[3]][which(get.vertex.attribute(s1_main_sf, "race") == "W" &
                                          get.vertex.attribute(s1_main_sf, "age.grp") == "2")]

sf.w34.id <- rep("NA", length(s1_main_sf_W34))

for (i in 1:length(s1_main_sf_W34)) {
  sf.w34.id[i] <- s1_main_sf_W34[[i]][2]
}

sf.w34.id <- paste0("V", sf.w34.id)

# Data sets of only black IDs
sf.all.W34 <- sf.all[sf.w34.id]
sf.main.W34 <- sf.main[sf.w34.id]
sf.casl.W34 <- sf.casl[sf.w34.id]
sf.inst.W34 <- sf.inst[sf.w34.id]

# ATL
s1_main_atl_W34 <- s1_main_atl[[3]][which(get.vertex.attribute(s1_main_atl, "race") == "W" &
                                            get.vertex.attribute(s1_main_atl, "age.grp") == "2")]

atl.w34.id <- rep("NA", length(s1_main_atl_W34))

for (i in 1:length(s1_main_atl_W34)) {
  atl.w34.id[i] <- s1_main_atl_W34[[i]][2]
}

atl.w34.id <- paste0("V", atl.w34.id)

# Data sets of only black IDs
atl.all.W34 <- atl.all[atl.w34.id]
atl.main.W34 <- atl.main[atl.w34.id]
atl.casl.W34 <- atl.casl[atl.w34.id]
atl.inst.W34 <- atl.inst[atl.w34.id]

# Black 35-44

# SF
s1_main_sf_B44 <- s1_main_sf[[3]][which(get.vertex.attribute(s1_main_sf, "race") == "B" &
                                          get.vertex.attribute(s1_main_sf, "age.grp") == "3")]

sf.b44.id <- rep("NA", length(s1_main_sf_B44))

for (i in 1:length(s1_main_sf_B44)) {
  sf.b44.id[i] <- s1_main_sf_B44[[i]][2]
}

sf.b44.id <- paste0("V", sf.b44.id)

# Data sets of only black IDs
sf.all.B44 <- sf.all[sf.b44.id]
sf.main.B44 <- sf.main[sf.b44.id]
sf.casl.B44 <- sf.casl[sf.b44.id]
sf.inst.B44 <- sf.inst[sf.b44.id]

# ATL
s1_main_atl_B44 <- s1_main_atl[[3]][which(get.vertex.attribute(s1_main_atl, "race") == "B" &
                                            get.vertex.attribute(s1_main_atl, "age.grp") == "3")]

atl.b44.id <- rep("NA", length(s1_main_atl_B44))

for (i in 1:length(s1_main_atl_B44)) {
  atl.b44.id[i] <- s1_main_atl_B44[[i]][2]
}

atl.b44.id <- paste0("V", atl.b44.id)

# Data sets of only black IDs
atl.all.B44 <- atl.all[atl.b44.id]
atl.main.B44 <- atl.main[atl.b44.id]
atl.casl.B44 <- atl.casl[atl.b44.id]
atl.inst.B44 <- atl.inst[atl.b44.id]

# White 35-44

# SF
s1_main_sf_W44 <- s1_main_sf[[3]][which(get.vertex.attribute(s1_main_sf, "race") == "W" &
                                          get.vertex.attribute(s1_main_sf, "age.grp") == "3")]

sf.w44.id <- rep("NA", length(s1_main_sf_W44))

for (i in 1:length(s1_main_sf_W44)) {
  sf.w44.id[i] <- s1_main_sf_W44[[i]][2]
}

sf.w44.id <- paste0("V", sf.w44.id)

# Data sets of only black IDs
sf.all.W44 <- sf.all[sf.w44.id]
sf.main.W44 <- sf.main[sf.w44.id]
sf.casl.W44 <- sf.casl[sf.w44.id]
sf.inst.W44 <- sf.inst[sf.w44.id]

# ATL
s1_main_atl_W44 <- s1_main_atl[[3]][which(get.vertex.attribute(s1_main_atl, "race") == "W" &
                                            get.vertex.attribute(s1_main_atl, "age.grp") == "3")]

atl.w44.id <- rep("NA", length(s1_main_atl_W44))

for (i in 1:length(s1_main_atl_W44)) {
  atl.w44.id[i] <- s1_main_atl_W44[[i]][2]
}

atl.w44.id <- paste0("V", atl.w44.id)

# Data sets of only black IDs
atl.all.W44 <- atl.all[atl.w44.id]
atl.main.W44 <- atl.main[atl.w44.id]
atl.casl.W44 <- atl.casl[atl.w44.id]
atl.inst.W44 <- atl.inst[atl.w44.id]

# Black 45-54

# SF
s1_main_sf_B54 <- s1_main_sf[[3]][which(get.vertex.attribute(s1_main_sf, "race") == "B" &
                                          get.vertex.attribute(s1_main_sf, "age.grp") == "4")]

sf.b54.id <- rep("NA", length(s1_main_sf_B54))

for (i in 1:length(s1_main_sf_B54)) {
  sf.b54.id[i] <- s1_main_sf_B54[[i]][2]
}

sf.b54.id <- paste0("V", sf.b54.id)

# Data sets of only black IDs
sf.all.B54 <- sf.all[sf.b54.id]
sf.main.B54 <- sf.main[sf.b54.id]
sf.casl.B54 <- sf.casl[sf.b54.id]
sf.inst.B54 <- sf.inst[sf.b54.id]

# ATL
s1_main_atl_B54 <- s1_main_atl[[3]][which(get.vertex.attribute(s1_main_atl, "race") == "B" &
                                            get.vertex.attribute(s1_main_atl, "age.grp") == "4")]

atl.b54.id <- rep("NA", length(s1_main_atl_B54))

for (i in 1:length(s1_main_atl_B54)) {
  atl.b54.id[i] <- s1_main_atl_B54[[i]][2]
}

atl.b54.id <- paste0("V", atl.b54.id)

# Data sets of only black IDs
atl.all.B54 <- atl.all[atl.b54.id]
atl.main.B54 <- atl.main[atl.b54.id]
atl.casl.B54 <- atl.casl[atl.b54.id]
atl.inst.B54 <- atl.inst[atl.b54.id]

# White 45-54

# SF
s1_main_sf_W54 <- s1_main_sf[[3]][which(get.vertex.attribute(s1_main_sf, "race") == "W" &
                                          get.vertex.attribute(s1_main_sf, "age.grp") == "4")]

sf.w54.id <- rep("NA", length(s1_main_sf_W54))

for (i in 1:length(s1_main_sf_W54)) {
  sf.w54.id[i] <- s1_main_sf_W54[[i]][2]
}

sf.w54.id <- paste0("V", sf.w54.id)

# Data sets of only black IDs
sf.all.W54 <- sf.all[sf.w54.id]
sf.main.W54 <- sf.main[sf.w54.id]
sf.casl.W54 <- sf.casl[sf.w54.id]
sf.inst.W54 <- sf.inst[sf.w54.id]

# ATL
s1_main_atl_W54 <- s1_main_atl[[3]][which(get.vertex.attribute(s1_main_atl, "race") == "W" &
                                            get.vertex.attribute(s1_main_atl, "age.grp") == "4")]

atl.w54.id <- rep("NA", length(s1_main_atl_W54))

for (i in 1:length(s1_main_atl_W54)) {
  atl.w54.id[i] <- s1_main_atl_W54[[i]][2]
}

atl.w54.id <- paste0("V", atl.w54.id)

# Data sets of only black IDs
atl.all.W54 <- atl.all[atl.w54.id]
atl.main.W54 <- atl.main[atl.w54.id]
atl.casl.W54 <- atl.casl[atl.w54.id]
atl.inst.W54 <- atl.inst[atl.w54.id]

# Black 55+

# SF
s1_main_sf_B64 <- s1_main_sf[[3]][which(get.vertex.attribute(s1_main_sf, "race") == "B" &
                                          get.vertex.attribute(s1_main_sf, "age.grp") == "5")]

sf.b64.id <- rep("NA", length(s1_main_sf_B64))

for (i in 1:length(s1_main_sf_B64)) {
  sf.b64.id[i] <- s1_main_sf_B64[[i]][2]
}

sf.b64.id <- paste0("V", sf.b64.id)

# Data sets of only black IDs
sf.all.B64 <- sf.all[sf.b64.id]
sf.main.B64 <- sf.main[sf.b64.id]
sf.casl.B64 <- sf.casl[sf.b64.id]
sf.inst.B64 <- sf.inst[sf.b64.id]

# ATL
s1_main_atl_B64 <- s1_main_atl[[3]][which(get.vertex.attribute(s1_main_atl, "race") == "B" &
                                            get.vertex.attribute(s1_main_atl, "age.grp") == "5")]

atl.b64.id <- rep("NA", length(s1_main_atl_B64))

for (i in 1:length(s1_main_atl_B64)) {
  atl.b64.id[i] <- s1_main_atl_B64[[i]][2]
}

atl.b64.id <- paste0("V", atl.b64.id)

# Data sets of only black IDs
atl.all.B64 <- atl.all[atl.b64.id]
atl.main.B64 <- atl.main[atl.b64.id]
atl.casl.B64 <- atl.casl[atl.b64.id]
atl.inst.B64 <- atl.inst[atl.b64.id]

# White 55+

# SF
s1_main_sf_W64 <- s1_main_sf[[3]][which(get.vertex.attribute(s1_main_sf, "race") == "W" &
                                          get.vertex.attribute(s1_main_sf, "age.grp") == "5")]

sf.w64.id <- rep("NA", length(s1_main_sf_W64))

for (i in 1:length(s1_main_sf_W64)) {
  sf.w64.id[i] <- s1_main_sf_W64[[i]][2]
}

sf.w64.id <- paste0("V", sf.w64.id)

# Data sets of only black IDs
sf.all.W64 <- sf.all[sf.w64.id]
sf.main.W64 <- sf.main[sf.w64.id]
sf.casl.W64 <- sf.casl[sf.w64.id]
sf.inst.W64 <- sf.inst[sf.w64.id]

# ATL
s1_main_atl_W64 <- s1_main_atl[[3]][which(get.vertex.attribute(s1_main_atl, "race") == "W" &
                                            get.vertex.attribute(s1_main_atl, "age.grp") == "5")]

atl.w64.id <- rep("NA", length(s1_main_atl_W64))

for (i in 1:length(s1_main_atl_W64)) {
  atl.w64.id[i] <- s1_main_atl_W64[[i]][2]
}

atl.w64.id <- paste0("V", atl.w64.id)

# Data sets of only black IDs
atl.all.W64 <- atl.all[atl.w64.id]
atl.main.W64 <- atl.main[atl.w64.id]
atl.casl.W64 <- atl.casl[atl.w64.id]
atl.inst.W64 <- atl.inst[atl.w64.id]

# 3. tPath Analysis -------------------------------------------------------

par(mfrow = c(2, 2))

### All partnerships ###

## Plots

# Overall
matplot(samp.sf.all, type = "l", ylim = c(0, 10000), xlab = "time sequence", ylab = "FRP", main = "SF All Partnerships")
matplot(samp.atl.all, type = "l", ylim = c(0, 10000), xlab = "time sequence", ylab = "FRP", main = "ATL All Partnerships")

## Summary

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


## Plots

par(mfrow = c(2, 2))

# Overall
matplot(samp.sf.casl, type = "l", ylim = c(0, 10000), xlab = "Time Period",
        ylab = "Forward Reachable Path", main = "San Francisco Casual Partnerships")
# axis(1, 1:10, labels = c("1", "9", "17", "25", "33", "41", "49", "57", "65", "73", "81", "89", "97", "105",
#                          "113", "121", "129", "137", "145", "153", "161", "169", "177", "185", "193", "201",
#                          "209", "217", "225", "233", "241", "249", "257"), cex.axis = 1.5)
matplot(samp.atl.casl, type = "l",ylim = c(0, 10000), xlab = "Time Period",
        ylab = "Forward Reachable Path", main = "Atlanta Casual Partnerships")

matplot(x = seq(1, 260, 8), samp.sf.casl, type = "l", lty = 1, xlab = "Week")

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




























