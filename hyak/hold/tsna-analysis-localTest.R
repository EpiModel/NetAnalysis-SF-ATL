## TSNA Analysis
## 9/7/2018

library("tsna")
library("networkDynamicData")
library("sna")


# 0.Net Processing --------------------------------------------------------

### SF ###

## Bind networks ##

sim <- readRDS("data/artnet.NetSim.SanFrancisco.rda")
sim <- readRDS("data/artnet.NetSim.Atlanta.rda")

# first sim
s1 <- sim[[1]]

# nD objects within first sim
s1_main <- s1[[1]]
s1_casl <- s1[[2]]
s1_inst <- s1[[3]]

# Add edge attribute for partnership type to main network
set.edge.attribute(s1_main, "edge.type", value = 1)

# Update main network to serve as master network
s1_all <- s1_main

# Convert casual and one-off nets to data frames
s1_casl_df <- as.data.frame(s1_casl)
head(s1_casl_df, 25)

s1_inst_df <- as.data.frame(s1_inst)
head(s1_inst_df, 25)

# Add edges for casual and one-offs to master network
add.edges.active(s1_all, tail = s1_casl_df[[3]], head = s1_casl_df[[4]], names.eval = "edge.type",
                 vals.eval = 2, onset = s1_casl_df[[1]], terminus = s1_casl_df[[2]])

add.edges.active(s1_all, tail = s1_inst_df[[3]], head = s1_inst_df[[4]], names.eval = "edge.type",
                 vals.eval = 3, onset = s1_inst_df[[1]], terminus = s1_inst_df[[2]])

# Confirming edges and att added correctly
test <- get.edge.attribute(s1_all$mel, "edge.type")
table(test)

tmp <- tPath(s1_all, v = 50)
df <- data.frame(tmp$tdist, tmp$gsteps)
df[df$tmp.tdist == Inf, ]
head(df, 250)

tmp <- tPath(s1_casl, v = 50, start = 1, end = 52)
sum(tmp$tdist<Inf)
mean(tmp$tdist[tmp$tdist<Inf])
mean(tmp$gsteps[tmp$gsteps<Inf])

sapply(50, function(x) sum(tPath(s1_all, v = x)$tdist<Inf))
sapply(327, function(x) sum(tPath(s1_all, v = x)$tdist<Inf))

### ATL ###


# 1.Net Analysis ----------------------------------------------------------

## Forward Reachable Set ##

# treach()

frs <- tReach(s1_all)

tReach(s1_casl, start = 1, end = 260, sample = 20)

