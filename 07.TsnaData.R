
## 
## Outcome data for tsna analysis of San Francisco & Atlanta sexual networks
## 

## Packages ##
# library("tsna")
# library("scales")
library("networkDynamicData")
# suppressMessages(library("EpiModel"))


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

# END script here ---------------------------------------------------------


# 7.Distribution of FRPs --------------------------------------------------

# par(mfrow = c(1, 2), oma = c(2, 0, 2, 0), xpd = NA)
par(mfrow = c(1, 2))
# pal <- adjustcolor(RColorBrewer::brewer.pal(5, "Set1"), alpha.f = 0.6)
matplot(sfa.frp/10000, type = "l", xlab = "Week", ylab = "FRP",
        col = alpha("red", 0.7))
matplot(atla.frp/10000, type = "l", xlab = "Week", ylab = "FRP",
        col = alpha("blue", 0.7))

par(mfrow = c(1, 2))
# pal <- adjustcolor(RColorBrewer::brewer.pal(5, "Set1"), alpha.f = 0.6)
matplot(sfa.frp/10000, type = "l", xlab = "Week", ylab = "FRP")
matplot(atla.frp/10000, type = "l", xlab = "Week", ylab = "FRP")

matplot(sfc.frp/10000, type = "l", xlab = "Week", ylab = "FRP")
matplot(atlc.frp/10000, type = "l", xlab = "Week", ylab = "FRP")

plot(x = 1:260, y = sfa.frp.av, type = "l", col = alpha("red", 0.7), lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable")
lines(x = 1:260, y = atla.frp.av, type = "l", col = alpha("blue", 0.7), 
      lwd = 2)
legend("bottomright", legend = c("San Francisco", "Atlanta"), 
       col = c("red", "blue"), lty = 1, cex(1.5))


#title()

library("viridis")
library("wesanderson")
palv <- viridis(n = 4, alpha = 0.25, option = "inferno")
palv <- adjustcolor(wes_palette(5, name = "Zissou1"), alpha.f = 1)
palv <- rainbow(10)

## All partnerships
# jpeg("Plot2.jpeg", width = 8, height = 4, units = 'in', res = 250)
par(mfrow = c(2, 3), oma = c(0, 0, 0, 0), xpd = NA, mgp = c(2,1,0), 
    mar = c(3,3,2,1))
# SF
matplot(sfm.frp, type = "l", ylim = c(0, 30), xlab = "", ylab = "FRP", lty = 1,
        col = palv, lwd = 0.1, main = "SF Main")
matplot(sfc.frp, type = "l", ylim = c(0, 10000), xlab = "", ylab = "", lty = 1,
        col = palv, lwd = 0.1, main = "SF Casual")
matplot(sfi.frp, type = "l", ylim = c(0, 10000), xlab = "", ylab = "", lty = 1,
        col = palv, lwd = 0.1, main = "SF One-Time")

# ATL
matplot(atlm.frp, type = "l", ylim = c(0, 30), xlab = "Week", ylab = "FRP", lty = 1,
        col = palv, lwd = 0.1, main = "ATL Main")
matplot(atlc.frp, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "", lty = 1,
        col = palv, lwd = 0.1, main = "ATL Casual")
matplot(atli.frp, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "", lty = 1,
        col = palv, lwd = 0.1, main = "ATL One-Time")

dev.off()

# title("Distribution of 5-Year Foward Reachable Paths by Partnership Type", 
      # outer = TRUE)








