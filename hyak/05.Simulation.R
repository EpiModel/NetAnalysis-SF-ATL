
##
## Network simulation for ART-Net Data
## v1: 2018-08
##

## Packages ##
rm(list = ls())
suppressMessages(library("EpiModelHIV"))


## Inputs ##
city <- Sys.getenv("CITY")
if (city == "A") {
  city_name <- "Atlanta"
} else {
  city_name <- "San Francisco"
}


## Load Data ##
fn <- paste("data/artnet.NetEst", gsub(" ", "", city_name), "rda", sep = ".")
est <- readRDS(file = fn)


## Dynamic network sim

sim_network <- function(est, nsteps = 52*5) {

  # Init network sim
  nw <- list()
  for (i in 1:3) {
    x <- est[[i]]
    nw[[i]] <- simulate(x$fit, control = control.simulate.ergm(MCMC.burnin = 2e5))
  }

  # Dynamic time loop
  for (at in 1:nsteps) {
    # Main #
    nw[[1]] <- suppressWarnings(simulate(nw[[1]],
                        formation = est[[1]]$formation,
                        dissolution = est[[1]]$coef.diss$dissolution,
                        coef.form = est[[1]]$coef.form,
                        coef.diss = est[[1]]$coef.diss$coef.crude,
                        time.start = at,
                        time.slices = 1,
                        time.offset = 0,
                        monitor = "all",
                        output = "networkDynamic"))

    deg_dist_main <- as.numeric(summary(nw[[1]] ~ sociality(base = 0), at = at))
    nw[[2]] <- set.vertex.attribute(nw[[2]], attrname = "deg.main", value = deg_dist_main)
    nw[[3]] <- set.vertex.attribute(nw[[3]], attrname = "deg.main", value = deg_dist_main)

    # Casual #
    nw[[2]] <- suppressWarnings(simulate(nw[[2]],
                        formation = est[[2]]$formation,
                        dissolution = est[[2]]$coef.diss$dissolution,
                        coef.form = est[[2]]$coef.form,
                        coef.diss = est[[2]]$coef.diss$coef.crude,
                        time.start = at,
                        time.slices = 1,
                        time.offset = 0,
                        monitor = "all",
                        output = "networkDynamic"))

    deg_dist_casl <- as.numeric(summary(nw[[2]] ~ sociality(base = 0), at = at))
    nw[[1]] <- set.vertex.attribute(nw[[1]], attrname = "deg.pers", value = deg_dist_casl)
    nw[[3]] <- set.vertex.attribute(nw[[3]], attrname = "deg.pers", value = deg_dist_casl)

    # One-Off #
    nw[[3]] <- suppressWarnings(simulate(nw[[3]],
                        formation = est[[3]]$formation,
                        dissolution = est[[3]]$coef.diss$dissolution,
                        coef.form = est[[3]]$coef.form,
                        coef.diss = est[[3]]$coef.diss$coef.crude,
                        time.start = at,
                        time.slices = 1,
                        time.offset = 0,
                        monitor = "all",
                        output = "networkDynamic"))

    # summary(nw[[3]] ~ nodematch("age.grp", diff = FALSE))

    cat("\n Step ", at, "/", nsteps)
  }

  return(nw)
}
# out <- sim_network(est, nsteps = 260)


## Full parallel stack
library(foreach)
library(doParallel)

nsims <- 1
nsteps <- 52*5

cluster.size <- nsims
registerDoParallel(cluster.size)

out <- foreach(i = 1:nsims) %dopar% {
  sim_network(est, nsteps = nsteps)
}

fns <- strsplit(fn, "[.]")[[1]]
fn.new <- paste(fns[1], "NetSim", fns[3], "rda", sep = ".")
saveRDS(out, file = fn.new)
