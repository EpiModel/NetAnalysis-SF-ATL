
##
## Network modeling diagnostics for ART-Net Data
## v1: 2018-08
##

## Packages ##
rm(list = ls())
suppressMessages(library("EpiModelHIV"))


## Inputs ##
city_name <- "San Francisco"


## Load Data ##
fn <- paste("data/artnet.NetEst", gsub(" ", "", city_name), "rda", sep = ".")
est <- readRDS(file = fn)


# Main --------------------------------------------------------------------

fit_main <- est[[1]]
# mcmc.diagnostics(fit_main$fit, vars.per.page = 8)

model_main_dx <- ~edges +
  nodematch("age.grp", diff = TRUE) +
  nodefactor("age.grp", base = 0) +
  nodematch("race") +
  nodefactor("race", base = 0) +
  nodefactor("deg.pers", base = 0) +
  degrange(from = 3) +
  concurrent +
  nodematch("role.class", diff = TRUE, keep = 1:2) +
  degree(0:3)
dx_main <- netdx(fit_main, nsims = 20, ncores = 12, nsteps = 500,
            nwstats.formula = model_main_dx,
            set.control.ergm = control.simulate.ergm(MCMC.burnin = 1e5))
print(dx_main)
rm(dx_main); gc()
# plot(dx_main)


# Casual ------------------------------------------------------------------

fit_casl <- est[[2]]
# mcmc.diagnostics(fit_main$fit, vars.per.page = 8)

model_casl_dx <- ~edges +
  nodematch("age.grp", diff = TRUE) +
  nodefactor("age.grp", base = 0) +
  nodematch("race") +
  nodefactor("race", base = 0) +
  nodefactor("deg.main", base = 0) +
  degrange(from = 4) +
  concurrent +
  nodematch("role.class", diff = TRUE, keep = 1:2) +
  degree(0:4)
dx_casl <- netdx(fit_casl, nsims = 20, ncores = 12, nsteps = 500,
            nwstats.formula = model_casl_dx,
            set.control.ergm = control.simulate.ergm(MCMC.burnin = 1e5))
print(dx_casl)
rm(dx_casl); gc()
# plot(dx_casl)

dx_casl <- netdx(fit_casl, nsims = 5000, dynamic = FALSE,
                 nwstats.formula = model_casl_dx,
                 set.control.ergm = control.simulate.ergm(MCMC.burnin = 1e5))
print(dx_casl)

# One-Off -----------------------------------------------------------------

fit_inst <- est[[3]]
# mcmc.diagnostics(fit_inst$fit, vars.per.page = 8)

model_inst_dx <- ~edges +
  nodematch("age.grp", diff = FALSE) +
  nodefactor("age.grp", base = 0) +
  nodematch("race") +
  nodefactor("race", base = 0) +
  nodefactor(c("risk.grp", "deg.tot"), base = 0) +
  nodefactor("risk.grp", base = 0) +
  nodematch("role.class", diff = TRUE, keep = 1:2) +
  degree(0:4)
dx_inst <- netdx(fit_inst, nsims = 5000, dynamic = FALSE,
            nwstats.formula = model_inst_dx,
            set.control.ergm = control.simulate.ergm(MCMC.burnin = 1e5))
print(dx_inst)
