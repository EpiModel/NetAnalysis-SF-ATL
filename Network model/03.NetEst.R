
##
## Network modeling for ARTnet Data
##

## Packages ##
rm(list = ls())
suppressMessages(library("EpiModelHIV"))

## Inputs ##
city_name <- "Atlanta"

## Load Data ##
fn <- paste("data/artnet.NetStats", gsub(" ", "", city_name), "rda", sep = ".")
tstats <- readRDS(file = fn)


# 0. Initialize Network ---------------------------------------------------

num.B <- tstats$demog$num.B
num.W <- tstats$demog$num.W
num <- num.B + num.W
nw <- network::network.initialize(num, directed = FALSE)

attr.names <- names(tstats$attr)
attr.values <- tstats$attr
nw <- network::set.vertex.attribute(nw, attr.names, attr.values)
nw_main <- nw_casl <- nw_inst <- nw


# 1. Main Model -----------------------------------------------------------

# Formula
model_main <- ~edges +
  nodematch("age.grp", diff = TRUE) +
  nodefactor("age.grp", base = 1) +
  nodematch("race") +
  nodefactor("race", base = 1) +
  nodefactor("deg.pers", base = 1) +
  concurrent +
  degrange(from = 3) +
  nodematch("role.class", diff = TRUE, keep = 1:2)

# Target Stats
tstats_main <- c(
  edges = tstats$main$edges,
  nodematch_age.grp = tstats$main$nodematch_age.grp,
  nodefactor_age.grp = tstats$main$nodefactor_age.grp[-1],
  nodematch_race = tstats$main$nodematch_race,
  nodefactor_race = tstats$main$nodefactor_race[-1],
  nodefactor_deg.pers = tstats$main$nodefactor_deg.pers[-1],
  concurrent = tstats$main$concurrent,
  degrange = 0,
  nodematch_role.class = c(0, 0)
)

cbind(tstats_main)
tstats_main <- unname(tstats_main)

# Fit model
fit_main <- netest(nw_main,
                   formation = model_main,
                   target.stats = tstats_main,
                   coef.diss = tstats$main$diss,
                   set.control.ergm = control.ergm(MCMLE.maxit = 500,
                                                   SAN.maxit = 2,
                                                   SAN.nsteps.times = 2),
                   verbose = TRUE)


# 2. Casual Model ---------------------------------------------------------

# Formula
model_casl <- ~edges +
  nodematch("age.grp", diff = TRUE) +
  nodefactor("age.grp", base = 3) +
  nodematch("race") +
  nodefactor("race", base = 1) +
  nodefactor("deg.main", base = 3) +
  concurrent +
  degrange(from = 4) +
  nodematch("role.class", diff = TRUE, keep = 1:2)

# Target Stats
tstats_casl <- c(
  edges = tstats$casl$edges,
  nodematch_age.grp = tstats$casl$nodematch_age.grp,
  nodefactor_age.grp = tstats$casl$nodefactor_age.grp[-3],
  nodematch_race = tstats$casl$nodematch_race,
  nodefactor_race = tstats$casl$nodefactor_race[-1],
  nodefactor_deg.main = tstats$casl$nodefactor_deg.main[-3],
  concurrent = tstats$casl$concurrent,
  degrange = 0,
  nodematch_role.class = c(0, 0)
)

cbind(tstats_casl)
tstats_casl <- unname(tstats_casl)

# Fit model
fit_casl <- netest(nw_casl,
                   formation = model_casl,
                   target.stats = tstats_casl,
                   coef.diss = tstats$casl$diss,
                   set.control.ergm = control.ergm(MCMLE.maxit = 500,
                                                   SAN.maxit = 10,
                                                   SAN.nsteps.times = 10),
                   verbose = TRUE)


# 3. One-Off Model --------------------------------------------------------

# Formula
model_inst <- ~edges +
  nodematch("age.grp", diff = FALSE) +
  nodefactor("age.grp", base = 1) +
  nodematch("race") +
  nodefactor("race", base = 1) +
  nodefactor(c("risk.grp", "deg.tot"), base = 8) +
  nodematch("role.class", diff = TRUE, keep = 1:2)

# Target Stats
tstats_inst <- c(
  edges = tstats$inst$edges,
  nodematch_age.grp = sum(tstats$inst$nodematch_age.grp),
  nodefactor_age.grp = tstats$inst$nodefactor_age.grp[-1],
  nodematch_race = tstats$inst$nodematch_race,
  nodefactor_race = tstats$inst$nodefactor_race[-1],
  nodefactor_deg.tot.risk = tstats$inst$nodefactor_deg.tot.risk[-8],
  nodematch_role.class = c(0, 0)
)

cbind(tstats_inst)
tstats_inst <- unname(tstats_inst)

# Fit model
fit_inst <- netest(nw_inst,
                   formation = model_inst,
                   target.stats = tstats_inst,
                   coef.diss = dissolution_coefs(~offset(edges), 1),
                   set.control.ergm = control.ergm(MCMLE.maxit = 500,
                                                   SAN.maxit = 2,
                                                   SAN.nsteps.times = 2),
                   verbose = TRUE)


# 4. Save Data ------------------------------------------------------------

out <- list(fit_main, fit_casl, fit_inst)

fns <- strsplit(fn, "[.]")[[1]]
fn.new <- paste(fns[1], "NetEst", fns[3], "rda", sep = ".")

saveRDS(out, file = fn.new)


