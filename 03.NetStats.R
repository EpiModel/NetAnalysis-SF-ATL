
##
## Network stats calculator for ARTnet Data
##

## Packages ##
rm(list = ls())
library("tidyverse")
library("EpiModelHIV")
library("ARTnetData")
library("readxl")

## Inputs ##
city_name <- "Atlanta"
coef_name <- paste0("city2", city_name)
network_size <- 10000
diss_nodematch <- TRUE
edges_avg_nfrace <- FALSE


## Load Data ##
# fn.01 <- paste("data/artnet.EpiStats", gsub(" ", "", city_name), "rda", sep = ".")
# estats <- readRDS(file = fn.01)

fn.02 <- paste("data/artnet.NetParam", gsub(" ", "", city_name), "rda", sep = ".")
nstats <- readRDS(file = fn.02)


# Demographic Initialization ----------------------------------------------

out <- list()
out$demog <- list()

# Overall network size
num <- network_size

# Population size by race group
rdist <- as.data.frame(read_excel("RaceDistribution.xlsx"))
prop <- rdist[which(rdist$City == city_name), -1]/100
num.B <- out$demog$num.B <- round(num * prop$`Black + Hispanic`)
num.W <- out$demog$num.W <- round(num * prop$`White + Other`)


## Age-sex-specific mortality rates (B, H, W)
#    in 5-year age decrments starting with age 15
ages <- out$demog$ages <- 15:64
asmr.B <- c(0.00078, 0.00148, 0.00157, 0.00168, 0.00198,
            0.00254, 0.00376, 0.00628, 0.00999, 0.01533)
asmr.W <- c(0.00059, 0.00117, 0.00144, 0.00168, 0.00194,
            0.00249, 0.00367, 0.00593, 0.00881, 0.01255)

# transformed to weekly rates
trans.asmr.B <- 1 - (1 - asmr.B)^(1/52)
trans.asmr.W <- 1 - (1 - asmr.W)^(1/52)

# Null rate for 0-14, transformed rates, total rate for 65
vec.asmr.B <- c(rep(0, 14), rep(trans.asmr.B, each = 5), 1)
vec.asmr.W <- c(rep(0, 14), rep(trans.asmr.W, each = 5), 1)
asmr <- data.frame(age = 1:65, vec.asmr.B, vec.asmr.W)

out$demog$asmr <- asmr

out$demog$city <- gsub(" ", "", city_name)


# Nodal Attribute Initialization ------------------------------------------

out$attr <- list()

# age attributes
attr_age <- apportion_lr(num, ages, rep(1/length(ages), length(ages)), shuffled = TRUE)
out$attr$age <- attr_age

# attr_age <- runif(num, min = min(ages), max = max(ages) + (51/52))
# out$attr$age <- attr_age

age.breaks <- c(0, 24, 34, 44, 54, 64, 100)
attr_age.grp <- cut(attr_age, age.breaks, labels = FALSE)
out$attr$age.grp <- attr_age.grp

# race attribute
attr_race <- apportion_lr(num, c("B", "W"), c(num.B/num, num.W/num), shuffled = TRUE)
out$attr$race <- attr_race

# attr_race <- apportion_lr(num, 0:1, c(num.B/num, num.W/num), shuffled = TRUE)
# out$attr$race <- attr_race
 
# deg.pers attribute
attr_deg.pers <- apportion_lr(num, 0:3, nstats$main$deg.casl.dist, shuffled = TRUE)
out$attr$deg.pers <- attr_deg.pers

# attr_deg.casl <- apportion_lr(num, 0:3, nstats$main$deg.casl.dist, shuffled = TRUE)
# out$attr$deg.casl <- attr_deg.casl

# deg main attribute
attr_deg.main <- apportion_lr(num, 0:2, nstats$casl$deg.main.dist, shuffled = TRUE)
out$attr$deg.main <- attr_deg.main

# deg tot 3 attribute
attr_deg.tot <- apportion_lr(num, 0:3, nstats$inst$deg.tot.dist, shuffled = TRUE)
out$attr$deg.tot <- attr_deg.tot

# risk group
attr_risk.grp <- apportion_lr(num, 1:2, rep(0.5, 2), shuffled = TRUE)
out$attr$risk.grp <- attr_risk.grp

# attr_risk.grp <- apportion_lr(num, 1:5, rep(0.2, 5), shuffled = TRUE)
# out$attr$risk.grp <- attr_risk.grp

# role class
attr_role.class <- apportion_lr(num, c("I", "R", "V"), nstats$all$role.type, shuffled = TRUE)
out$attr$role.class <- attr_role.class

# attr_role.class <- apportion_lr(num, 0:2, nstats$all$role.type, shuffled = TRUE)
# out$attr$role.class <- attr_role.class

# 1. Main Partnership Stats --------------------------------------------------

out$main <- list()

# 1A: edges
if (edges_avg_nfrace == FALSE) {
  out$main$edges <- (nstats$main$md.main * num) / 2
} else {
  out$main$edges <- sum(unname(table(out$attr$race)) * nstats$main$nf.race)/2
}

# 1B: nodematch("age.grp")
out$main$nodematch_age.grp <- out$main$edges * 
  (unname(table(out$attr$age.grp))/num) * nstats$main$nm.age.grp

# 1C: nodefactor("age.grp")
out$main$nodefactor_age.grp <- unname(table(out$attr$age.grp)) * 
  nstats$main$nf.age.grp

# 1D: nodematch("race")
out$main$nodematch_race <- out$main$edges * nstats$main$nm.race

# 1E: nodefactor("race")
out$main$nodefactor_race <- unname(table(out$attr$race)) * nstats$main$nf.race

# 1F: nodefactor("deg.pers")
out$main$nodefactor_deg.pers <- num * nstats$main$deg.casl.dist * 
  nstats$main$md.main.pers

# 1G: concurrent
out$main$concurrent <- num * nstats$main$concurrent

# Dissolution
exp.mort <- (mean(trans.asmr.B) + mean(trans.asmr.W)) / 2
if (diss_nodematch == FALSE) {
  out$main$diss <- dissolution_coefs(dissolution = ~offset(edges),
                                     duration = 
                                       nstats$main$durs.main.homog$mean.dur.adj,
                                     d.rate = exp.mort)
} else {
  out$main$diss <- dissolution_coefs(dissolution = ~offset(edges) + 
                                     offset(nodematch("age.grp", diff = TRUE)),
                                     duration = 
                                       nstats$main$durs.main.byage$mean.dur.adj,
                                     d.rate = exp.mort)
}


# # 1B: nodefactor("age.grp")
# nodefactor_age.grp <- table(out$attr$age.grp) * nstats$main$nf.age.grp
# out$main$nodefactor_age.grp <- unname(nodefactor_age.grp)
# 
# # 1C: nodematch("age.grp")
# nodematch_age.grp <- nodefactor_age.grp/2 * nstats$main$nm.age.grp
# out$main$nodematch_age.grp <- unname(nodematch_age.grp)
# 
# # 1D: nodefactor("race")
# nodefactor_race <- table(out$attr$race) * nstats$main$nf.race
# out$main$nodefactor_race <- unname(nodefactor_race)
# 
# # 1E: nodematch("race")
# nodematch_race <- nodefactor_race/2 * nstats$main$nm.race
# out$main$nodematch_race <- unname(nodematch_race)
# 
# # 1Eb: nodematch("race", diff = FALSE)
# nodematch_race <- out$main$edges * nstats$main$nm.race_diffF
# out$main$nodematch_race_diffF <- unname(nodematch_race)
# 
# # 1F: nodefactor("deg.casl")
# out$main$nodefactor_deg.casl <- num * nstats$main$deg.casl.dist * nstats$main$nf.deg.casl
# 
# # 1G: concurrent
# out$main$concurrent <- num * nstats$main$concurrent
# 
# # 1H: nodefactor("diag.status")
# nodefactor_diag.status <- table(out$attr$diag.status) * nstats$main$nf.diag.status
# out$main$nodefactor_diag.status <- unname(nodefactor_diag.status)
# 
# # Dissolution
# exp.mort <- (mean(trans.asmr.B) + mean(trans.asmr.H) + mean(trans.asmr.W)) / 3
# if (diss_nodematch == FALSE) {
#   out$main$diss <- dissolution_coefs(dissolution = ~offset(edges),
#                                      duration = nstats$main$durs.main.homog$mean.dur.adj,
#                                      d.rate = exp.mort)
# } else {
#   out$main$diss <- dissolution_coefs(dissolution = ~offset(edges) + offset(nodematch("age.grp", diff = TRUE)),
#                                      duration = nstats$main$durs.main.byage$mean.dur.adj,
#                                      d.rate = exp.mort)
# }


# 2. Casual Partnership Stats ------------------------------------------------

out$casl <- list()

# 2A: edges
if (edges_avg_nfrace == FALSE) {
  out$casl$edges <- (nstats$casl$md.casl * num) / 2
} else {
  out$casl$edges <- sum(unname(table(out$attr$race)) * nstats$casl$nf.race)/2
}

# 2B: nodematch("age.grp")
out$casl$nodematch_age.grp <- out$casl$edges * 
  (unname(table(out$attr$age.grp))/num) * nstats$casl$nm.age.grp

# 2C: nodefactor("age.grp")
out$casl$nodefactor_age.grp <- unname(table(out$attr$age.grp)) * 
  nstats$casl$nf.age.grp

# 2D: nodematch("race")
out$casl$nodematch_race <- out$casl$edges * nstats$casl$nm.race

# 2E: nodefactor("race")
out$casl$nodefactor_race <- unname(table(out$attr$race)) * nstats$casl$nf.race

# 2F: nodefactor("deg.main")
out$casl$nodefactor_deg.main <- num * nstats$casl$deg.main.dist * 
  nstats$casl$md.pers.main

# sG: concurrent
out$casl$concurrent <- num * nstats$casl$concurrent

# Dissolution
exp.mort <- (mean(trans.asmr.B) + mean(trans.asmr.W)) / 2
if (diss_nodematch == FALSE) {
  out$casl$diss <- dissolution_coefs(dissolution = ~offset(edges),
                                     duration = 
                                       nstats$casl$durs.main.homog$mean.dur.adj,
                                     d.rate = exp.mort)
} else {
  out$casl$diss <- dissolution_coefs(dissolution = ~offset(edges) + 
                                     offset(nodematch("age.grp", diff = TRUE)),
                                     duration = 
                                       nstats$casl$durs.casl.byage$mean.dur.adj,
                                     d.rate = exp.mort)
}

# 
# # 2A: edges
# if (edges_avg_nfrace == FALSE) {
#   out$casl$edges <- (nstats$casl$md.casl * num) / 2
# } else {
#   out$casl$edges <- sum(unname(table(out$attr$race)) * nstats$casl$nf.race)/2
# }
# 
# # 2B: nodefactor("age.grp")
# nodefactor_age.grp <- table(out$attr$age.grp) * nstats$casl$nf.age.grp
# out$casl$nodefactor_age.grp <- unname(nodefactor_age.grp)
# 
# # 2C: nodematch("age.grp")
# nodematch_age.grp <- nodefactor_age.grp/2 * nstats$casl$nm.age.grp
# out$casl$nodematch_age.grp <- unname(nodematch_age.grp)
# 
# # 2D: nodefactor("race")
# nodefactor_race <- table(out$attr$race) * nstats$casl$nf.race
# out$casl$nodefactor_race <- unname(nodefactor_race)
# 
# # 2E: nodematch("race")
# nodematch_race <- nodefactor_race/2 * nstats$casl$nm.race
# out$casl$nodematch_race <- unname(nodematch_race)
# 
# # 2Eb: nodematch("race", diff = FALSE)
# nodematch_race <- out$casl$edges * nstats$casl$nm.race_diffF
# out$casl$nodematch_race_diffF <- unname(nodematch_race)
# 
# 
# # 2F: nodefactor("deg.main")
# out$casl$nodefactor_deg.main <- num * nstats$casl$deg.main.dist * nstats$casl$nf.deg.main
# 
# # 2G: concurrent
# out$casl$concurrent <- num * nstats$casl$concurrent
# 
# # 2H: nodefactor("diag.status")
# nodefactor_diag.status <- table(out$attr$diag.status) * nstats$casl$nf.diag.status
# out$casl$nodefactor_diag.status <- unname(nodefactor_diag.status) 
# 
# # Dissolution
# if (diss_nodematch == FALSE) {
#   out$casl$diss <- dissolution_coefs(dissolution = ~offset(edges),
#                                      duration = nstats$casl$durs.casl.homog$mean.dur.adj,
#                                      d.rate = exp.mort)
# } else {
#   out$casl$diss <- dissolution_coefs(dissolution = ~offset(edges) + offset(nodematch("age.grp", diff = TRUE)),
#                                      duration = nstats$casl$durs.casl.byage$mean.dur.adj,
#                                      d.rate = exp.mort)
# }


# 3. One-Off Partnership Stats -----------------------------------------------

out$inst <- list()

# 3A: edges
if (edges_avg_nfrace == FALSE) {
  out$inst$edges <- (nstats$inst$md.inst * num) / 2
} else {
  out$inst$edges <- sum(unname(table(out$attr$race)) * nstats$inst$nf.race)/2
}

# 3B: nodematch("age.grp", diff = FALSE)
out$inst$nodematch_age.grp <- sum(out$inst$edges * 
                                    (unname(table(out$attr$age.grp))/num) * 
                                    nstats$inst$nm.age.grp)

# 3C: nodefactor("age.grp")
out$inst$nodefactor_age.grp <- unname(table(out$attr$age.grp)) * 
  nstats$inst$nf.age.grp

# 3D: nodematch("race", diff = FALSE)
out$inst$nodematch_race <- out$inst$edges * nstats$inst$nm.race

# 3E: nodefactor("race")
out$inst$nodefactor_race <- unname(table(out$attr$race)) * nstats$inst$nf.race

# 3F: nodefactor(c("risk.grp", "tot.deg3"))

# low risk // deg 0, 1, 2, 3; high risk // deg 0, 1, 2, 3
table(attr_deg.tot, attr_risk.grp)
as.numeric(table(attr_deg.tot, attr_risk.grp))

nstats$inst$nf.risk.deg
as.numeric(nstats$inst$nf.risk.deg)

out$inst$nodefactor_deg.tot.risk <- as.numeric(table(attr_deg.tot, 
                                                     attr_risk.grp)) *
  as.numeric(nstats$inst$nf.risk.deg)


# 
# # 3A: edges
# if (edges_avg_nfrace == FALSE) {
#   out$inst$edges <- (nstats$inst$md.inst * num) / 2
# } else {
#   out$inst$edges <- sum(unname(table(out$attr$race)) * nstats$inst$nf.race)/2
# }
# 
# # 3B: nodefactor("age.grp")
# nodefactor_age.grp <- table(out$attr$age.grp) * nstats$inst$nf.age.grp
# out$inst$nodefactor_age.grp <- unname(nodefactor_age.grp)
# 
# # 3C: nodematch("age.grp")
# nodematch_age.grp <- nodefactor_age.grp/2 * nstats$inst$nm.age.grp
# out$inst$nodematch_age.grp <- unname(nodematch_age.grp)
# 
# # 3D: nodefactor("race")
# nodefactor_race <- table(out$attr$race) * nstats$inst$nf.race
# out$inst$nodefactor_race <- unname(nodefactor_race)
# 
# # 3E: nodematch("race")
# nodematch_race <- nodefactor_race/2 * nstats$inst$nm.race
# out$inst$nodematch_race <- unname(nodematch_race)
# 
# # 3Eb: nodematch("race", diff = FALSE)
# nodematch_race <- out$inst$edges * nstats$inst$nm.race_diffF
# out$inst$nodematch_race_diffF <- unname(nodematch_race)
# 
# # 3F: nodefactor("risk.grp")
# nodefactor_risk.grp <- table(out$attr$risk.grp) * nstats$inst$nf.risk.grp
# out$inst$nodefactor_risk.grp <- unname(nodefactor_risk.grp)
# 
# # 3G: nodefactor("deg.tot")
# nodefactor_deg.tot <- table(out$attr$deg.tot) * nstats$inst$nf.deg.tot
# out$inst$nodefactor_deg.tot <- unname(nodefactor_deg.tot) * nstats$inst$nf.deg.tot
# 
# # 3H: nodefactor("diag.status")
# nodefactor_diag.status <- table(out$attr$diag.status) * nstats$inst$nf.diag.status
# out$inst$nodefactor_diag.status <- unname(nodefactor_diag.status)


# Save Out File -----------------------------------------------------------
fns <- strsplit(fn.02, "[.]")[[1]]
fn.new <- paste(fns[1], "NetStats", fns[3], "rda", sep = ".")

saveRDS(out, file = fn.new)

