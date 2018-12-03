
##
## Network stats calculator for ART-Net Data
## v1: 2018-08
##

## Packages ##
rm(list = ls())
library("tidyverse")
library("readxl")
suppressPackageStartupMessages(library("EpiModelHIV"))


## Inputs ##
city_name <- "Atlanta"
network_size <- 10000
diss_nodematch <- TRUE
edges_avg_nfrace <- TRUE


## Load Data ##
fn <- paste("data/artnet.NetParam", gsub(" ", "", city_name), "rda", sep = ".")
stats <- readRDS(file = fn)


# Demographic Initialization ----------------------------------------------

out <- list()
out$demog <- list()

# Overall network size
num <- network_size

# Population size by race group
# B = Black, Hispanic, Other
# W = White, Asian
rdist <- as.data.frame(read_excel("RaceDistribution.xlsx"))
prop.w <- rdist[which(rdist$City == city_name), 2]/100
num.W <- out$demog$num.W <- round(num * prop.w)
num.B <- out$demog$num.B <- num - num.W

## Age-sex-specific mortality rates (B, W)
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


# Nodal Attribute Initialization ------------------------------------------

out$attr <- list()

# age attribute
attr_age <- apportion_lr(num, ages, rep(1/length(ages), length(ages)), shuffled = TRUE)
out$attr$age <- attr_age

age.breaks <- c(0, 24, 34, 44, 54, 64, 100)
attr_age.grp <- cut(attr_age, age.breaks, labels = FALSE)
out$attr$age.grp <- attr_age.grp

# race attribute
attr_race <- apportion_lr(num, c("B", "W"), c(num.B/num, num.W/num), shuffled = TRUE)
out$attr$race <- attr_race

# deg.pers attribute
attr_deg.pers <- apportion_lr(num, 0:3, stats$main$deg.casl.dist, shuffled = TRUE)
out$attr$deg.pers <- attr_deg.pers

# deg main attribute
attr_deg.main <- apportion_lr(num, 0:2, stats$casl$deg.main.dist, shuffled = TRUE)
out$attr$deg.main <- attr_deg.main

# deg tot 3 attribute
attr_deg.tot <- apportion_lr(num, 0:3, stats$inst$deg.tot.dist , shuffled = TRUE)
out$attr$deg.tot <- attr_deg.tot

# risk group
attr_risk.grp <- apportion_lr(num, 1:2, rep(0.5, 2), shuffled = TRUE)
out$attr$risk.grp <- attr_risk.grp

# role class
attr_role.class <- apportion_lr(num, c("I", "R", "V"), stats$all$role.type, shuffled = TRUE)
out$attr$role.class <- attr_role.class


# 1. Main Partnership Stats --------------------------------------------------

out$main <- list()

# 1A: edges
if (edges_avg_nfrace == FALSE) {
  out$main$edges <- (stats$main$md.main * num) / 2
} else {
  out$main$edges <- sum(unname(table(out$attr$race)) * stats$main$nf.race)/2
}

# 1B: nodematch("age.grp")
out$main$nodematch_age.grp <- out$main$edges * (unname(table(out$attr$age.grp))/num) * stats$main$nm.age.grp

# 1C: nodefactor("age.grp")
out$main$nodefactor_age.grp <- unname(table(out$attr$age.grp)) * stats$main$nf.age.grp

# 1D: nodematch("race")
out$main$nodematch_race <- out$main$edges * stats$main$nm.race

# 1E: nodefactor("race")
out$main$nodefactor_race <- unname(table(out$attr$race)) * stats$main$nf.race

# 1F: nodefactor("deg.pers")
out$main$nodefactor_deg.pers <- num * stats$main$deg.casl.dist * stats$main$md.main.pers

# 1G: concurrent
out$main$concurrent <- num * stats$main$concurrent

# Dissolution
exp.mort <- (mean(trans.asmr.B) + mean(trans.asmr.W)) / 2
if (diss_nodematch == FALSE) {
  out$main$diss <- dissolution_coefs(dissolution = ~offset(edges),
                                     duration = stats$main$durs.main.homog$mean.dur.adj,
                                     d.rate = exp.mort)
} else {
  out$main$diss <- dissolution_coefs(dissolution = ~offset(edges) + offset(nodematch("age.grp", diff = TRUE)),
                                     duration = stats$main$durs.main.byage$mean.dur.adj,
                                     d.rate = exp.mort)
}



# 2. Casual Partnership Stats ------------------------------------------------

out$casl <- list()

# 2A: edges
if (edges_avg_nfrace == FALSE) {
  out$casl$edges <- (stats$casl$md.casl * num) / 2
} else {
  out$casl$edges <- sum(unname(table(out$attr$race)) * stats$casl$nf.race)/2
}

# 2B: nodematch("age.grp")
out$casl$nodematch_age.grp <- out$casl$edges * (unname(table(out$attr$age.grp))/num) * stats$casl$nm.age.grp

# 2C: nodefactor("age.grp")
out$casl$nodefactor_age.grp <- unname(table(out$attr$age.grp)) * stats$casl$nf.age.grp

# 2D: nodematch("race")
out$casl$nodematch_race <- out$casl$edges * stats$casl$nm.race

# 2E: nodefactor("race")
out$casl$nodefactor_race <- unname(table(out$attr$race)) * stats$casl$nf.race

# 2F: nodefactor("deg.main")
out$casl$nodefactor_deg.main <- num * stats$casl$deg.main.dist * stats$casl$md.pers.main

# sG: concurrent
out$casl$concurrent <- num * stats$casl$concurrent

# Dissolution
exp.mort <- (mean(trans.asmr.B) + mean(trans.asmr.W)) / 2
if (diss_nodematch == FALSE) {
  out$casl$diss <- dissolution_coefs(dissolution = ~offset(edges),
                                     duration = stats$casl$durs.main.homog$mean.dur.adj,
                                     d.rate = exp.mort)
} else {
  out$casl$diss <- dissolution_coefs(dissolution = ~offset(edges) + offset(nodematch("age.grp", diff = TRUE)),
                                     duration = stats$casl$durs.casl.byage$mean.dur.adj,
                                     d.rate = exp.mort)
}


# 3. One-Off Partnership Stats -----------------------------------------------

out$inst <- list()

# 3A: edges
if (edges_avg_nfrace == FALSE) {
  out$inst$edges <- (stats$inst$md.inst * num) / 2
} else {
  out$inst$edges <- sum(unname(table(out$attr$race)) * stats$inst$nf.race)/2
}

# 3B: nodematch("age.grp", diff = FALSE)
out$inst$nodematch_age.grp <- sum(out$inst$edges * (unname(table(out$attr$age.grp))/num) * stats$inst$nm.age.grp)

# 3C: nodefactor("age.grp")
out$inst$nodefactor_age.grp <- unname(table(out$attr$age.grp)) * stats$inst$nf.age.grp

# 3D: nodematch("race", diff = FALSE)
out$inst$nodematch_race <- out$inst$edges * stats$inst$nm.race

# 3E: nodefactor("race")
out$inst$nodefactor_race <- unname(table(out$attr$race)) * stats$inst$nf.race

# 3F: nodefactor(c("risk.grp", "tot.deg3"))

# low risk // deg 0, 1, 2, 3; high risk // deg 0, 1, 2, 3
table(attr_deg.tot, attr_risk.grp)
as.numeric(table(attr_deg.tot, attr_risk.grp))

stats$inst$nf.risk.deg
as.numeric(stats$inst$nf.risk.deg)

out$inst$nodefactor_deg.tot.risk <- as.numeric(table(attr_deg.tot, attr_risk.grp)) *
  as.numeric(stats$inst$nf.risk.deg)



# Save Out File -----------------------------------------------------------

fns <- strsplit(fn, "[.]")[[1]]
fn.new <- paste(fns[1], "NetStats", fns[3], "rda", sep = ".")

saveRDS(out, file = fn.new)
