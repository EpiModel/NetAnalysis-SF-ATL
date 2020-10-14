
##
## Network parameters analysis for ARTnet Data
##

## Packages ##
library("tidyverse")
library("ARTnetData")

## Data ##
d <- ARTnet.wide
l <- ARTnet.long

## Inputs ##
city_name <- "Atlanta"
coef_name <- paste0("city2", city_name)


# 0. Data Processing ------------------------------------------------------

## Degree calculations ##

l$ONGOING <- as.numeric(l$ONGOING)
l$ongoing2 <- ifelse(is.na(l$ONGOING), 0, l$ONGOING)
l$ONGOING <- NULL

d <- l %>%
  filter(RAI == 1 | IAI == 1) %>%
  filter(ptype == 1) %>%
  group_by(AMIS_ID) %>%
  summarise(deg.main = sum(ongoing2)) %>%
  right_join(d, by = "AMIS_ID")

d <- l %>%
  filter(RAI == 1 | IAI == 1) %>%
  filter(ptype == 2) %>%
  group_by(AMIS_ID) %>%
  summarise(deg.casl = sum(ongoing2)) %>%
  right_join(d, by = "AMIS_ID")

# If missing degree, then set to 0
d$deg.main <- ifelse(is.na(d$deg.main), 0, d$deg.main)
d$deg.casl <- ifelse(is.na(d$deg.casl), 0, d$deg.casl)

summary(d$deg.main)
summary(d$deg.casl)

# Recoding to truncate degree
d$deg.casl <- ifelse(d$deg.casl > 3, 3, d$deg.casl)
d$deg.main <- ifelse(d$deg.main > 2, 2, d$deg.main)

d$deg.tot <- d$deg.main + d$deg.casl

md <- group_by(d, city2) %>%
  summarise(dm = mean(deg.main), dc = mean(deg.casl), dt = mean(deg.tot))
print(md, n = nrow(md))
table(d$deg.main, d$deg.casl)
round(prop.table(table(d$deg.main, d$deg.casl)), 3)

# Concurrency
d$deg.main.conc <- ifelse(d$deg.main > 1, 1, 0)
d$deg.casl.conc <- ifelse(d$deg.casl > 1, 1, 0)


## One-off calcs ##

# Total MC anal sex partner count
d <- l %>%
  filter(RAI == 1 | IAI == 1) %>%
  filter(ptype %in% 1:2) %>%
  group_by(AMIS_ID) %>%
  count() %>%
  rename(count.mc.part = n) %>%
  right_join(d, by = "AMIS_ID")
d$count.mc.part <- ifelse(is.na(d$count.mc.part), 0, d$count.mc.part)

d$count.oo.part <- d$ai.part - d$count.mc.part
d$count.oo.part <- pmax(0, d$count.oo.part)

head(data.frame(d$ai.part, d$count.mc.part, d$count.oo.part), 25)
summary(d$count.oo.part)


## Race/Ethnicity ##

table(d$race.cat)

d$race.cat2 <- ifelse(d$race.cat %in% c("white", "other"), 1, 0)
table(d$race.cat2)

l$race.cat2 <- ifelse(l$race.cat %in% c("white", "other"), 1, 0)
l$p_race.cat2 <- ifelse(l$p_race.cat %in% c("white", "other"), 1, 0)
table(l$race.cat2, l$p_race.cat2, useNA = "always")

table(l$race.cat, useNA = "always")
table(l$p_race.cat, useNA = "always")
table(l$race.cat, l$p_race.cat, useNA = "always")


## HIV status ##

l$p_hiv2 <- ifelse(l$p_hiv == 1, 1, 0)


## Setup output list ##

out <- list()


# 1. Main model -----------------------------------------------------------

out$main <- list()
lmain <- l[l$ptype == 1, ]


## 1A: edges ##

mod <- glm(deg.main ~ city2 - 1,
           data = d, family = poisson())
summary(mod)

b <- coef(mod)
md.main <- exp(b[coef_name])

out$main$md.main <- as.numeric(md.main)


## 1B: nodematch("age.grp") ##

age.breaks <- c(0, 24, 34, 44, 54, 64, 100)
lmain$index.age.grp <- cut(lmain$age, age.breaks, labels = FALSE)
lmain$part.age.grp <- cut(as.numeric(lmain$p_age), age.breaks, labels = FALSE)
data.frame(lmain$age, lmain$index.age.grp, lmain$p_age, lmain$part.age.grp)

lmain$same.age.grp <- ifelse(lmain$index.age.grp == lmain$part.age.grp, 1, 0)

mod <- glm(same.age.grp ~ city2 + index.age.grp - 1,
           data = lmain, family = binomial())
summary(mod)

b <- coef(mod)
nm.age.grp <- plogis(b[coef_name] + b["index.age.grp"]*1:5)
out$main$nm.age.grp <- nm.age.grp


## 1C: nodefactor("age.grp") ##

d$age.grp <- cut(d$age, age.breaks, labels = FALSE)

mod <- glm(deg.main ~ city2 + age.grp + sqrt(age.grp) - 1,
           data = d, family = poisson())
summary(mod)

b <- coef(mod)
nf.age.grp <- exp(b[coef_name] + b["age.grp"]*1:5 + 
                  b["sqrt(age.grp)"]*sqrt(1:5))
out$main$nf.age.grp <- nf.age.grp


## 1D: nodematch("race") ##

table(lmain$race.cat2)
table(lmain$p_race.cat2)

lmain$same.race <- ifelse(lmain$race.cat2 == lmain$p_race.cat2, 1, 0)
mean(lmain$same.race, na.rm = TRUE)

mod <- glm(same.race ~ city2 - 1,
           data = lmain, family = binomial())
summary(mod)

b <- coef(mod)
nm.race <- plogis(b[coef_name])
out$main$nm.race <- as.numeric(nm.race)


## 1E: nodefactor("race") ##

mod <- glm(deg.main ~ city2 + race.cat2 - 1,
           data = d, family = poisson())
summary(mod)

b <- coef(mod)
nf.race <- exp(b[coef_name] + b["race.cat2"]*0:1)
out$main$nf.race <- nf.race


## 1F: nodefactor("deg.casl") ##

mod <- glm(deg.main ~ city2 + deg.casl - 1,
           data = d, family = poisson())
summary(mod)

b <- coef(mod)
md.main.pers <- exp(b[coef_name] + b["deg.casl"]*0:3)
out$main$md.main.pers <- as.numeric(md.main.pers)

deg.casl.dist <- prop.table(table(d$deg.casl[d$city2 == city_name]))
out$main$deg.casl.dist <- as.numeric(deg.casl.dist)


## 1G: concurrent ##

mod <- glm(deg.main.conc ~ city2 - 1,
           data = d, family = binomial())
summary(mod)

b <- coef(mod)
concurrent <- plogis(b[coef_name])
out$main$concurrent <- as.numeric(concurrent)


## Durations ##

# overall
durs.main <- lmain %>%
  filter(RAI == 1 | IAI == 1) %>%
  filter(index.age.grp < 6) %>%
  filter(ongoing2 == 1) %>%
  summarise(mean.dur = mean(duration, na.rm = TRUE),
            median.dur = median(duration, na.rm = TRUE)) %>%
  as.data.frame()

# create city weights
durs.main.geo <- lmain %>%
  filter(RAI == 1 | IAI == 1) %>%
  filter(index.age.grp < 6) %>%
  filter(ongoing2 == 1) %>%
  filter(city2 == city_name) %>%
  summarise(mean.dur = mean(duration, na.rm = TRUE),
            median.dur = median(duration, na.rm = TRUE)) %>%
  as.data.frame()

# city-specific weight based on ratio of medians
wt <- durs.main.geo$median.dur/durs.main$median.dur

# The weekly dissolution rate is function of the mean of the geometric 
# distribution which relates to the median as:
durs.main$rates.main.adj <- 1 - (2^(-1/(wt*durs.main$median.dur)))

# Mean duration associated with a geometric distribution that median:
durs.main$mean.dur.adj <- 1/(1 - (2^(-1/(wt*durs.main$median.dur))))
out$main$durs.main.homog <- durs.main

# stratified by age

# first, non-matched by age group
durs.main.nonmatch <- lmain %>%
  filter(RAI == 1 | IAI == 1) %>%
  filter(index.age.grp < 6) %>%
  filter(ongoing2 == 1) %>%
  filter(same.age.grp == 0) %>%
  # group_by(index.age.grp) %>%
  summarise(mean.dur = mean(duration, na.rm = TRUE),
            median.dur = median(duration, na.rm = TRUE)) %>%
  as.data.frame()
durs.main.nonmatch$index.age.grp <- 0

# then, matched within age-groups
durs.main.matched <- lmain %>%
  filter(RAI == 1 | IAI == 1) %>%
  filter(index.age.grp < 6) %>%
  filter(ongoing2 == 1) %>%
  filter(same.age.grp == 1) %>%
  group_by(index.age.grp) %>%
  summarise(mean.dur = mean(duration, na.rm = TRUE),
            median.dur = median(duration, na.rm = TRUE)) %>%
  as.data.frame()
durs.main.matched

durs.main.all <- rbind(durs.main.nonmatch, durs.main.matched)

durs.main.all$rates.main.adj <- 1 - (2^(-1/(wt*durs.main.all$median.dur)))
durs.main.all$mean.dur.adj <- 1/(1 - (2^(-1/(wt*durs.main.all$median.dur))))

durs.main.all <- durs.main.all[, c(3, 1, 2, 4, 5)]
out$main$durs.main.byage <- durs.main.all


# 2. Casual model ---------------------------------------------------------

out$casl <- list()
lcasl <- l[l$ptype == 2, ]


## 2A: edges ##

mod <- glm(deg.casl ~ city2 - 1,
           data = d, family = poisson())
summary(mod)

b <- coef(mod)
md.casl <- exp(b[coef_name])

out$casl$md.casl <- as.numeric(md.casl)


## 2B: nodematch("age.grp") ##

age.breaks <- c(0, 24, 34, 44, 54, 64, 100)
lcasl$index.age.grp <- cut(lcasl$age, age.breaks, labels = FALSE)
lcasl$part.age.grp <- cut(as.numeric(lcasl$p_age), age.breaks, labels = FALSE)
data.frame(lcasl$age, lcasl$index.age.grp, lcasl$p_age, lcasl$part.age.grp)

lcasl$same.age.grp <- ifelse(lcasl$index.age.grp == lcasl$part.age.grp, 1, 0)

mod <- glm(same.age.grp ~ city2 + index.age.grp - 1,
           data = lcasl, family = binomial())
summary(mod)

b <- coef(mod)
nm.age.grp <- plogis(b[coef_name] + b["index.age.grp"]*1:5)
out$casl$nm.age.grp <- nm.age.grp


## 2C: nodefactor("age.grp") ##

d$age.grp <- cut(d$age, age.breaks, labels = FALSE)

mod <- glm(deg.casl ~ city2 + age.grp + sqrt(age.grp) - 1,
           data = d, family = poisson())
summary(mod)

b <- coef(mod)
nf.age.grp <- exp(b[coef_name] + b["age.grp"]*1:5 + 
                  b["sqrt(age.grp)"]*sqrt(1:5))
out$casl$nf.age.grp <- nf.age.grp


## 2D: nodematch("race") ##

table(lcasl$race.cat2)
table(lcasl$p_race.cat2)
prop.table(table(lcasl$race.cat2, lcasl$p_race.cat2), 1)

lcasl$same.race <- ifelse(lcasl$race.cat2 == lcasl$p_race.cat2, 1, 0)
mean(lcasl$same.race, na.rm = TRUE)

mod <- glm(same.race ~ city2 - 1,
           data = lcasl, family = binomial())
summary(mod)

b <- coef(mod)
nm.race <- plogis(b[coef_name])
out$casl$nm.race <- as.numeric(nm.race)


## 2E: nodefactor("race") ##

mod <- glm(deg.casl ~ city2 + race.cat2 - 1,
           data = d, family = poisson())
summary(mod)

b <- coef(mod)
nf.race <- exp(b[coef_name] + b["race.cat2"]*0:1)
out$casl$nf.race <- nf.race


## 2F: nodefactor("deg.main") ##

mod <- glm(deg.casl ~ city2 + deg.main - 1,
           data = d, family = poisson())
summary(mod)

b <- coef(mod)
md.pers.main <- exp(b[coef_name] + b["deg.main"]*0:2)
out$casl$md.pers.main <- as.numeric(md.pers.main)

deg.main.dist <- prop.table(table(d$deg.main[d$city2 == city_name]))
out$casl$deg.main.dist <- as.numeric(deg.main.dist)


## 2G: concurrent ##

mod <- glm(deg.casl.conc ~ city2 - 1,
           data = d, family = binomial())
summary(mod)

b <- coef(mod)
concurrent <- plogis(b[coef_name])
out$casl$concurrent <- as.numeric(concurrent)


## Durations ##

# overall
durs.casl <- lcasl %>%
  filter(RAI == 1 | IAI == 1) %>%
  filter(index.age.grp < 6) %>%
  filter(ongoing2 == 1) %>%
  summarise(mean.dur = mean(duration, na.rm = TRUE),
            median.dur = median(duration, na.rm = TRUE)) %>%
  as.data.frame()

# create city weights
durs.casl.geo <- lcasl %>%
  filter(RAI == 1 | IAI == 1) %>%
  filter(index.age.grp < 6) %>%
  filter(ongoing2 == 1) %>%
  filter(city2 == city_name) %>%
  summarise(mean.dur = mean(duration, na.rm = TRUE),
            median.dur = median(duration, na.rm = TRUE)) %>%
  as.data.frame()

# city-specific weight based on ratio of medians
wt <- durs.casl.geo$median.dur/durs.casl$median.dur

# The weekly dissolution rate is function of the mean of the geometric 
# distribution which relates to the median as:
durs.casl$rates.casl.adj <- 1 - (2^(-1/(wt*durs.casl$median.dur)))

# Mean duration associated with a geometric distribution that median:
durs.casl$mean.dur.adj <- 1/(1 - (2^(-1/(wt*durs.casl$median.dur))))
out$casl$durs.casl.homog <- durs.casl

# stratified by age

# first, non-matched by age group
durs.casl.nonmatch <- lcasl %>%
  filter(RAI == 1 | IAI == 1) %>%
  filter(index.age.grp < 6) %>%
  filter(ongoing2 == 1) %>%
  filter(same.age.grp == 0) %>%
  # group_by(index.age.grp) %>%
  summarise(mean.dur = mean(duration, na.rm = TRUE),
            median.dur = median(duration, na.rm = TRUE)) %>%
  as.data.frame()
durs.casl.nonmatch$index.age.grp <- 0

# then, matched within age-groups
durs.casl.matched <- lcasl %>%
  filter(RAI == 1 | IAI == 1) %>%
  filter(index.age.grp < 6) %>%
  filter(same.age.grp == 1) %>%
  filter(ongoing2 == 1) %>%
  group_by(index.age.grp) %>%
  summarise(mean.dur = mean(duration, na.rm = TRUE),
            median.dur = median(duration, na.rm = TRUE)) %>%
  as.data.frame()
durs.casl.matched

durs.casl.all <- rbind(durs.casl.nonmatch, durs.casl.matched)
durs.casl.all

durs.casl.all$rates.casl.adj <- 1 - (2^(-1/(wt*durs.casl.all$median.dur)))
durs.casl.all$mean.dur.adj <- 1/(1 - (2^(-1/(wt*durs.casl.all$median.dur))))

durs.casl.all <- durs.casl.all[, c(3, 1, 2, 4, 5)]
out$casl$durs.casl.byage <- durs.casl.all


# 3. One-off model --------------------------------------------------------

out$inst <- list()
linst <- l[l$ptype == 3, ]


## 3A: edges ##

head(d$count.oo.part, 25)
summary(d$count.oo.part)

# weekly rate
d$rate.oo.part <- d$count.oo.part/52
summary(d$rate.oo.part)

mod <- glm(count.oo.part ~ city2 - 1,
           data = d, family = poisson())
summary(mod)

b <- coef(mod)
md.inst <- exp(b[coef_name])/52
out$inst$md.inst <- as.numeric(md.inst)


## 3B: nodematch("age.grp") ##

age.breaks <- c(0, 24, 34, 44, 54, 64, 100)
linst$index.age.grp <- cut(linst$age, age.breaks, labels = FALSE)
linst$part.age.grp <- cut(as.numeric(linst$p_age), age.breaks, labels = FALSE)
data.frame(linst$age, linst$index.age.grp, linst$p_age, linst$part.age.grp)

linst$same.age.grp <- ifelse(linst$index.age.grp == linst$part.age.grp, 1, 0)

mod <- glm(same.age.grp ~ city2 + index.age.grp - 1,
           data = linst, family = binomial())
summary(mod)

b <- coef(mod)
nm.age.grp <- plogis(b[coef_name] + b["index.age.grp"]*1:5)
out$inst$nm.age.grp <- nm.age.grp


## 3C: nodefactor("age.grp") ##

d$age.grp <- cut(d$age, age.breaks, labels = FALSE)

mod <- glm(count.oo.part ~ city2 + age.grp + sqrt(age.grp) - 1,
           data = d, family = poisson())
summary(mod)

b <- coef(mod)
nf.age.grp <- exp(b[coef_name] + b["age.grp"]*1:5 + 
                  b["sqrt(age.grp)"]*sqrt(1:5))/52
out$inst$nf.age.grp <- nf.age.grp


## 3D: nodematch("race") ##

linst$same.race <- ifelse(linst$race.cat2 == linst$p_race.cat2, 1, 0)
mean(lcasl$same.race, na.rm = TRUE)

mod <- glm(same.race ~ city2 - 1,
           data = linst, family = binomial())
summary(mod)

b <- coef(mod)
nm.race <- plogis( b[coef_name])
out$inst$nm.race <- as.numeric(nm.race)


## 3E: nodefactor("race") ##

mod <- glm(count.oo.part ~ city2 + race.cat2 - 1,
           data = d, family = poisson())
summary(mod)

b <- coef(mod)
nf.race <- exp(b[coef_name] + b["race.cat2"]*0:1)/52
out$inst$nf.race <- nf.race


## 3F: nodefactor("risk.grp", "tot.deg3") ##

summary(d$deg.tot)
table(d$deg.tot)
d$deg.tot3 <- ifelse(d$deg.tot >= 3, 3, d$deg.tot)
table(d$deg.tot3)

deg.tot.dist <- prop.table(table(d$deg.tot3[d$city2 == city_name]))
out$inst$deg.tot.dist <- as.numeric(deg.tot.dist)

# city-specific wts
wt <- mean(d$rate.oo.part[d$city2 == city_name], 
           na.rm = TRUE)/mean(d$rate.oo.part, na.rm = TRUE)
wt.rate <- d$rate.oo.part * wt

# heterogeneous across tot.deg3
oo.quants <- array(NA, dim = c(4, 2))
for (i in 0:3) {
  sr <- sort(wt.rate[d$deg.tot3 == i])
  qsize <- floor(length(sr) / 2)
  
  oo.quants[i + 1, 1] <- mean(sr[1:qsize])
  oo.quants[i + 1, 2] <- mean(sr[(qsize + 1):length(sr)])
}
row.names(oo.quants) <- paste0("tdeg", 0:3)
colnames(oo.quants) <- c("lowr", "highr")

# Weekly acquisition rate
oo.quants

# Yearly OO partner count
oo.quants * 52

# New OO partner every X years
1/(oo.quants * 52)

# New OO partner every weeks
1/oo.quants

# Save it
out$inst$nf.risk.deg <- oo.quants


# 4. Other Parameters -----------------------------------------------------

## Sexual Role ##

roletype  <- rep(NA, nrow(d))
recept <- which(d$PART1RAI == 1 | d$PART2RAI == 1 |
                  d$PART3RAI == 1 | d$PART4RAI == 1 |
                  d$PART5RAI == 1)
insert <- which(d$PART1IAI == 1 | d$PART2IAI == 1 |
                  d$PART3IAI == 1 | d$PART4IAI == 1 |
                  d$PART5IAI == 1)
vers <- intersect(recept, insert)
receptonly <- setdiff(recept, vers)
insertonly <- setdiff(insert, vers)
roletype[receptonly] <- "R"
roletype[insertonly] <- "I"
roletype[vers] <- "V"

table(roletype)
out$all$role.type <- prop.table(table(roletype))


# SAVE --------------------------------------------------------------------

fn <- paste("data/artnet.NetParam", gsub(" ", "", city_name), "rda", sep = ".")
saveRDS(out, file = fn)
