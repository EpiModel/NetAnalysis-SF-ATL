
## 
## tsna anlaysis for San Francisco & Atlanta sexual networks
## 

rm(list = ls())

## Packages ##
library("scales")

## Load Data ##
fn <- "data/artnet.TsnaData.rda"
frp.data <- readRDS(file = fn)


# FRP data frames ---------------------------------------------------------

## Create data frames for FRP data ##

# SF
sfa.frp <- as.data.frame(t(frp.data$frp[['sfa.frp']]))
sfm.frp <- as.data.frame(t(frp.data$frp[['sfm.frp']]))
sfc.frp <- as.data.frame(t(frp.data$frp[['sfc.frp']]))
sfi.frp <- as.data.frame(t(frp.data$frp[['sfi.frp']]))

# ATL
atla.frp <- as.data.frame(t(frp.data$frp[['atla.frp']]))
atlm.frp <- as.data.frame(t(frp.data$frp[['atlm.frp']]))
atlc.frp <- as.data.frame(t(frp.data$frp[['atlc.frp']]))
atli.frp <- as.data.frame(t(frp.data$frp[['atli.frp']]))

## FRPs at one-year ##

# SF
sfa.frp1 <- sfa.frp[ , 52]
sfm.frp1 <- sfm.frp[ , 52]
sfc.frp1 <- sfc.frp[ , 52]
sfi.frp1 <- sfi.frp[ , 52]

# ATL
atla.frp1 <- atla.frp[ , 52]
atlm.frp1 <- atlm.frp[ , 52]
atlc.frp1 <- atlc.frp[ , 52]
atli.frp1 <- atli.frp[ , 52]


# Race & age IDs ----------------------------------------------------------

## SF ##

# age
sf.24 <- frp.data$demog$sf.24
sf.34 <- frp.data$demog$sf.34
sf.44 <- frp.data$demog$sf.44
sf.54 <- frp.data$demog$sf.54
sf.64 <- frp.data$demog$sf.64

# race
sf.b <- frp.data$demog$sf.b
sf.w <- frp.data$demog$sf.w

## ATL ##

# age
atl.24 <- frp.data$demog$atl.24
atl.34 <- frp.data$demog$atl.34
atl.44 <- frp.data$demog$atl.44
atl.54 <- frp.data$demog$atl.54
atl.64 <- frp.data$demog$atl.64

# race
atl.b <- frp.data$demog$atl.b
atl.w <- frp.data$demog$atl.w


# Manuscript Table 2 ------------------------------------------------------

## Function to extract summary stats from FRP dfs ##

table2 <- function(df) {
  summary <- summary(df)
  mean <- summary[['Mean']]
  median <- summary[['Median']]
  IQRlower <- summary[['1st Qu.']]
  IQRupper <- summary[['3rd Qu.']]
  
  x <- cbind(mean, median, IQRlower, IQRupper)
  return(x)
}

## Create rows for table 2 ##

## SF

# Overall
row1 <- cbind(table2(sfa.frp1), table2(sfm.frp1), table2(sfc.frp1), 
              table2(sfi.frp1))

# Age
row2 <- cbind(table2(sfa.frp1[sf.24]), table2(sfm.frp1[sf.24]),
              table2(sfc.frp1[sf.24]), table2(sfi.frp1[sf.24]))

row3 <- cbind(table2(sfa.frp1[sf.34]), table2(sfm.frp1[sf.34]),
              table2(sfc.frp1[sf.34]), table2(sfc.frp1[sf.34]))

row4 <- cbind(table2(sfa.frp1[sf.44]), table2(sfm.frp1[sf.44]),
              table2(sfc.frp1[sf.44]), table2(sfc.frp1[sf.44]))

row5 <- cbind(table2(sfa.frp1[sf.54]), table2(sfm.frp1[sf.54]),
              table2(sfc.frp1[sf.54]), table2(sfc.frp1[sf.54]))

row6 <- cbind(table2(sfa.frp1[sf.64]), table2(sfm.frp1[sf.64]),
              table2(sfc.frp1[sf.64]), table2(sfc.frp1[sf.64]))

# Race
row7 <- cbind(table2(sfa.frp1[sf.b]), table2(sfm.frp1[sf.b]),
              table2(sfc.frp1[sf.b]), table2(sfc.frp1[sf.b]))

row8 <- cbind(table2(sfa.frp1[sf.w]), table2(sfm.frp1[sf.w]),
              table2(sfc.frp1[sf.w]), table2(sfc.frp1[sf.w]))

## ATL

# Overall
row9 <- cbind(table2(atla.frp1), table2(atlm.frp1), table2(atlc.frp1), 
              table2(atli.frp1))

# Age
row10 <- cbind(table2(atla.frp1[sf.24]), table2(atlm.frp1[sf.24]),
              table2(atlc.frp1[sf.24]), table2(atli.frp1[sf.24]))

row11 <- cbind(table2(atla.frp1[sf.34]), table2(atlm.frp1[sf.34]),
              table2(atlc.frp1[sf.34]), table2(atli.frp1[sf.34]))

row12 <- cbind(table2(atla.frp1[sf.44]), table2(atlm.frp1[sf.44]),
               table2(atlc.frp1[sf.44]), table2(atli.frp1[sf.44]))

row13 <- cbind(table2(atla.frp1[sf.54]), table2(atlm.frp1[sf.54]),
               table2(atlc.frp1[sf.54]), table2(atli.frp1[sf.54]))

row14 <- cbind(table2(atla.frp1[sf.64]), table2(atlm.frp1[sf.64]),
               table2(atlc.frp1[sf.64]), table2(atli.frp1[sf.64]))

# Race
row15 <- cbind(table2(atla.frp1[sf.b]), table2(atlm.frp1[sf.b]),
              table2(atlc.frp1[sf.b]), table2(atlc.frp1[sf.b]))

row16 <- cbind(table2(atla.frp1[sf.w]), table2(atlm.frp1[sf.w]),
               table2(atlc.frp1[sf.w]), table2(atlc.frp1[sf.w]))

comp.table2 <- rbind(row1, row2, row3, row4, row5, row6, row7, row8, row9, row10, 
                  row11, row12, row13, row14, row15, row16)


rownames(comp.table2) <- c("SF, Overall", "SF, Age: 15-24", "SF, Age: 25-34", 
                       "SF, Age: 35-44", "SF, Age: 45-54", "SF, Age: 55-64", 
                       "SF, Black", "SF, White", "ATL, Overall", 
                       "ATL, Age: 15-24", "ATL, Age: 25-34",  "ATL, Age: 35-44",
                       "ATL, Age: 45-54", "ATL, Age: 55-64", "ATL, Black",
                       "ATL, White")


colnames(comp.table2) <- c("All Mean", "All Median", "All IQR Lower", 
                           "All IQR Upper", "Main Mean", "Main Median", 
                           "Main IQR Lower", "Main IQR Upper", "Cas Mean", 
                           "Cas Median", "Cas IQR Lower", "Cas IQR Upper", 
                           "OT Mean", "OT Median","OT IQR Lower", 
                           "OT IQR Upper")

## Export table to csv ##
write.csv(comp.table2, "Table_2.csv")


# Manuscript Figure 2 -----------------------------------------------------

#title()

library("viridis")
library("wesanderson")
# palv <- viridis(n = 4, alpha = 0.25, option = "inferno")
# palv <- adjustcolor(wes_palette(5, name = "Zissou1"), alpha.f = 1)
# palv <- rainbow(10)
# pal <- adjustcolor(RColorBrewer::brewer.pal(5, "Set1"), alpha.f = 0.8)

## All partnerships
# jpeg("Plot2.jpeg", width = 8, height = 4, units = 'in', res = 250)
par(mfrow = c(2, 3), oma = c(0, 0, 0, 0), xpd = NA, mgp = c(2,1,0), 
    mar = c(3,3,2,1))
# SF
matplot(sfm.frp, type = "l", ylim = c(0, 30), xlab = "", ylab = "FRP", lty = 1,
        col = palv, lwd = 0.5, main = "SF Main")
matplot(sfc.frp, type = "l", ylim = c(0, 10000), xlab = "", ylab = "", lty = 1,
        col = palv, lwd = 0.5, main = "SF Casual")
matplot(sfi.frp, type = "l", ylim = c(0, 10000), xlab = "", ylab = "", lty = 1,
        col = palv, lwd = 0.5, main = "SF One-Time")

# ATL
matplot(atlm.frp, type = "l", ylim = c(0, 30), xlab = "Week", ylab = "FRP", lty = 1,
        col = palv, lwd = 0.5, main = "ATL Main")
matplot(atlc.frp, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "", lty = 1,
        col = palv, lwd = 0.5, main = "ATL Casual")
matplot(atli.frp, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "", lty = 1,
        col = palv, lwd = 0.5, main = "ATL One-Time")

dev.off()

# title("Distribution of 5-Year Foward Reachable Paths by Partnership Type", 
# outer = TRUE)

par(mfrow = c(2, 3), oma = c(0, 0, 0, 0), xpd = NA, mgp = c(2,1,0), 
    mar = c(3,3,2,1))
# SF
matplot(sfm.frp, type = "l", ylim = c(0, 30), xlab = "", ylab = "FRP",
        main = "SF Main")
matplot(sfc.frp, type = "l", ylim = c(0, 10000), xlab = "", ylab = "", 
        main = "SF Casual")
matplot(sfi.frp, type = "l", ylim = c(0, 10000), xlab = "", ylab = "", 
        main = "SF One-Time")

# ATL
matplot(atlm.frp, type = "l", ylim = c(0, 30), xlab = "Week", ylab = "FRP", 
        main = "ATL Main")
matplot(atlc.frp, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "", 
        main = "ATL Casual")
matplot(atli.frp, type = "l", ylim = c(0, 10000), xlab = "Week", ylab = "", 
        main = "ATL One-Time")

dev.off()

# Manuscript Figure 3 -----------------------------------------------------

## Mean proportional reachable among casual partnerships by age over 5-years ##

## SF
sfc.24.avg <- colMeans(as.data.frame(sfc.frp[sf.24, ]))/10000
sfc.34.avg <- colMeans(as.data.frame(sfc.frp[sf.34, ]))/10000
sfc.44.avg <- colMeans(as.data.frame(sfc.frp[sf.44, ]))/10000
sfc.54.avg <- colMeans(as.data.frame(sfc.frp[sf.54, ]))/10000
sfc.64.avg <- colMeans(as.data.frame(sfc.frp[sf.64, ]))/10000

## ATL
atlc.24.avg <- colMeans(as.data.frame(atlc.frp[atl.24, ]))/10000
atlc.34.avg <- colMeans(as.data.frame(atlc.frp[atl.34, ]))/10000
atlc.44.avg <- colMeans(as.data.frame(atlc.frp[atl.44, ]))/10000
atlc.54.avg <- colMeans(as.data.frame(atlc.frp[atl.54, ]))/10000
atlc.64.avg <- colMeans(as.data.frame(atlc.frp[atl.64, ]))/10000

## Line plots ##

## Set plot options
pal <- adjustcolor(RColorBrewer::brewer.pal(5, "Set1"), alpha.f = 0.8)
jpeg("Plot3.jpeg", width = 8, height = 4, units = 'in', res = 300)
par(mfrow = c(1,2), mgp = c(2,1,0), mar = c(3,3,2,1))

## SF
plot(x = 1:260, y = sfc.24.avg, type = "l", col = pal[1], lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable", main = "San Francisco")
lines(x = 1:260, y = sfc.34.avg, type = "l", col = pal[2], 
      lwd = 2)
lines(x = 1:260, y = sfc.44.avg, type = "l", col = pal[3], 
      lwd = 2)
lines(x = 1:260, y = sfc.54.avg, type = "l", col = pal[4], 
      lwd = 2)
lines(x = 1:260, y = sfc.64.avg, type = "l", col = pal[5], 
      lwd = 2)
legend("topleft", legend = c("15-24", "25-34", "35-44", "45-54", "55-64"), 
       col = pal, lty = 1, cex = 0.8)

## ATL
plot(x = 1:260, y = atlc.24.avg, type = "l", col = pal[1], lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable", main = "Atlanta")
lines(x = 1:260, y = atlc.34.avg, type = "l", col = pal[2], 
      lwd = 2)
lines(x = 1:260, y = atlc.44.avg, type = "l", col = pal[3], 
      lwd = 2)
lines(x = 1:260, y = atlc.54.avg, type = "l", col = pal[4], 
      lwd = 2)
lines(x = 1:260, y = atlc.64.avg, type = "l", col = pal[5], 
      lwd = 2)
legend("topleft", legend = c("15-24", "25-34", "35-44", "45-54", "55-64"), 
       col = pal, lty = 1, cex = 0.8)

dev.off()


# Additional analyses -----------------------------------------------------

## 95th interval of FRP at one-year ##

## Function to calculate 2.5th and 97.5th percentiles
quant.95int <- function(df) {
  df.L <- quantile(df , 0.025)
  df.U <- quantile(df , 0.975)
  interval <- cbind(df.L, df.U)
  rownames(interval) <- c('FRP')
  return(interval)
}


## SF all partnerships

# Overall
sfa.quant <- quant.95int(sfa.frp1)

# By age
sfa24.quant <- quant.95int(sfa.frp1[sf.24])
sfa34.quant <- quant.95int(sfa.frp1[sf.34])
sfa44.quant <- quant.95int(sfa.frp1[sf.44])
sfa54.quant <- quant.95int(sfa.frp1[sf.54])
sfa64.quant <- quant.95int(sfa.frp1[sf.64])

# By race
sfab.quant <- quant.95int(sfa.frp1[sf.b])
sfaw.quant <- quant.95int(sfa.frp1[sf.w])


## SF main partnerships

# Overall
sfm.quant <- quant.95int(sfm.frp1)

# By age
sfm24.quant <- quant.95int(sfm.frp1[sf.24])
sfm34.quant <- quant.95int(sfm.frp1[sf.34])
sfm44.quant <- quant.95int(sfm.frp1[sf.44])
sfm54.quant <- quant.95int(sfm.frp1[sf.54])
sfm64.quant <- quant.95int(sfm.frp1[sf.64])

# By race
sfmb.quant <- quant.95int(sfm.frp1[sf.b])
sfmw.quant <- quant.95int(sfm.frp1[sf.w])


## SF casual partnerships

# Overall
sfc.quant <- quant.95int(sfa.frp1)

# By age
sfc24.quant <- quant.95int(sfc.frp1[sf.24])
sfc34.quant <- quant.95int(sfc.frp1[sf.34])
sfc44.quant <- quant.95int(sfc.frp1[sf.44])
sfc54.quant <- quant.95int(sfc.frp1[sf.54])
sfc64.quant <- quant.95int(sfc.frp1[sf.64])

# By race
sfcb.quant <- quant.95int(sfc.frp1[sf.b])
sfcw.quant <- quant.95int(sfc.frp1[sf.w])


## SF one-time partnerships

# Overall
sfi.quant <- quant.95int(sfa.frp1)

# By age
sfi24.quant <- quant.95int(sfi.frp1[sf.24])
sfi34.quant <- quant.95int(sfi.frp1[sf.34])
sfi44.quant <- quant.95int(sfi.frp1[sf.44])
sfi54.quant <- quant.95int(sfi.frp1[sf.54])
sfi64.quant <- quant.95int(sfi.frp1[sf.64])

# By race
sfib.quant <- quant.95int(sfi.frp1[sf.b])
sfiw.quant <- quant.95int(sfi.frp1[sf.w])


## ATL all partnerships

# Overall
atla.quant <- quant.95int(atla.frp1)

# By age
atla24.quant <- quant.95int(atla.frp1[sf.24])
atla34.quant <- quant.95int(atla.frp1[sf.34])
atla44.quant <- quant.95int(atla.frp1[sf.44])
atla54.quant <- quant.95int(atla.frp1[sf.54])
atla64.quant <- quant.95int(atla.frp1[sf.64])

# By race
atlab.quant <- quant.95int(atla.frp1[sf.b])
atlaw.quant <- quant.95int(atla.frp1[sf.w])


## ATL main partnerships

# Overall
atlm.quant <- quant.95int(atlm.frp1)

# By age
atlm24.quant <- quant.95int(atlm.frp1[sf.24])
atlm34.quant <- quant.95int(atlm.frp1[sf.34])
atlm44.quant <- quant.95int(atlm.frp1[sf.44])
atlm54.quant <- quant.95int(atlm.frp1[sf.54])
atlm64.quant <- quant.95int(atlm.frp1[sf.64])

# By race
atlmb.quant <- quant.95int(atlm.frp1[sf.b])
atlmw.quant <- quant.95int(atlm.frp1[sf.w])


## ATL casual partnerships

# Overall
atlc.quant <- quant.95int(atlc.frp1)

# By age
atlc24.quant <- quant.95int(atlc.frp1[sf.24])
atlc34.quant <- quant.95int(atlc.frp1[sf.34])
atlc44.quant <- quant.95int(atlc.frp1[sf.44])
atlc54.quant <- quant.95int(atlc.frp1[sf.54])
atlc64.quant <- quant.95int(atlc.frp1[sf.64])

# By race
atlcb.quant <- quant.95int(atlc.frp1[sf.b])
atlcw.quant <- quant.95int(atlc.frp1[sf.w])


## ATL one-time partnerships

# Overall
atli.quant <- quant.95int(atli.frp1)

# By age
atli24.quant <- quant.95int(atli.frp1[sf.24])
atli34.quant <- quant.95int(atli.frp1[sf.34])
atli44.quant <- quant.95int(atli.frp1[sf.44])
atli54.quant <- quant.95int(atli.frp1[sf.54])
atli64.quant <- quant.95int(atli.frp1[sf.64])

# By race
atlib.quant <- quant.95int(atli.frp1[sf.b])
atliw.quant <- quant.95int(atli.frp1[sf.w])


# Additional figures ------------------------------------------------------

## All partnerships - Mean FRP over 5-years ##

## Overall - PLOT 1 (all ptypes)

# Mean proportional reachable
sfa.frp.avg <- colMeans(as.data.frame(sfa.frp))/10000
atla.frp.avg <- colMeans(as.data.frame(atla.frp))/10000

# Set plot options
par(mfrow = c(1,1))

# Plot
plot(x = 1:260, y = sfa.frp.avg, type = "l", col = alpha("red", 0.7), lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable")
lines(x = 1:260, y = atla.frp.avg, type = "l", col = alpha("blue", 0.7), 
      lwd = 2)
legend("bottomright", legend = c("San Francisco", "Atlanta"), 
       col = c("red", "blue"), lty = 1, cex(1.5))


## By age - PLOT 2 (all ptypes)

# SF mean proportional reachable
sfa.24.avg <- colMeans(as.data.frame(sfa.frp[sf.24, ]))/10000
sfa.34.avg <- colMeans(as.data.frame(sfa.frp[sf.34, ]))/10000
sfa.44.avg <- colMeans(as.data.frame(sfa.frp[sf.44, ]))/10000
sfa.54.avg <- colMeans(as.data.frame(sfa.frp[sf.54, ]))/10000
sfa.64.avg <- colMeans(as.data.frame(sfa.frp[sf.64, ]))/10000

# ATL mean proportional reachable
atla.24.avg <- colMeans(as.data.frame(atla.frp[atl.24, ]))/10000
atla.34.avg <- colMeans(as.data.frame(atla.frp[atl.34, ]))/10000
atla.44.avg <- colMeans(as.data.frame(atla.frp[atl.44, ]))/10000
atla.54.avg <- colMeans(as.data.frame(atla.frp[atl.54, ]))/10000
atla.64.avg <- colMeans(as.data.frame(atla.frp[atl.64, ]))/10000

# Set plot options
pal <- adjustcolor(RColorBrewer::brewer.pal(5, "Set1"), alpha.f = 0.8)
par(mfrow = c(1,2))

# Plots
plot(x = 1:260, y = sfa.24.avg, type = "l", col = pal[1], lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable ", main = "San Francisco")
lines(x = 1:260, y = sfa.34.avg, type = "l", col = pal[2], 
      lwd = 2)
lines(x = 1:260, y = sfa.44.avg, type = "l", col = pal[3], 
      lwd = 2)
lines(x = 1:260, y = sfa.54.avg, type = "l", col = pal[4], 
      lwd = 2)
lines(x = 1:260, y = sfa.64.avg, type = "l", col = pal[5], 
      lwd = 2)
legend("bottomright", legend = c("15-24", "25-34", "35-44", "45-54", "55-64"), 
       col = pal, lty = 1)

plot(x = 1:260, y = atla.24.avg, type = "l", col = pal[1], lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable", main = "Atlanta")
lines(x = 1:260, y = atla.34.avg, type = "l", col = pal[2], 
      lwd = 2)
lines(x = 1:260, y = atla.44.avg, type = "l", col = pal[3], 
      lwd = 2)
lines(x = 1:260, y = atla.54.avg, type = "l", col = pal[4], 
      lwd = 2)
lines(x = 1:260, y = atla.64.avg, type = "l", col = pal[5], 
      lwd = 2)
legend("bottomright", legend = c("15-24", "25-34", "35-44", "45-54", "55-64"), 
       col = pal, lty = 1)


## By race - PLOT 3 (all ptypes)

# SF mean proportional reachable
sfa.b.avg <- colMeans(as.data.frame(sfa.frp[sf.b, ]))/10000
sfa.w.avg <- colMeans(as.data.frame(sfa.frp[sf.w, ]))/10000

# ATL mean proportional reachable
atla.b.avg <- colMeans(as.data.frame(atla.frp[atl.b, ]))/10000
atla.w.avg <- colMeans(as.data.frame(atla.frp[atl.w, ]))/10000

# Set plot options
par(mfrow = c(1,1))

# Plot
plot(x = 1:260, y = sfa.b.avg, type = "l", col = alpha("red", 0.5), lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable")
lines(x = 1:260, y = sfa.w.avg, type = "l", col = alpha("blue", 0.5), 
      lwd = 2)
lines(x = 1:260, y = atla.b.avg, type = "l", col = alpha("yellow", 0.5), 
      lwd = 2)
lines(x = 1:260, y = atla.w.avg, type = "l", col = alpha("green", 0.5), 
      lwd = 2)
legend("bottomright", 
       legend = c("Black - SF", "White - SF", "Black - ATL", "White - ATL"), 
       col = c("red", "blue", "yellow", "green"), lty = 1)


## Main partnerships - Mean FRP over 5-years ##

## Overall - PLOT 4 (main ptypes)

# Mean proportional reachable
sfm.frp.avg <- colMeans(as.data.frame(sfm.frp))/10000
atlm.frp.avg <- colMeans(as.data.frame(atlm.frp))/10000

# Set plot options
par(mfrow = c(1,1))

# Plots
plot(x = 1:260, y = sfm.frp.avg, type = "l", col = alpha("red", 0.7), lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable")
lines(x = 1:260, y = atlm.frp.avg, type = "l", col = alpha("blue", 0.7), 
      lwd = 2)
legend("bottomright", legend = c("San Francisco", "Atlanta"), 
       col = c("red", "blue"), lty = 1)


## By age - PLOT 5 (main ptypes)

# SF mean proportional reachable
sfm.24.avg <- colMeans(as.data.frame(sfm.frp[sf.24, ]))/10000
sfm.34.avg <- colMeans(as.data.frame(sfm.frp[sf.34, ]))/10000
sfm.44.avg <- colMeans(as.data.frame(sfm.frp[sf.44, ]))/10000
sfm.54.avg <- colMeans(as.data.frame(sfm.frp[sf.54, ]))/10000
sfm.64.avg <- colMeans(as.data.frame(sfm.frp[sf.64, ]))/10000

# ATL mean proportional reachable
atlm.24.avg <- colMeans(as.data.frame(atlm.frp[atl.24, ]))/10000
atlm.34.avg <- colMeans(as.data.frame(atlm.frp[atl.34, ]))/10000
atlm.44.avg <- colMeans(as.data.frame(atlm.frp[atl.44, ]))/10000
atlm.54.avg <- colMeans(as.data.frame(atlm.frp[atl.54, ]))/10000
atlm.64.avg <- colMeans(as.data.frame(atlm.frp[atl.64, ]))/10000

# Set plot options
pal <- adjustcolor(RColorBrewer::brewer.pal(5, "Set1"), alpha.f = 0.8)
par(mfrow = c(1,2))

# Plots
plot(x = 1:260, y = sfm.24.avg, type = "l", col = pal[1], lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable ", main = "San Francisco",
     ylim = c(0.0001, 0.0005))
lines(x = 1:260, y = sfm.34.avg, type = "l", col = pal[2], 
      lwd = 2)
lines(x = 1:260, y = sfm.44.avg, type = "l", col = pal[3], 
      lwd = 2)
lines(x = 1:260, y = sfm.54.avg, type = "l", col = pal[4], 
      lwd = 2)
lines(x = 1:260, y = sfm.64.avg, type = "l", col = pal[5], 
      lwd = 2)
legend("topleft", legend = c("15-24", "25-34", "35-44", "45-54", "55-64"), 
       col = pal, lty = 1)

plot(x = 1:260, y = atlm.24.avg, type = "l", col = pal[1], lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable", main = "Atlanta",
     ylim = c(0.0001, 0.0005))
lines(x = 1:260, y = atlm.34.avg, type = "l", col = pal[2], 
      lwd = 2)
lines(x = 1:260, y = atlm.44.avg, type = "l", col = pal[3], 
      lwd = 2)
lines(x = 1:260, y = atlm.54.avg, type = "l", col = pal[4], 
      lwd = 2)
lines(x = 1:260, y = atlm.64.avg, type = "l", col = pal[5], 
      lwd = 2)
legend("topleft", legend = c("15-24", "25-34", "35-44", "45-54", "55-64"), 
       col = pal, lty = 1)


## By race - PLOT 6 (main ptypes)

# SF mean proportional reachable
sfm.b.avg <- colMeans(as.data.frame(sfm.frp[sf.b, ]))/10000
sfm.w.avg <- colMeans(as.data.frame(sfm.frp[sf.w, ]))/10000

# ATL mean proportional reachable
atlm.b.avg <- colMeans(as.data.frame(atlm.frp[atl.b, ]))/10000
atlm.w.avg <- colMeans(as.data.frame(atlm.frp[atl.w, ]))/10000

# Set plot options
par(mfrow = c(1,1))

# Plot
plot(x = 1:260, y = sfm.b.avg, type = "l", col = alpha("red", 0.5), lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable")
lines(x = 1:260, y = sfm.w.avg, type = "l", col = alpha("blue", 0.5), 
      lwd = 2)
lines(x = 1:260, y = atlm.b.avg, type = "l", col = alpha("yellow", 0.5), 
      lwd = 2)
lines(x = 1:260, y = atlm.w.avg, type = "l", col = alpha("green", 0.5), 
      lwd = 2)
legend("bottomright", 
       legend = c("Black - SF", "White - SF", "Black - ATL", "White - ATL"), 
       col = c("red", "blue", "yellow", "green"), lty = 1)


## Casual partnerships - Mean FRP over 5-years ##

## Overall - PLOT 7 (casual ptypes)

# Mean proportional reachable
sfc.frp.avg <- colMeans(as.data.frame(sfc.frp))/10000
atlc.frp.avg <- colMeans(as.data.frame(atlc.frp))/10000

# Set plot options
par(mfrow = c(1,1))

# Plots
plot(x = 1:260, y = sfc.frp.avg, type = "l", col = alpha("red", 0.7), lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable")
lines(x = 1:260, y = atlc.frp.avg, type = "l", col = alpha("blue", 0.7), 
      lwd = 2)
legend("topleft", legend = c("San Francisco", "Atlanta"), 
       col = c("red", "blue"), lty = 1)


## By age - see lines 179-232 for manuscript table 3 code


## By race - PLOT 8 (casual ptypes)

# SF mean proportional reachable
sfc.b.avg <- colMeans(as.data.frame(sfc.frp[sf.b, ]))/10000
sfc.w.avg <- colMeans(as.data.frame(sfc.frp[sf.w, ]))/10000

# ATL mean proportional reachable
atlc.b.avg <- colMeans(as.data.frame(atlc.frp[atl.b, ]))/10000
atlc.w.avg <- colMeans(as.data.frame(atlc.frp[atl.w, ]))/10000

# Set plot options
par(mfrow = c(1,1))

# Plot
plot(x = 1:260, y = sfc.b.avg, type = "l", col = alpha("red", 0.5), lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable")
lines(x = 1:260, y = sfc.w.avg, type = "l", col = alpha("blue", 0.5), 
      lwd = 2)
lines(x = 1:260, y = atlc.b.avg, type = "l", col = alpha("yellow", 0.5), 
      lwd = 2)
lines(x = 1:260, y = atlc.w.avg, type = "l", col = alpha("green", 0.5), 
      lwd = 2)
legend("topleft", 
       legend = c("Black - SF", "White - SF", "Black - ATL", "White - ATL"), 
       col = c("red", "blue", "yellow", "green"), lty = 1)


## One-time partnerships - Mean FRP over 5-years ##

## Overall - PLOT 9 (inst. ptypes)

# Mean proportional reachable
sfi.frp.avg <- colMeans(as.data.frame(sfi.frp))/10000
atli.frp.avg <- colMeans(as.data.frame(atli.frp))/10000

# Set plot options
par(mfrow = c(1,1))

# Plots
plot(x = 1:260, y = sfi.frp.avg, type = "l", col = alpha("red", 0.7), lwd = 2, 
     xlab = "Week", ylab = "Proportion Reachable")
lines(x = 1:260, y = atli.frp.avg, type = "l", col = alpha("blue", 0.7), 
      lwd = 2)
legend("topleft", legend = c("San Francisco", "Atlanta"), 
       col = c("red", "blue"), lty = 1)


## By age - PLOT 10 (inst. ptypes)

# SF mean proportional reachable
sfi.24.avg <- colMeans(as.data.frame(sfi.frp[sf.24, ]))/10000
sfi.34.avg <- colMeans(as.data.frame(sfi.frp[sf.34, ]))/10000
sfi.44.avg <- colMeans(as.data.frame(sfi.frp[sf.44, ]))/10000
sfi.54.avg <- colMeans(as.data.frame(sfi.frp[sf.54, ]))/10000
sfi.64.avg <- colMeans(as.data.frame(sfi.frp[sf.64, ]))/10000
  
# ATL mean proportional reachable
atli.24.avg <- colMeans(as.data.frame(atli.frp[atl.24, ]))/10000
atli.34.avg <- colMeans(as.data.frame(atli.frp[atl.34, ]))/10000
atli.44.avg <- colMeans(as.data.frame(atli.frp[atl.44, ]))/10000
atli.54.avg <- colMeans(as.data.frame(atli.frp[atl.54, ]))/10000
atli.64.avg <- colMeans(as.data.frame(atli.frp[atl.64, ]))/10000
  
# Set plot options
pal <- adjustcolor(RColorBrewer::brewer.pal(5, "Set1"), alpha.f = 0.8)
par(mfrow = c(1,2))
  
# Plots
plot(x = 1:260, y = sfi.24.avg, type = "l", col = pal[1], lwd = 2, 
      xlab = "Week", ylab = "Proportion Reachable", main = "San Francisco",
      ylim = c(0, 0.35))
lines(x = 1:260, y = sfi.34.avg, type = "l", col = pal[2], 
      lwd = 2)
lines(x = 1:260, y = sfi.44.avg, type = "l", col = pal[3], 
      lwd = 2)
lines(x = 1:260, y = sfi.54.avg, type = "l", col = pal[4], 
      lwd = 2)
lines(x = 1:260, y = sfi.64.avg, type = "l", col = pal[5], 
      lwd = 2)
legend("bottomright", legend = c("15-24", "25-34", "35-44", "45-54", "55-64"), 
         col = pal, lty = 1, cex = 0.7)
  
plot(x = 1:260, y = atli.24.avg, type = "l", col = pal[1], lwd = 2, 
      xlab = "Week", ylab = "Proportion Reachable", main = "Atlanta", 
      ylim = c(0, 0.35))
lines(x = 1:260, y = atli.34.avg, type = "l", col = pal[2], 
      lwd = 2)
lines(x = 1:260, y = atli.44.avg, type = "l", col = pal[3], 
      lwd = 2)
lines(x = 1:260, y = atli.54.avg, type = "l", col = pal[4], 
      lwd = 2)
lines(x = 1:260, y = atli.64.avg, type = "l", col = pal[5], 
      lwd = 2)
legend("bottomright", legend = c("15-24", "25-34", "35-44", "45-54", "55-64"), 
        col = pal, lty = 1, cex = 0.7)

  
## By race - PLOT 11 (inst. ptypes)
  
# SF mean proportional reachable
sfi.b.avg <- colMeans(as.data.frame(sfi.frp[sf.b, ]))/10000
sfi.w.avg <- colMeans(as.data.frame(sfi.frp[sf.w, ]))/10000
  
# ATL mean proportional reachable
atli.b.avg <- colMeans(as.data.frame(atli.frp[atl.b, ]))/10000
atli.w.avg <- colMeans(as.data.frame(atli.frp[atl.w, ]))/10000
  
# Set plot options
par(mfrow = c(1,1))

# Plots
plot(x = 1:260, y = sfi.b.avg, type = "l", col = alpha("red", 0.5), lwd = 2, 
      xlab = "Week", ylab = "Proportion Reachable")
lines(x = 1:260, y = sfi.w.avg, type = "l", col = alpha("blue", 0.5), 
      lwd = 2)
lines(x = 1:260, y = atli.b.avg, type = "l", col = alpha("yellow", 0.5), 
      lwd = 2)
lines(x = 1:260, y = atli.w.avg, type = "l", col = alpha("green", 0.5), 
      lwd = 2)
legend("bottomright", 
       legend = c("Black - SF", "White - SF", "Black - ATL", "White - ATL"), 
       col = c("red", "blue", "yellow", "green"), lty = 1, cex = 0.8)

# END ---------------------------------------------------------------------



