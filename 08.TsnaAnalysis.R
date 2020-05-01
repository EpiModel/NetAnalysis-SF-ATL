
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


colnames(comp.table2) <- c("All Mean", "All Median", "All IQR Lower", "All IQR Upper",
                    "Main Mean", "Main Median", "Main IQR Lower", 
                    "Main IQR Upper", "Cas Mean", "Cas Median", 
                    "Cas IQR Lower", "Cas IQR Upper", "OT Mean", "OT Median",
                    "OT IQR Lower", "OT IQR Upper")

## Export table to csv ##
write.csv(comp.table2, "Table_2.csv")


# Manuscript Figure 2 -----------------------------------------------------







