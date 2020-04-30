
## 
## tsna anlaysis for San Francisco & Atlanta sexual networks
## 

## Packages ##
library("scales")

## Load Data ##
fn <- "data/artnet.TsnaData.rda"
frp.data <- readRDS(file = fn)


# FRP data frames ---------------------------------------------------------

## Create data frames for FRP data ##

# SF
sfa.frp <- as.data.frame(t(frp.data[['sfa.frp']]))
sfm.frp <- as.data.frame(t(frp.data[['sfm.frp']]))
sfc.frp <- as.data.frame(t(frp.data[['sfc.frp']]))
sfi.frp <- as.data.frame(t(frp.data[['sfi.frp']]))

# ATL
atla.frp <- as.data.frame(t(frp.data[['atla.frp']]))
atlm.frp <- as.data.frame(t(frp.data[['atlm.frp']]))
atlc.frp <- as.data.frame(t(frp.data[['atlc.frp']]))
atli.frp <- as.data.frame(t(frp.data[['atli.frp']]))

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

row1 <- cbind(table2(sfa.frp1), table2(sfm.frp1), table2(sfc.frp1), 
              table2(sfi.frp1))

row2 <- cbind(table2(sfa.frp1[sf.24]), table2(sfm.frp1[sf.24]),
              table2(sfc.frp1[sf.24]), table2(sfi.frp1[sf.24]))

row3 <- cbind(table2(sfa.frp1[sf.34]), table2(sfm.frp1[sf.34]),
              table2(sfc.frp1[sf.34]), table2(sfc.frp1[sf.34]))


table2 <- rbind(row1, row2, row3)
rownames(table2) <- c("Overall", "Age: 15-24", "Age: 25-34")
table2





colnames(test) <- c("All Mean", "All Median", "All IQR Lower", "All IQR Upper",
                    "Main Mean", "Main Median", "Main IQR Lower", 
                    "Main IQR Upper", "Cas Mean", "Cas Median", 
                    "Cas IQR Lower", "Cas IQR Upper", "OT Mean", "OT Median",
                    "OT IQR Lower", "OT IQR Upper")




