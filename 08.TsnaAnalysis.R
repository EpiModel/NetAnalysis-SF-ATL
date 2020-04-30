
## 
## tsna anlaysis for San Francisco & Atlanta sexual networks
## 

## Packages ##
library("scales")


## Load Data ##
fn <- "data/artnet.TsnaData.rda"
test <- readRDS(file = fn)


df <- as.data.frame(test[['sfa.frp']])
df1 <- as.data.frame(t(df))
