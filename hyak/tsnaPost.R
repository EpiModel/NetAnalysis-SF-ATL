## tsna hyak post process

fn <- list.files("data/", full.names = TRUE, pattern = "atl")
fn
length(fn)

load(fn[1])
tdf <- df
for (i in 2:length(fn)) {
  load(fn[i])
  tdf <- cbind(tdf, df)
}
dim(tdf)

# matplot(tdf, type = "l", col = "black", lty = 1)
fn.new <- paste(strsplit(fn[1], "[.]")[[1]][-4], collapse = ".")
fn.new
save(tdf, file = fn.new)
unlink(fn)
rm(list = ls())