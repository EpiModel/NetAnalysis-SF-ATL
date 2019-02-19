## tsna hyak post process

# 1. Check no missing batches
fn <- list.files("data/", full.names = TRUE, pattern = "atl.all")
fn
length(fn)

batches <- as.numeric(sapply(1:400, function(x) strsplit(fn, "[.]")[[x]][4]))
setdiff(1:400, batches)

# 2. Merge all data files in column order

load(fn[1])
dim(df)
tdf <- df
for (i in 2:length(fn)) {
  load(fn[i])
  if (ncol(df) != 25) cat("\nerror in", fn[i])
  tdf <- cbind(tdf, df)
}
dim(tdf)

fn.new <- paste(strsplit(fn[1], "[.]")[[1]][-4], collapse = ".")
fn.new
save(tdf, file = fn.new)
unlink(fn)


# 3. Split data frames and save as list of data frames

fn <- list.files("data/", full.names = TRUE, pattern = "atl.all")
fn

load(fn[1])

dim(tdf)
str(tdf)
class(tdf)

vars <- 5
batchSize <- nrow(tdf)/vars
set <- vars
defineSet <- function(batchSize, set) ((batchSize*set) - (batchSize - 1)):(batchSize*set)
defineSet(batchSize, vars)

out <- list()
for (i in 1:vars) {
  rows <- defineSet(batchSize, i)
  out[[i]] <- tdf[rows, ]
}
names(out) <- c("frp", "medtdist", "medgeod", "degree", "cumldegree")
str(out)
out$frp[1:25, 1:10]
out$cumldegree[1:25, 1:10]

save(out, file = fn)
