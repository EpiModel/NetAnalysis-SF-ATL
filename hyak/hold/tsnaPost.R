## tsna hyak post process

# 1. rename all files to have trailing zeros so they load in correct order
#    first batch only
fn <- list.files("data/", full.names = TRUE, pattern = "sfo.all")
fn
length(fn)

for (i in 1:length(fn)) {
  load(fn[i])
  fns <- strsplit(fn[i], "[.]")
  fns[[1]][4] <- stringr::str_pad(fns[[1]][4], 3, pad = "0")
  fnn <- paste(fns[[1]], collapse = ".")
  unlink(fn[i])
  save(df, file = fnn)
  rm(df)
}

# 2. Merge all data files in column order

fn <- list.files("data/done1/", full.names = TRUE, pattern = "sfo.all")
fn
length(fn)

load(fn[1])
dim(df)
tdf <- df
for (i in 2:length(fn)) {
  load(fn[i])
  cat("\n ", dim(df))
  tdf <- cbind(tdf, df)
}
dim(tdf)

fn.new <- paste(strsplit(fn[1], "[.]")[[1]][-4], collapse = ".")
fn.new
save(tdf, file = fn.new)
unlink(fn)


# 3. Split data frames and save as list of data frames

fn <- list.files("data/", full.names = TRUE, pattern = "sfo.all.1")
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
names(out) <- c("frp", "medtdist", "medgeod", "degree", "cumldegree", "bcent")
names(out) <- c("frp", "medtdist", "medgeod", "degree", "cumldegree")
str(out)

save(out, file = fn)
