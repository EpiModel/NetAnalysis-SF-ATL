## tsna hyak post process

# 1. rename all files to have trailing zeros so they load in correct order
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

fn <- list.files("data/", full.names = TRUE, pattern = "sfo.all")
fn
length(fn)

load(fn[1])
tdf <- df
for (i in 2:length(fn)) {
  load(fn[i])
  tdf <- cbind(tdf, df)
}
dim(tdf)

fn.new <- paste(strsplit(fn[1], "[.]")[[1]][-4], collapse = ".")
fn.new
save(tdf, file = fn.new)
unlink(fn)


# 3. Split data frames and save as list of data frames

fn <- list.files("data/", full.names = TRUE, pattern = "sfo.all")
fn

load(fn[1])

dim(tdf)
str(tdf)
class(tdf)

batchSize <- nrow(tdf)/6
set <- 6
defineSet <- function(batchSize, set) ((batchSize*set) - (batchSize - 1)):(batchSize*set)
defineSet(batchSize, 6)

out <- list()
for (i in 1:6) {
  rows <- defineSet(batchSize, i)
  out[[i]] <- tdf[rows, ]
}
names(out) <- c("frp", "medtdist", "medgeod", "degree", "cumldegree", "bcent")
str(out)

save(out, file = fn)
