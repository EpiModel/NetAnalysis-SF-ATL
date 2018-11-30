
## TSNA Hyak

suppressMessages(library("tsna"))
suppressMessages(library("doParallel"))

city <- Sys.getenv("CITY")
if (city == "atl") {
  sim1 <- readRDS("data/artnet.NetSim.Atlanta.sim1.rda")
} else {
  sim1 <- readRDS("data/artnet.NetSim.SanFrancisco.sim1.rda")
}

net <- Sys.getenv("NET")
if (net == "main") {
  sim1 <- sim1[[1]]
} else if (net == "casl") {
  sim1 <- sim1[[2]]
} else if (net == "inst") {
  sim1 <- sim1[[3]]
} else if (net == "all") {
  s1_main <- sim1[[1]]
  s1_casl <- sim1[[2]]
  s1_inst <- sim1[[3]]

  s1_all <- s1_main
  s1_casl_df <- as.data.frame(s1_casl)
  s1_inst_df <- as.data.frame(s1_inst)

  s1_all <- add.edges.active(s1_all, tail = s1_casl_df[[3]], head = s1_casl_df[[4]],
                             onset = s1_casl_df[[1]], terminus = s1_casl_df[[2]])

  s1_all <- add.edges.active(s1_all, tail = s1_inst_df[[3]], head = s1_inst_df[[4]],
                             onset = s1_inst_df[[1]], terminus = s1_inst_df[[2]])
  sim1 <- s1_all
}

simset <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
v <- ((100*simset)-99):(100*simset)

int <- as.numeric(Sys.getenv("INT"))
ts <- seq(1, 260, int)

f <- function(net, v, ts) {
  m <- matrix(NA, nrow = length(ts), ncol = length(v))
  for (jj in 1:length(v)) {
    for (ii in 1:length(ts)) {
      m[ii, jj] <- sum(tPath(net, v = v[jj],
                             start = 1, end = ts[ii],
                             direction = "fwd")$tdist < Inf)
    }
  }
  return(m)
}

registerDoParallel(parallel::detectCores())
out <- foreach(vv = 1:length(v)) %dopar% {
  f(sim1, v[vv], ts)
}
df <- do.call("cbind", out)

fn <- paste(city, net, int, simset, "rda", sep = ".")
save(df, file = paste0("data/", fn))
