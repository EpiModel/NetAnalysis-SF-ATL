
## TSNA Hyak

suppressMessages(library("tsna"))
suppressMessages(library("EpiModel"))
suppressMessages(library("doParallel"))

city <- Sys.getenv("CITY")
if (city == "atl") {
  sim <- readRDS("data/artnet.NetSim.Atlanta.rda")
} else {
  sim <- readRDS("data/artnet.NetSim.SanFrancisco.rda")
}

net <- Sys.getenv("NET")
if (net == "main") {
  sim <- sim[[1]]
} else if (net == "casl") {
  sim <- sim[[2]]
} else if (net == "inst") {
  sim <- sim[[3]]
} else if (net == "all") {
  sim_main <- sim[[1]]
  sim_casl <- sim[[2]]
  sim_inst <- sim[[3]]

  sim_all <- sim_main
  sim_casl_df <- as.data.frame(sim_casl)
  sim_inst_df <- as.data.frame(sim_inst)

  sim_all <- add.edges.active(sim_all, tail = sim_casl_df[[3]], head = sim_casl_df[[4]],
                             onset = sim_casl_df[[1]], terminus = sim_casl_df[[2]])

  sim_all <- add.edges.active(sim_all, tail = sim_inst_df[[3]], head = sim_inst_df[[4]],
                             onset = sim_inst_df[[1]], terminus = sim_inst_df[[2]])
  sim <- sim_all
}

simset <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))

batchSize <- 25
v <- ((batchSize*simset) - (batchSize - 1)):(batchSize*simset)

int <- as.numeric(Sys.getenv("INT"))
ts <- seq(1, 260, int)

f <- function(net, v, ts) {
  m <- array(NA, dim = c(length(ts), length(v), 5))
  for (jj in 1:length(v)) {
    for (ii in 1:length(ts)) {
      tp <- tsna::tPath(sim, v = v[jj], start = 1, end = ts[ii], direction = "fwd")
      # forward reachable path
      m[ii, jj, 1] <- sum(tp$tdist < Inf)
      # median temporal distance
      m[ii, jj, 2] <- median(tp$tdist[tp$tdist < Inf])
      # median geodesic steps
      m[ii, jj, 3] <- median(tp$gsteps[tp$gsteps < Inf])
      # cross-sectional degree
      m[ii, jj, 4] <- EpiModel::get_degree(network.collapse(sim, at = ts[ii]))[v[jj]]
      # cumulative degree
      m[ii, jj, 5] <- EpiModel::get_degree(network.collapse(sim, onset = 1, terminus = ts[[ii]]))[v[jj]]
      # betweenness centrality
      # m[ii, jj, 6] <- sna::betweenness(network.collapse(sim, at = ts[ii]), nodes = v[jj])
    }
  }
  return(m)
}

registerDoParallel(parallel::detectCores())
out <- foreach(vv = 1:length(v)) %dopar% {
  f(sim, v[vv], ts)
}
df <- do.call("cbind", out)

fn <- paste(city, net, int, stringr::str_pad(simset, 3, pad = "0"), "rda", sep = ".")
save(df, file = paste0("data/", fn))
