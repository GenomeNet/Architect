cluster.functions <- makeClusterFunctionsSocket(
  ncpus = max(length(strsplit(Sys.getenv("CUDA_VISIBLE_DEVICES"), ",")[[1]]), 1)
)
default.resources <- list(walltime = 300, memory = 60 * 1024, ncpus = 1)
