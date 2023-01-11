source("redisjobqueue/redisworker.R", local = environment())  # batchtools.conf gets sourced inside the env of the registry, we make sure

cluster.functions <- makeClusterFunctionsRedis("redisjobqueue/redisdir")
default.resources <- list(walltime = 300, memory = 60 * 1024, ncpus = 1)
max.concurrent.jobs <- 1000000000L
