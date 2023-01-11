# this just works by setting up parallelMap and batchtools
# we need to get batchtools to work in the cluster for this.

# calling batchtools here
# interesting resources we can declare:
# - walltime
# - memory
# - ngpus
# - max.concurrent.jobs

# MAX.PARALLEL: parallelly running evaluations
MAX.PARALLEL = 3

WALLTIME_TOTAL = 60 * 60 * 47 # 1 day, 23 hours, dictated by cluster 2 days limit
MEMORY = 60 * 1024

library("parallelMap")

parallelStartBatchtools(bt.resources = list(
    ngpus = 1,
    walltime = WALLTIME_TOTAL,
    memory = MEMORY,
    max.concurrent.jobs = MAX.PARALLEL),
  logging = FALSE
)
