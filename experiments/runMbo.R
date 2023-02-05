#!/usr/bin/env Rscript

# initialize, or run, MBO
library("checkmate")

source("experiments/config/run.conf")
assertString(OUTPUTDIR, .var.name = "OUTPUTDIR, which should be defined in run.conf")

source("experiments/config/experimentinfo.R")
source("experiments/config/parallelization.R")  # start parallelization
source("experiments/config/mboruns.R")

library("GNArchitect")

options(warn = 1)

arguments <- commandArgs(trailingOnly=TRUE)

demoarg <- "--demo"

dodemo <- FALSE
if (demoarg %in% arguments) {
  arguments <- arguments[arguments != demoarg]
  dodemo <- TRUE
}

if (!length(arguments) ||
  !arguments[[1]] %in% c("init", "run") ||
  length(arguments) != 2 + (arguments[[1]] == "init") ||
  (arguments[[1]] == "init" && (!testInt(as.numeric(arguments[[3]]), lower = 1, upper = nrow(runs), tol = 1e-100)))
) {
  stop(sprintf("Usage: <script> init|run <filename> [<experiment index, between %s and %s>, only for 'init'] [--demo]", 1, nrow(runs)))
}

save.file.path = arguments[[2]]

if (dodemo) {
  fidelity <- data.table::rbindlist(list(
      list(walltimehrs = 2, iters = MAX.PARALLEL * 3),
      list(walltimehrs = 6, iters = MAX.PARALLEL * 4),
      list(walltimehrs = 20, iters = Inf)
  ))
} else {
  # use 'fidelity' from config/mboruns.R
}

## # Is now in mlrMBO
##
## predictLearner.regr.nuggetkm <- function(.learner, .model, ...) {
##   res <- NextMethod()
##   res[, 2] <- sqrt(pmax(res[, 2]^2 - .model$learner.model@covariance@nugget, 0))
##   res
## }
##
## registerS3method("predictLearner", "regr.nuggetkm", predictLearner.regr.nuggetkm)

if (arguments[[1]] == "init") {
  i <- assertInt(as.numeric(arguments[[3]]), lower = 1, upper = nrow(runs), tol = 1e-100)
  maxlen <- runs[i, maxlen]
  initMboRun(save.file.path,
    maxlen = maxlen, residual_block = runs[i, residual_block], type = runs[i, type], fidelity = fidelity,
    getEpochsDesired = getEpochsDesired, path = path, path.val = path.val, labels = labels, plgpath = plginfo[J(maxlen), path],
    outputdir = OUTPUTDIR, parallel = MAX.PARALLEL, testrun = dodemo)
} else {
  evaluateMboRun(save.file.path, fidelity, MAX.PARALLEL)
}

