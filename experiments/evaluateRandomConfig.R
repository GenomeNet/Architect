#!/usr/bin/env Rscript

# Evaluate a single invocation of the objective function.
# maxlen, type, and residual_block are chosen randomly.
# 
# Usage: evaluateRandomConfig.R <seed>

library("GNArchitect")

source("experiments/config/run.conf")
assertString(OUTPUTDIR, .var.name = "OUTPUTDIR, which should be defined in run.conf")


source("experiments/config/experimentinfo.R")  # loads path, path.val, labels, plginfo, and getEpochsDesired


set.seed(as.numeric(commandArgs(trailingOnly = TRUE)[[1]]))

MAXLEN <- sample(c(150, 10000), 1)
RESIDUALBLOCK <- sample(0:1, 1)
TYPE <- sample(c("recurrent", "gap"), 1)
WALLTIME <- 3600

ss <- get_searchspace(MAXLEN, TYPE)
ss <- c(ss, pSS(
  walltime: numeric[log(WALLTIME), log(WALLTIME)] [[trafo = function(x) exp(x)]],
  maxlen: numeric[log(MAXLEN), log(MAXLEN)] [[trafo = function(x) exp(x)]],
  residual_block: integer[RESIDUALBLOCK, RESIDUALBLOCK]
))

x <- sampleValue(ss, trafo = TRUE)

obj.genomenet.fn <- makeGenomeNetObjective(MAXLEN, TYPE, getEpochsDesired, path, path.val, labels, plginfo[J(MAXLEN), path], OUTPUTDIR) {

print(system.time(res <- obj.genomenet.fn(x)))

print(res)
