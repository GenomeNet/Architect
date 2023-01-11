
library("GNArchitect")
library("data.table")
library("ParamHelpers")

source("experiments/config/mboruns.R")  # use the `runs` data.table to map run-index
source("experiments/config/experimentinfo.R")  # use getEpochsDesired, labels

# The pattern of result files, created with `experiments/runMbo.R`.
#
# We assume that the `runs` variable is a data.table defined in `experiments/config/mboruns.R`
# with columns `maxlen`, `type` and `residual_block`. The line `i` of that table defines the
# setup used to generate `data/opt_<i>.RData`.
basepath <- "data/opt_%s.RData"

# call `GNArchitect::evaluatePerf` on all entries of an opt.path,
# returning the resulting performance for each entry as a vector.
# - opt.path: run database to extract performance values from
# - perftype (`character(1)`): which entry of `extras$.train_results$metrics` to use. One of `"loss"`, `"acc"`, `"val_loss"`, `"val_acc"`.
# - perfmul: 1 if performance is to be maximized (e.g. accuracy), -1 if performance is minimized (e.g. loss)
extractperf <- function(opt.path, perftype, perfmul) {
  walltimes <- exp(as.data.table(opt.path, include.x = TRUE, include.y = FALSE, include.rest = FALSE)$walltime)
  epochs.desired <- vapply(walltimes, getEpochsDesired, numeric(1))

  extra <- opt.path$env$extra
  perfmul * mapply(
    function(ex, ed) {
      if (!length(ex$.train_result$metrics[[perftype]])) {
        NA
      } else {
        evaluatePerf(perfmul * ex$.train_result$metrics[[perftype]], ed)  # exported from GNArchitect
      }
    },
    extra, epochs.desired
  )
}

# get data.table of actual hyperparameter values.
# This entails applying the transformation (e.g. exponentiating log-space hyperparameters)
# and also calling `GNArchitect::cleanUpX()`
transformOptPath <- function(data, ps) {
  rbindlist(lapply(split(data, seq_len(nrow(data))), function(x) {
    trafod <- ParamHelpers::trafoValue(par = ps, as.list(x)[names(ps$pars)])
    cleanUpX(trafod, num_targets = length(labels))  # labels from experimentinfo.R
  }))
}

# collect results from all runs into common tables.
optpath.x <- data.table()  # the "x"-values: points of the search space that were evaluated.
optpath.x.transformed <- data.table()  # the "x"-values, but transformed: log-scale parameters are exponentiated, `GNArchitect::cleanUpX()` was applied.
optpath.y <- data.table()  # "y"-values: results of the evaluation, i.e. performance values.
optpath.z <- data.table()  # further meta-information.

paramsets <- list()

for (i in seq_len(nrow(runs))) {
  load(sprintf(basepath, i))
  if (is.null(opt.state$opt.path)) next
  ps <- getParamSet(opt.state$opt.problem$fun)
  new.x <- as.data.table(opt.state$opt.path, include.x = TRUE, include.y = FALSE, include.rest = FALSE)
  new.x.transformed <- transformOptPath(new.x, ps)
  new.y <- as.data.table(opt.state$opt.path, include.x = FALSE, include.y = TRUE, include.rest = FALSE)
  new.z <- cbind(as.data.table(opt.state$opt.path, include.x = FALSE, include.y = FALSE, include.rest = TRUE),
    numepochs = sapply(opt.state$opt.path$env$extra, function(x) length(x$.train_result$metrics$val_acc)),
    perf.acc = extractperf(opt.state$opt.path, "val_acc", 1),
    perf.loss = extractperf(opt.state$opt.path, "val_loss", -1)
  )

  optpath.x <- rbind(optpath.x, cbind(new.x, runid = i), fill = TRUE)
  optpath.x.transformed <- rbind(optpath.x.transformed, cbind(new.x.transformed, runid = i), fill = TRUE)
  optpath.y <- rbind(optpath.y, cbind(new.y, runid = i), fill = TRUE)
  optpath.z <- rbind(optpath.z, cbind(new.z, runid = i), fill = TRUE)

  paramsets[[i]] <- ps
}

# truncate error messages, if any
optpath.z$error.message <- substr(optpath.z$error.message, 1, 50)

# the following may be desired if there are runs with errors in the datasets
## exclude <- !is.na(optpath.z$error.message)
## optpath.x <- optpath.x[!exclude]
## optpath.y <- optpath.y[!exclude]
## optpath.z <- optpath.z[!exclude]

# combine: optpath.xy is optpath.x + optpath.y etc.
optpath.xy <- cbind(optpath.x[, -"runid"], optpath.y)
optpath.xyz <- cbind(optpath.x[, -"runid"], optpath.y[, -"runid"], optpath.z)

optpath.yz <- cbind(optpath.y[, -"runid"], optpath.z)

# walltime, maxlen etc. is also part of meta-information, so we include it
optpath.yz$walltime <- exp(optpath.x$walltime)
optpath.yz$maxlen <- exp(optpath.x$maxlen)
optpath.x$walltime <- NULL
optpath.x$maxlen <- NULL
optpath.x$runid <- NULL

optpath.yz$eol <- NULL
optpath.yz$error.model <- NULL
# train.time is the surrogate model train time, but it is only recorded for one out of all the `MAXPARALLEL` runs with the same DOB.
optpath.yz[, train.time := mean(train.time, na.rm = TRUE), by = c("dob", "runid")]

saveRDS(list(
  INFO = "optpath.x: evaluated hyperparameters, pre-transformation (i.e. mostly on log-scale)\noptpath.x.transformed: evaluated hyperparameters, transformed to arguments of create_model_genomenet()\nparamsets: parameter sets of individual runs\nmetainfo: further information about each evaluation: performance ('y' -- the performance seen by MBO; 'perfo.acc', 'perf.loss' -- offline calculated values), evaluation statistics ('error.message', 'exec.time' -- includes time for batch size estimation), MBO-internal details ('cb'...'lambda'), information about the model and its invocation ('batchsize'...'numepochs'), runid (index into 'runs' table in config/mboruns.R), time limit given to trainNetwork ('walltime')",
  optpath.x = optpath.x,
  optpath.x.transformed = optpath.x.transformed,
  paramsets = paramsets,
  metainfo = optpath.yz
), file = "data/optruns.rds")
