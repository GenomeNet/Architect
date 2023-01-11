



library("ggplot2")
library("data.table")

MAXLEN <- c(150, 10000)
TYPE <- c("recurrent", "gap")
RESIDUAL_BLOCK <- c(0, 1)
runs <- CJ(maxlen = MAXLEN, type = TYPE, residual_block = RESIDUAL_BLOCK)

load("analysis/opt_try1_1.RData")

load("analysis/opt_try1_4.RData")

runs[4]



dtx <- as.data.table(as.data.frame(opt.state$opt.path))


allpaths <- rbindlist(lapply(seq_len(ParamHelpers::getOptPathLength(opt.state$opt.path)), function(i) {
  dti <- ParamHelpers::getOptPathEl(opt.state$opt.path, i)
  accs <- dti$extra$.train_result$metrics$val_acc
  if (is.null(accs)) {
    data.table(el = i, accs = 0, times = 0)
  } else {
    data.table(el = i, accs = accs, times = seq(0, dti$exec.time, length.out = length(accs) + 1)[-1])
  }
}))

ggplot(allpaths, aes(x = times, y = accs + el, color = as.factor(el))) + geom_point() + geom_line() + theme_bw()

ggplot(allpaths, aes(x = times, y = accs, color = as.factor(el))) + geom_point() + geom_line() + theme_bw()

ggplot(dtx, aes(x = dob, y = y)) + geom_jitter()

last(dtx, 3)



colnames(dtx)

for (col in colnames(dtx)) {
  cat(sprintf("Column: %s\n", col))
  print(ggplot(dtx, aes(x = get(col), y = y, color = as.factor(dob))) + geom_point())
  readLines(n = 1)
}

for (col in colnames(dtx)) {
  cat(sprintf("Column: %s\n", col))
  print(ggplot(dtx, aes(x = get(col), y = exec.time, color = as.factor(dob))) + geom_point())
  readLines(n = 1)
}


colnames(dtx)


ggplot(dtx, aes(x = paramcount, y = exec.time)) + geom_point()


ggplot(dtx, aes(x = skip_block_fraction, y = y, color = as.factor(dob))) + geom_point()
