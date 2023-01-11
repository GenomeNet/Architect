


## MBO: init design (random) --> repeat([fit model on prev points, predict good next point, eval point])

## 376 total init design (2 hr) +  [ 50 for rnn (x4), 40 for gap (x4)
## 50 * 24 2-hr-runs +
## 34 * 24 6-hr-runs +
## 22 * 24 20-hr-runs

## initdesign --> 752 hr == 31 days
## 2-hr-opt --> 100  days
## 6-hr-opt --> 204 days
## 20-hr-opt --> 440 days
## --> until 2-hr-opt end: 131 days /24  --> 5.5 days walltime
## --> until 6-hr-opt ends: 335 days / 24 --> 14.0 days walltime
## --> 20-hr-opt:  795 days / 24 --> 33.1 days walltime

##  (2*83+6*dob6+20*dob20)*24


library("data.table")
library("ggplot2")
library("ggbeeswarm")
library("ParamHelpers")

# run fit_surrogate_models.R and collect_run_results.R

# load("data/createdfiles.RData")
optruns <- readRDS("data/optruns.rds")

source("config/mboruns.R")


## similarity of chosen


clusterParams <- function(mindob) {
  cols <- optruns$optpath.x[optruns$metainfo$dob >= mindob, -"skip_block_fraction"]
  cormat <- sapply(cols, function(a) sapply(cols, function(b) abs(cor(a[!is.na(a) & !is.na(b)], b[!is.na(a) & !is.na(b)], method = "spearman"))))
  hclust(as.dist(1 - cormat))
}

dob.6hrs <- optruns$metainfo[walltime >= 6 * 3600, min(dob)]
dob.20hrs <- optruns$metainfo[walltime >= 20 * 3600, min(dob)]


plot(clusterParams(mindob = 20))
plot(clusterParams(mindob = dob.6hrs))
plot(clusterParams(mindob = dob.20hrs))


# residual_block <--> recurrent_type
# dilation_end <--> reverse_encoding
# max_pool_end <--> kealy_relu_alpha
# number_of_cnn_layers <--> dense activation
# kernel_size_0 <--> {number_of_cnn_layers, dense_activation}


lowerbounds <- as.list(rbindlist(lapply(optruns$paramsets, function(x) as.data.frame(as.list(getLower(x)))), fill = TRUE)[, lapply(.SD, min, na.rm = TRUE)])
upperbounds <- as.list(rbindlist(lapply(optruns$paramsets, function(x) as.data.frame(as.list(getUpper(x)))), fill = TRUE)[, lapply(.SD, max, na.rm = TRUE)])

bounds <- data.table(param = names(lowerbounds), lower = as.numeric(lowerbounds), upper = as.numeric(upperbounds), key = "param")

doTranspose <- function(optpath) {
  optpath <- copy(optpath)[, rownum := seq_len(.N)]

  for (cn in intersect(colnames(optpath), names(lowerbounds))) {
    ## if (is.integer(optpath[[cn]])) {
    ##   optpath[[cn]] <- optpath[[cn]] + rnorm(nrow(optpath), 0, .02)
    ## }
    optpath[[cn]] <- as.numeric(bounds[cn, (optpath[[cn]] - lower) / (upper - lower)])
  }

  transposed <- melt(optpath, id.vars = "rownum")[!is.na(value)]

  transposed[, variable := factor(variable, levels = colnames(optpath))][]
}


transposed <- doTranspose(optruns$optpath.x)
# doTranspose(optruns$optpath.x.transformed)

transposed[, runtype := runs[optruns$metainfo[transposed$rownum, runid], type]]
transposed[, time.bracket := as.ordered(optruns$metainfo[transposed$rownum, round(walltime / 3600)])]
transposed[, maxlen := optruns$metainfo[transposed$rownum, maxlen]]


info <- copy(optruns$metainfo)[, rowid := seq_len(.N)][, yrank := rank(-y, ties.method = "min"), by = "walltime"][, yq := yrank / .N, by = "walltime"]
transposed <- cbind(transposed, info[transposed, .(yq, yrank), on = c(rowid = "rownum")])



library("ggbeeswarm")

binaries <- transposed[, .(uniques = length(unique(value))), by = "variable"][uniques == 2, variable]

transposed <- transposed[!variable %in% binaries]



theme_set(theme_minimal())
theme_set(theme_bw())

ggplot(transposed, aes(x = variable, y = value)) + geom_quasirandom(dodge.width = .8, alpha = .5) + theme(axis.text.x = element_text(angle = 45))
ggplot(transposed, aes(x = variable, y = value, color = runtype)) + geom_quasirandom(dodge.width = .8, alpha = .5) + theme(axis.text.x = element_text(angle = 45))
ggplot(transposed, aes(x = variable, y = value, color = time.bracket)) + geom_quasirandom(dodge.width = .8, alpha = .5) + theme(axis.text.x = element_text(angle = 45))
ggplot(transposed[time.bracket <= 6 & maxlen < 200], aes(x = variable, y = value, color = time.bracket)) + geom_quasirandom(dodge.width = .8, alpha = .5) + theme(axis.text.x = element_text(angle = 45))


ml = 10000

topRows <- function(time, quantile = NULL, absolute = NULL) {
  info <- copy(optruns$metainfo)[, rowid := seq_len(.N)][round(maxlen) == ml][round(walltime) == time * 3600][, yrank := rank(-y, ties.method = "min"), by = "walltime"][, yq := yrank / .N, by = "walltime"]
  if (!is.null(quantile)) {
    info[yq < quantile, rowid]
  } else {
    info[yrank <= absolute, rowid]
  }
}

top.one <- c(topRows(2, .01), topRows(6, .01), topRows(20, .01))

top.hundred <- c(topRows(2, absolute = 100), topRows(6, absolute = 100), topRows(20, absolute = 100))
top.ten <- c(topRows(2, absolute = 10), topRows(6, absolute = 10), topRows(20, absolute = 10))
top.best <- c(topRows(2, absolute = 1), topRows(6, absolute = 1), topRows(20, absolute = 1))

ggplot(transposed[rownum %in% top.hundred], aes(x = variable, y = value)) + geom_quasirandom(dodge.width = .8, alpha = .5) + theme(axis.text.x = element_text(angle = 45))
ggplot(transposed[rownum %in% top.hundred], aes(x = variable, y = value, color = runtype)) + geom_quasirandom(dodge.width = .8, alpha = .5) + theme(axis.text.x = element_text(angle = 45))
ggplot(transposed[rownum %in% top.hundred], aes(x = variable, y = value, color = time.bracket)) + geom_quasirandom(dodge.width = .8, alpha = .5) + theme(axis.text.x = element_text(angle = 45))

theme_set(theme_bw())

ggplot(transposed[rownum %in% top.ten], aes(x = variable, y = value, color = time.bracket)) + geom_quasirandom(dodge.width = .8, alpha = 1) + theme(axis.text.x = element_text(angle = 45))


ggplot(rbind(transposed[rownum %in% top.ten][, ix:="ten"], transposed[rownum %in% top.hundred][, ix:="hundred"]), aes(x = variable, y = value, color = as.factor(time.bracket), alpha = ix)) +
  geom_quasirandom(dodge.width = .8) + theme(axis.text.x = element_text(angle = 45)) +
 scale_color_hue()

 scale_color_manual(values=c("red", "green", "blue"))



ggplot(transposed[time.bracket <= 6 & maxlen < 200], aes(x = variable, y = value, color = time.bracket, alpha = rownum %in% top.ten)) + geom_quasirandom(dodge.width = .8) + theme(axis.text.x = element_text(angle = 45))



ggplot(transposed[time.bracket <= 6 & maxlen < 200][,
  performance := factor(ifelse(rownum %in% top.ten, ifelse(rownum %in% top.best, "best", "top 10"), "others"), levels = c("others", "top 10", "best"))],
  aes(x = variable, y = value, color = time.bracket, alpha = performance)) +
  geom_quasirandom(dodge.width = .8) + theme(axis.text.x = element_text(angle = 45))


ggplot(transposed[time.bracket <= 6 & maxlen > 200][,
  performance := factor(ifelse(rownum %in% top.ten, ifelse(rownum %in% top.best, "top 10", "top 10"), "others"), levels = c("others", "top 10", "best"))],
  aes(x = variable, y = value, color = time.bracket, alpha = performance)) +
  geom_quasirandom(dodge.width = .8) + theme(axis.text.x = element_text(angle = 45))


ggsave("/tmp/x.pdf", scale = 0.7)



optruns$metainfo[topRows(20, absolute = 1)]

optruns$metainfo[topRows(20, absolute = 1)]





optruns$optpath.x[topRows(20, absolute = 1)]

topten <- copy(optruns$metainfo)[, rowid:=1:.N][round(maxlen) == 150 & round(walltime) == 20 * 3600][order(-y)][1:10]$rowid
toptx <- function(x)copy(optruns$metainfo)[, rowid:=1:.N][round(maxlen) == 150 & round(walltime) == 20 * 3600][order(-y)][1:x]$rowid

toptx <- function(x)copy(optruns$metainfo)[, rowid:=1:.N][round(maxlen) == 150 & round(walltime) == 6 * 3600][order(-y)][1:x]$rowid

toptx <- function(x)copy(optruns$metainfo)[, rowid:=1:.N][round(maxlen) == 10000 & round(walltime) == 20 * 3600][order(-y)][1:x]$rowid


tt <- function(x, time, type = "gap", mx=150)copy(cbind(optruns$metainfo, optruns$optpath.x.transformed[, .(model_type)]))[, rowid:=1:.N][model_type == type & round(maxlen) == mx & round(walltime) == time * 3600][order(-y)][1:x]$rowid


consensus <- function(x) {
  if (is.integer(x)) return(round(median(x)))
  if (is.numeric(x)) return(median(x))
  if (is.logical(x)) return(as.logical((median(x))))
  if (all(is.na(x))) return(x[[1]])
  return(names(sort(-table(x)))[[1]])
}

make.consensus <- function(number, evaltime, type, mx) {
  desc <- sprintf("median-of-%s_%shr_%s_len_%s", number, evaltime, type, mx)
  optruns$optpath.x.transformed[tt(number, evaltime, type, mx = mx)][, lapply(.SD, consensus)][, desc := desc]
}

consensustable <- rbindlist(do.call(Map, c(list(make.consensus), as.list(CJ(number = 9, mx = c(150, 10000), type = c("gap", "recurrent"), evaltime = c(2, 6, 20))))), fill = TRUE)

fwrite(
    consensustable
  , "median_of_9.csv")


plot(consensustable$filters_0, ylim = c(0, 1000))
points(consensustable$filters_end, col = "red")

plot(consensustable$kernel_size_0, ylim = c(0, 50))
points(consensustable$kernel_size_end, col = "red")

rbindlist(do.call(Map, c(list(make.consensus), as.list(CJ(number = 1, mx = c(150, 10000), type = c("gap", "recurrent"), evaltime = c(2, 6, 20))))), fill = TRUE)



cons <- rbind(
optruns$optpath.x.transformed[tt(1, 6)],
optruns$optpath.x.transformed[tt(10, 6)][, lapply(.SD, consensus)],
optruns$optpath.x.transformed[tt(1, 20)],
optruns$optpath.x.transformed[tt(10, 20)][, lapply(.SD, consensus)],
fill = TRUE
)

cbind(optruns$metainfo, optruns$optpath.x.transformed[, .(model_type)])[walltime < 70000 & round(maxlen) == 150 & model_type == "gap"][, sum(exec.time, na.rm = TRUE) / 3600 / 24]
cbind(optruns$metainfo, optruns$optpath.x.transformed[, .(model_type)])[round(maxlen) == 150 & model_type == "gap"][, sum(exec.time, na.rm = TRUE) / 3600 / 24]

cbind(optruns$metainfo, optruns$optpath.x.transformed[, .(model_type)])[walltime < 70000 & round(maxlen) == 150 & model_type == "recurrent"][, sum(exec.time, na.rm = TRUE) / 3600 / 24]
cbind(optruns$metainfo, optruns$optpath.x.transformed[, .(model_type)])[round(maxlen) == 150 & model_type == "recurrent"][, sum(exec.time, na.rm = TRUE) / 3600 / 24]

cbind(optruns$metainfo, optruns$optpath.x.transformed[, .(model_type)])[walltime < 70000 & round(maxlen) == 10000 & model_type == "recurrent"][, sum(exec.time, na.rm = TRUE) / 3600 / 24]
cbind(optruns$metainfo, optruns$optpath.x.transformed[, .(model_type)])[round(maxlen) == 10000 & model_type == "recurrent"][, sum(exec.time, na.rm = TRUE) / 3600 / 24]
cbind(optruns$metainfo, optruns$optpath.x.transformed[, .(model_type)])[walltime < 70000 & round(maxlen) == 10000 & model_type == "gap"][, sum(exec.time, na.rm = TRUE) / 3600 / 24]
cbind(optruns$metainfo, optruns$optpath.x.transformed[, .(model_type)])[round(maxlen) == 10000 & model_type == "gap"][, sum(exec.time, na.rm = TRUE) / 3600 / 24]

min(optruns$metainfo[tt(10, 6)]$dob)

min(optruns$metainfo[tt(10, 20)]$dob)


cbind(optruns$metainfo, optruns$optpath.x.transformed[, .(model_type)])[dob <= 77 & round(maxlen) == 150 & model_type == "gap"][, sum(exec.time, na.rm = TRUE) / 3600 / 24]

cbind(optruns$metainfo, optruns$optpath.x.transformed[, .(model_type)])[dob <= 109 & round(maxlen) == 150 & model_type == "gap"][, sum(exec.time, na.rm = TRUE) / 3600 / 24]

cbind(optruns$metainfo, optruns$optpath.x.transformed[, .(model_type)])[dob <= 61 & round(maxlen) == 150 & model_type == "gap"][, sum(exec.time, na.rm = TRUE) / 3600 / 24]

cbind(optruns$metainfo, optruns$optpath.x.transformed[, .(model_type)])[dob <= 85 & round(maxlen) == 150 & model_type == "gap"][, sum(exec.time, na.rm = TRUE) / 3600 / 24]

cons


plot(optruns$optpath.x.transformed[topten]$learning.rate)
plot(optruns$optpath.x.transformed[topten]$filters_0)
plot(optruns$optpath.x.transformed[topten]$filters_end)
plot(optruns$optpath.x.transformed[topten]$kernel_size_0)
plot(optruns$optpath.x.transformed[topten]$kernel_size_end)

plot(optruns$optpath.x.transformed[topten]$dense_layer_num)
plot(optruns$optpath.x.transformed[topten]$dense_layer_units)
plot(optruns$optpath.x.transformed[topten]$dense_activation)

optruns$optpath.x[toptx(50), plot(kernel_size_end, kernel_size_0)]
ggplot(cbind(optruns$metainfo[, .(y)], optruns$optpath.x.transformed)[order(y)], aes(x=log(kernel_size_end), y = log(kernel_size_0), color = y)) + geom_point()




optruns$optpath.x.transformed[toptx(100), plot(kernel_size_end, kernel_size_0, pch = model_type)]

source("R/searchspace.R")

config <- as.list(optruns$optpath.x.transformed[toptx(2)][1])
config <- as.list(optruns$optpath.x.transformed[toptx(2)][2])

config <- config[!is.na(config)]

source("R/gpuhelpers.R")

config$runid <- NULL

model <- do.call(create_model_genomenet, config)

model




## model:


## 13 (could also be 9) [[number_of_cnn_layers]]
## 1 (often more: 3, 7) [[conv_block_count]]
## --> conv_block_size: #cnn / #convblocks 13 (best), mostly 1, up to 4 (otherwise)
## --> number_of_cnn_layers gets rounded up from 7 to 9

## residual block: true [[may also be false, then dense layer num is 1]]

## best model: filters_exponent is rep(0, 13)
### --> best model: filters always ceil(filters_0) == **173**
##  most other models: filters_exponent is counting up from 0 to 1
### --> filters go from ~ 400+-100 to ~80 +- 20

### kernel size: kernel_size_0: 3, (sometimes 5) --> 18 (40 +- 20)

## dilation_end: 13 (otherwise ~ 2). higher dilation_end *with* residual block; lower without
##                                   lower dilation_end when conv block size is low

## max pooling steps:
##    doubling: round(log2(max_pool_end) * seq(0, 1, length.out = number_of_cnn_layers + 1))
## between 2, sometimes 4 (150 -> 38 and 150 -> 9)

## global average pooling: 38 x 173 --> 173 [[model_type]]
### could have had 1 dense layer here, but we don't
###   dense layer units: 200-400 [[dense_layer_units]]
###   dense layer activation: relu, always [[dense_activation]]
###   dropout: between .05 and .4 [[dropout]]
## dense --> 3

## skip_block_fraction ~ 0.5 +- .1
### best model: no skip blocks, since only one conv-block

## average(model, reverse complement)
## -->
## output


ishort <- lapply(createdfiles[1:3], readRDS)
ilong <- lapply(createdfiles[4:6], readRDS)
irshort <- lapply(createdfilesrec[1:3], readRDS)
irlong <- lapply(createdfilesrec[4:6], readRDS)





## i1 <- runmodel(getindex(10000, "gap"))
## i2 <- runmodel(getindex(10000, "gap")[[1]])
## i3 <- runmodel(getindex(10000, "gap")[[2]])


## m1 <- readRDS("indices_5,6_2022-03-17_00:16:01.rds")
## m2 <- readRDS("indices_5_2022-03-17_00:24:50.rds")
## m3 <- readRDS("indices_6_2022-03-17_00:32:01.rds")



## sapply(m1$opts, attr, "fitness")

## apply(cbind(sapply(m2$opts, attr, "fitness"), sapply(m3$opts, attr, "fitness")), 1, which.min)

## rbindlist(m1$opts)$residual_block


getbw <- function(m) {
  bws <- attributes((attributes(m$learner.model$next.model$learner.model)$covariance))
  structure(bws$range.val, names = bws$var.names)
}

getActualOptimum <- function(data) {
  rbindlist(lapply(split(data$data[, last(.SD[order(y)]), by = "walltime"], 1:3), function(x) {
    trafod <- ParamHelpers::trafoValue(par = data$ps, as.list(x)[names(data$ps$pars)])
    trafod$maxlen <- exp(x$maxlen)
    trafod$walltime <- exp(x$walltime)
    print(trafod$walltime)
    cleanUpX(trafod)
  }))
}




write.csv(getActualOptimum(ishort[[1]]), "optima_gap_maxlen_150.csv", row.names = FALSE)
write.csv(getActualOptimum(ilong[[1]]), "optima_gap_maxlen_10k.csv", row.names = FALSE)


write.csv(getActualOptimum(irshort[[1]]), "optima_recurrent_maxlen_150.csv", row.names = FALSE)
write.csv(getActualOptimum(irlong[[1]]), "optima_recurrent_maxlen_10k.csv", row.names = FALSE)

getActualOptimum(ishort[[1]])
getActualOptimum(ilong[[1]])
getActualOptimum(irshort[[1]])
getActualOptimum(irlong[[1]])


ggplot(ishort[[1]]$data, aes(x = walltime + rnorm(length(walltime)) / 10, y = y, color = as.factor(residual_block))) + geom_point()

ix <- ishort
ix <- ilong


for (xx in ix) {
  xx$data[, yhat := predict(xx$model, newdata = xx$data[, xx$model$features, with = FALSE])$data$response]
}


ix <- ishort
shortparams <-
ix <- ilong


# which of the individual residual_block models got the best fitness
sepmodeloptima <- apply(cbind(sapply(ix[[2]]$opts, attr, "fitness"), sapply(ix[[3]]$opts, attr, "fitness")), 1, which.min) - 1
combinedmodeloptima <- rbindlist(ix[[1]]$opts)$residual_block

sepmodeloptima
combinedmodeloptima

bandwidths <- rbindlist(lapply(ix, function(x) as.list(getbw(x$model))), fill = TRUE)

plot(apply(log(bandwidths), 2, function(x) diff(range(x))))

canonicalbw <- apply(bandwidths, 2, median, na.rm = TRUE)

singularresults <- rbindlist(Map(function(idx, x1, x2) c(list(x1, x2)[[idx + 1]], residual_block = idx), rbindlist(ix[[1]]$opts)$residual_block, ix[[2]]$opts, ix[[3]]$opts))
multiresult <- rbindlist(ix[[1]]$opts)


multiresult
singularresults

sweep((multiresult - singularresults[, colnames(multiresult), with = FALSE]), 2, canonicalbw[names(multiresult)], `/`)


actualbests <- rbind(
    tail(ix[[1]]$data[walltime == log(3600 * 2)][order(y)], 1),
    tail(ix[[1]]$data[walltime == log(3600 * 6)][order(y)], 1),
    tail(ix[[1]]$data[walltime == log(3600 * 20)][order(y)], 1)
)[, colnames(multiresult), with = FALSE]

sweep((multiresult - actualbests[, colnames(multiresult), with = FALSE]), 2, canonicalbw[names(multiresult)], `/`)


ggplot(ix[[1]]$data, aes(x = seq_along(y), y = y)) + geom_point()



tail(ix[[1]]$data[walltime == log(3600 * 2)][order(y)], 10)
tail(ix[[1]]$data[walltime == log(3600 * 6)][order(y)], 10)
tail(ix[[1]]$data[walltime == log(3600 * 20)][order(y)], 10)

tail(ix[[1]]$data[walltime == log(3600 * 2)][order(yhat)], 10)
tail(ix[[1]]$data[walltime == log(3600 * 6)][order(yhat)], 10)
tail(ix[[1]]$data[walltime == log(3600 * 20)][order(yhat)], 10)



ggplot(ix[[1]]$data, aes(x = y, y = yhat, color = as.factor(walltime))) + geom_point() + geom_hline(yintercept = -sapply(ix[[1]]$opts, attr, "fitness"))
ggplot(ix[[1]]$data, aes(x = y, y = yhat, color = as.factor(walltime))) + geom_point() + geom_hline(yintercept = -sapply(ix[[1]]$opts, attr, "fitness"))

ggplot(ix[[2]]$data, aes(x = y, y = yhat, color = as.factor(walltime))) + geom_point() + geom_hline(yintercept = -sapply(ix[[2]]$opts, attr, "fitness"))
ggplot(ix[[3]]$data, aes(x = y, y = yhat, color = as.factor(walltime))) + geom_point() + geom_hline(yintercept = -sapply(ix[[3]]$opts, attr, "fitness"))

plot(ix[[1]]$data[, y - yhat])
ix[[1]]$model$learner.model$next.model$learner.model





dt <- getfiles(getindex(10000, "gap"))$data

library("ggplot2")

ggplot(dt, aes(x = dense_layer_units, y = y, color = as.factor(walltime))) + geom_point()
ggplot(dt, aes(x = leaky_relu_alpha, y = y, color = as.factor(walltime))) + geom_point()
ggplot(dt, aes(x = learning.rate, y = y, color = as.factor(walltime))) + geom_point()
ggplot(dt, aes(x = dropout, y = y, color = as.factor(walltime))) + geom_point()
ggplot(dt, aes(x = filters_0, y = y, color = as.factor(walltime))) + geom_point()
ggplot(dt, aes(x = number_of_cnn_layers, y = y, color = as.factor(walltime), alpha = as.factor(residual_block))) + geom_point()


names(attributes(m1$model$learner.model$next.model$learner.model))


attributes(m1$model$learner.model$next.model$learner.model)$covariance


attributes((attributes(m1$model$learner.model$next.model$learner.model)$covariance))



plotx <- function(rr, time = 20, dim, scale, dim2 = NULL, scale2 = NULL, nscale2 = NULL, model = model2) {
  model$learner$predict.type <- "se"
  model$learner$next.learner$predict.type <- "se"
  scale <- seq(scale[[1]], scale[[2]], length.out = 100)
  rr$walltime <- log(3600 * time)
  ..gp <- function(...) ggplot() + geom_ribbon(..., alpha = .3) + geom_line(...)
  if (is.null(scale2)) {
    rr[[dim]] <- scale
    ..gp(data = as.data.frame(cbind(pred = predict(model, newdata = as.data.frame(rr)[model$features])$data, scale)),
      aes(x = scale, y = pred.response, ymin = pred.response - pred.se, ymax = pred.response + pred.se))
  } else {
    scale2 <- if (is.null(nscale2)) seq(scale2[[1]], scale2[[2]]) else seq(scale2[[1]], scale2[[2]], length.out = nscale2)
    rr[[dim]] <- rep(scale, length(scale2))
    rr[[dim2]] <- rep(scale2, each = length(scale))
    ..gp(data = as.data.frame(cbind(pred = predict(model, newdata = as.data.frame(rr)[model$features])$data, scale = rr[[dim]], scale2 = rr[[dim2]])),
      aes(x = scale, y = pred.response, ymin = pred.response - pred.se, ymax = pred.response + pred.se,
        group = as.factor(scale2), color = as.factor(scale2)))
  }
}

plotx(m1$opts[[1]], time = 20, dim = "learning.rate", c(2, 6), model = m1$model)
plotx(m2$opts[[1]], time = 20, dim = "learning.rate", c(2, 6), model = m2$model)
plotx(m3$opts[[1]], time = 20, dim = "learning.rate", c(2, 6), model = m3$model)
plotx(m1$opts[[1]], time = 20, dim = "learning.rate", c(2, 6), model = m1$model)
plotx(m1$opts[[1]], time = 20, dim = "dropout", c(0, 1), model = m1$model)
plotx(m1$opts[[1]], time = 20, dim = "dense_layer_units", c(4, 11), model = m1$model)
plotx(m1$opts[[1]], time = 20, dim = "optimizer", c(0, 1), model = m1$model)
plotx(m3$opts[[1]], time = 20, dim = "optimizer", c(0, 1), model = m3$model)




plots <- function(rr, time = 1, dim, scale, dim2, scale2, model = model2) {
  model$learner$predict.type <- "se"
  model$learner$next.learner$predict.type <- "se"

  scale <- seq(scale[[1]], scale[[2]], length.out = 100)
  scale2 <- seq(scale2[[1]], scale2[[2]], length.out = 100)
  rr$walltime <- log(3600 * time)
  rr[[dim]] <- rep(scale, length(scale2))
  rr[[dim2]] <- rep(scale2, each = length(scale))
  data = as.data.frame(cbind(pred = predict(model, newdata = as.data.frame(rr)[model$features])$data, scale = rr[[dim]], scale2 = rr[[dim2]]))
  filled.contour(scale, scale2, matrix(data$pred.response, nrow = length(scale)))
}


plotn <- function(rr, time = 1, dim, scale, model = model2) {
  model$learner$predict.type <- "se"
  scale <- seq(scale[[1]], scale[[2]], length.out = 100)
  rr = rr[walltime == log(3600 * time)]
  lines <- rbindlist(lapply(seq_len(nrow(rr)), function(i) {
    rrx <- as.list(rr[i, ])[model$features]
    rrx[[dim]] <- scale
    data.frame(cbind(pred = predict(model, newdata = as.data.frame(rrx))$data, scale = rrx[[dim]], point = i))
  }))

  gp <- function(...) ggplot() + geom_line(..., alpha = .3)
  gp(data = lines,
    aes(x = scale, y = pred.response, ymin = pred.response - pred.se, ymax = pred.response + pred.se,
      group = as.factor(point)))
}
