

ggplot(copy(optpath.xyz)[dob > 0][, .(xtime = max(exec.time, na.rm = TRUE)), by = c("runid", "dob")][, alltime := cumsum(nafill(xtime, fill = 0)), by = "runid"], aes(x = dob, y = alltime / 3600 / 24, color = as.factor(runid))) + geom_point()


ggplot(optpath.xyz, aes(x = dob, y = y, color = as.factor(runid))) + geom_jitter() + ylim(0.3, 1)

ggplot(optpath.xyz, aes(x = dob, y = y, color = as.factor(runid))) + geom_jitter() + ylim(0, 1)



ggplot(copy(optpath.xyz)[, .(y = c(1/3, max(y, na.rm = TRUE))), by = c("dob", "runid")][, ymax := cummax(y), by = "runid"], aes(x = dob, y = ymax, color = as.factor(runid))) + geom_line() + ylim(0.3, 1) + geom_vline(xintercept = 50) + geom_vline(xintercept = 84)

ggplot(copy(optpath.xyz)[, `:=`(ymax = cummax(y), dob = seq_along(y)), by = "runid"], aes(x = dob, y = ymax, color = as.factor(runid))) + geom_line() + ylim(0.3, 1) + geom_vline(xintercept = 50) + geom_vline(xintercept = 84)

ggplot(optpath.xyz, aes(x = dob, y = y, color = as.factor(runid))) + geom_jitter() + ylim(0, 1)

optpath.xyz




ggplot(optpath.xyz, aes(x = dob, y = perf.loss, color = as.factor(runid))) + geom_jitter() + ylim(0.3, 1)

ggplot(optpath.xyz, aes(x = dob, y = numepochs, color = as.factor(runid))) + geom_jitter()
ggplot(optpath.xyz, aes(x = dob, y = log(exec.time / 3600 / 2) / log(3), color = as.factor(runid))) + geom_jitter()
plot(sort(optpath.xyz$exec.time))

# excess time log10 / hours
plot(log10(sort(optpath.xyz$exec.time - exp(optpath.xyz$walltime)) / 3600))

ggplot(optpath.xyz[dob > 0 & y > 0], aes(x = dob, y = y, color = as.factor(runid))) + geom_point() + geom_smooth()


ggplot(optpath.xyz[, .(y = mean(y, na.rm = TRUE)), by = c("dob", "runid")], aes(x = dob, y = y, color = as.factor(runid))) + geom_line() + ylim(0.4, 1)
ggplot(optpath.xyz[, .(y = max(y, na.rm = TRUE)), by = c("dob", "runid")], aes(x = dob, y = y, color = as.factor(runid))) + geom_line() + ylim(0.4, 1)

ggplot(optpath.xyz[, .(y = cummax(y), dob), by = c("runid")], aes(x = dob, y = y, color = as.factor(runid))) + geom_line() + ylim(0.4, 1)

# ggplot(optpath.xyz, aes(group = paste0(dob, "_", runid), y = y, color = as.factor(runid))) + geom_boxplot() + ylim(0.4, 1)


alpha=0.8
optpath.lag <- copy(optpath.xyz)[, .(y = mean(y)), by = c("dob", "runid")][
    order(dob), .(dob = dob, y = y, y.lag = Reduce(function(x, y) x * alpha + y * (1 - alpha), y, 1 / 3, accumulate = TRUE)[-1]), by = "runid"]
ggplot(optpath.lag, aes(x = dob, y = y.lag, color = as.factor(runid))) + geom_line() + geom_line(aes(y = y), alpha = 0.1)


optpath.xyz[dob != 0, .N, by = "runid"][order(N)]




for (indep in colnames(optpath.x[, -"runid"])) {
  print(ggplot(optpath.xyz, aes(x = get(indep), y = y, color = dob)) + geom_point() + xlab(indep) + facet_grid(maxlen ~ .) + ggtitle("Accuracy"))
  readLines(n = 1)
}

for (indep in colnames(optpath.x[, -"runid"])) {
  print(ggplot(optpath.xyz, aes(x = get(indep), y = exp(-perf.loss), color = dob)) + geom_point() + xlab(indep) + facet_grid(maxlen ~ .) + ggtitle("Accuracy"))
  readLines(n = 1)
}


for (indep in colnames(optpath.x[, -"runid"])) {
  print(ggplot(copy(optpath.xyz)[, dobmax := max(dob), by = "runid"][dob >= dobmax - 3],
    aes(x = get(indep), y = y, color = as.factor(runid))) + geom_point() + xlab(indep) + facet_grid(maxlen ~ .) + ggtitle("Accuracy"))
  readLines(n = 1)
}

for (indep in colnames(optpath.x[, -"runid"])) {
  print(ggplot(copy(optpath.xyz)[, ismax := rank(-y) <= 5, by = "runid"][(ismax)],
    aes(x = get(indep), y = y, color = as.factor(runid))) + geom_point() + xlab(indep) + facet_grid(maxlen ~ .) + ggtitle("Accuracy"))
  readLines(n = 1)
}

for (indep in colnames(optpath.x[, -"runid"])) {
  print(ggplot(optpath.xyz, aes(x = dob, y = get(indep), color = as.factor(runid)))
    + geom_point() + ylab(indep) + facet_grid(maxlen ~ .) + ggtitle("Accuracy") + geom_smooth())
  readLines(n = 1)
}




colnames(optpath.x)[apply(is.na(optpath.x), 2, any)]


bsdata <- copy(cbind(optpath.x, batchsize = optpath.z$batchsize))[optpath.z$dob == 0][, `:=`(walltime = NULL, runid = NULL)]
bsdataplus <- cbind(bsdata, paramcount = optpath.z[dob == 0, paramcount])
bsdataplusrestricted <- cbind(bsdata, paramcount = optpath.z[dob == 0, paramcount])[log(batchsize) > 2 & batchsize != max(batchsize)]

summary(lm(I(batchsize) ~ ., data = bsdata))
summary(lm(I(1/batchsize) ~ ., data = bsdata))

summary(lm(I(log(batchsize)) ~ ., data = bsdata))

summary(lm(I(log(batchsize)) ~ ., data = bsdataplus))
summary(lm(I(log(batchsize)) ~ ., data = bsdataplusrestricted))

li <- summary(lm(I(log(batchsize)) ~ filters_0 + filters_end + max_pool_end + number_of_cnn_layers + reverse_encoding + maxlen + residual_block,
  data = bsdata))

li2 <- summary(lm(I(log(batchsize)) ~ filters_0 + filters_end + max_pool_end + number_of_cnn_layers + I(number_of_cnn_layers^2) + reverse_encoding + maxlen + residual_block,
  data = bsdata))

li3 <- summary(lm(I(log(batchsize)) ~ filters_0 + filters_end + max_pool_end + number_of_cnn_layers +
                    reverse_encoding + maxlen + residual_block + paramcount + skip_block_fraction,
  data = bsdataplus))


mod4 <- lm(I(log(batchsize)) ~ filters_0 + filters_end + max_pool_end + number_of_cnn_layers +
                    reverse_encoding + maxlen + residual_block + paramcount + skip_block_fraction,
  data = bsdataplusrestricted)
li4 <- summary(mod4)


plot(bsdataplus[, log(batchsize)])
plot(li$residuals)
plot(li2$residuals)
plot(li3$residuals)
plot(li4$residuals)

plot(predict(mod4, bsdataplusrestricted), li4$residuals)
plot(li4$residuals, log10(bsdataplusrestricted$batchsize))


for (indep in colnames(bsdata[, -"batchsize"])) {
  print(ggplot(bsdata, aes(x = get(indep), y = batchsize, color = rank(li$residuals) > length(li$residuals) * .9)) + scale_y_continuous(trans="log10") +
    geom_point() + xlab(indep) + facet_grid(maxlen ~ .) + ggtitle("Batchsize"))
  readLines(n = 1)
}



for (indep in colnames(bsdata[, -"batchsize"])) {
  print(ggplot(bsdataplusrestricted, aes(x = get(indep), y = batchsize, color = rank(li4$residuals) > length(li4$residuals) * .9)) + scale_y_continuous(trans="log10") +
    geom_point() + xlab(indep) + facet_grid(maxlen ~ .) + ggtitle("Batchsize"))
  readLines(n = 1)
}


bsdata[order(-li$residuals)[1:10]]



sort(optpath.z$batchsize)[1:10]

optpath.xyz[batchsize == 0]


## Interesting:
# filters_0, filters_end(slightly), max_pool_end(slightly), number_of_cnn_layers
for (indep in colnames(optpath.x[, -"runid"])) {
  print(ggplot(optpath.xyz, aes(x = get(indep), y = batchsize, color = dob)) + geom_point() + xlab(indep) + facet_grid(maxlen ~ .) + scale_y_continuous(trans="log10") +
    ggtitle("Batchsize"))
  readLines(n = 1)
}


for (indep in colnames(optpath.x[, -"runid"])) {
  print(ggplot(optpath.xyz, aes(x = get(indep), y = log(numepochs * stepsperepoch), color = dob)) + geom_point() + xlab(indep) + facet_grid(maxlen ~ .) +
    ggtitle("log(number of steps done)"))
  readLines(n = 1)
}

for (indep in colnames(optpath.x[, -"runid"])) {
  print(ggplot(optpath.xyz, aes(x = get(indep), y = log(numepochs * stepsperepoch * batchsize), color = dob)) + geom_point() + xlab(indep) + facet_grid(maxlen ~ .) +
    ggtitle("log(number of samples evaluated)"))
  readLines(n = 1)
}


###### actual results

results <- rbindlist(list(
  list(recurrent = character(0), testperf = numeric(0), stage = numeric(0), median = logical(0), len = numeric(0), parameters = numeric(0)),
  list(recurrent = "gap", testperf = .8001179, stage = 6, median = TRUE, len = 150, parameters = 4273879),
  list(recurrent = "gap", testperf = .7880966, stage = 20, median = TRUE, len = 150, parameters = 3154361),
  list(recurrent = "rnn", testperf = .7894199, stage = 6, median = TRUE, len = 150, parameters = 1519119),
  list(recurrent = "rnn", testperf = .7807715, stage = 20, median = TRUE, len = 150, parameters = 2463535),
  list(recurrent = "gap", testperf = .7978408, stage = 6, median = FALSE, len = 150, parameters = 3471893),
  list(recurrent = "rnn", testperf = .7904803, stage = 6, median = FALSE, len = 150, parameters = 1606250),
  list(recurrent = "rnn", testperf = .7788247, stage = 2, median = FALSE, len = 150, parameters = 8050761),
  list(recurrent = "gap", testperf = .7608769, stage = 2, median = FALSE, len = 150, parameters = 2675616),
  list(recurrent = "gap", testperf = .7515309, stage = 20, median = FALSE, len = 150, parameters = 3305341),
  list(recurrent = "rnn", testperf = .739959,  stage = 20, median = FALSE, len = 150, parameters = 2845369),
  list(recurrent = "rnn", testperf = .9889531,  stage = 2, median = TRUE, len = 10000, parameters = 4015623),
  list(recurrent = "gap", testperf = .9870114,  stage = 2, median = FALSE, len = 10000, parameters = 1367073),
  list(recurrent = "rnn", testperf = .987891,  stage = 2, median = FALSE, len = 10000, parameters = 3704628),
  list(recurrent = "rnn", testperf = .9843555,  stage = 6, median = TRUE, len = 10000, parameters = 117200),
  list(recurrent = "gap", testperf = .9868849,  stage = 6, median = TRUE, len = 10000, parameters = 768772),
  list(recurrent = "gap", testperf = .9837772,  stage = 20, median = TRUE, len = 10000, parameters = 989956),
  list(recurrent = "rnn", testperf = .9888797,  stage = 20, median = TRUE, len = 10000, parameters = 1134975),
  list(recurrent = "rnn", testperf = .9887034,  stage = 20, median = FALSE, len = 10000, parameters = 802452),
  list(recurrent = "gap", testperf = .986721,  stage = 20, median = FALSE, len = 10000, parameters = 695248),
  list(recurrent = "gap", testperf = .9864071,  stage = 6, median = FALSE, len = 10000, parameters = 714802),
  list(recurrent = "rnn", testperf = .9858881,  stage = 6, median = FALSE, len = 10000, parameters = 175426),
  list(recurrent = "fiannaca", testperf = .750392, stage = 30, median = FALSE, len = 150, parameters = 20467293),
  list(recurrent = "Viraminer", testperf = .710429, stage = 30, median = FALSE, len = 150, parameters = 2535803),
  list(recurrent = "Deepvirfinder", testperf = .7038548, stage = 30, median = FALSE, len = 150, parameters = 1045003),
  list(recurrent = "Cheer-onehot", testperf = .6443927, stage = 30, median = FALSE, len = 150, parameters = 1613827),
  list(recurrent = "pprmeta", testperf = .6356797, stage = 30, median = FALSE, len = 150, parameters = 542595),
  list(recurrent = "Seeker", testperf = .4623374, stage = 30, median = FALSE, len = 150, parameters = 218),
  list(recurrent = "fiannaca", testperf = .9863857, stage = 30, median = FALSE, len = 10000, parameters = 20467293),
  list(recurrent = "Deepvirfinder", testperf = .9803515, stage = 30, median = FALSE, len = 10000, parameters = 1045003),
  list(recurrent = "Viraminer", testperf = .9764851, stage = 30, median = FALSE, len = 10000, parameters = 2535803),
  list(recurrent = "Cheer-onehot", testperf = .9715599, stage = 30, median = FALSE, len = 10000, parameters = 1613827),
  list(recurrent = "pprmeta", testperf = .8803204, stage = 30, median = FALSE, len = 10000, parameters = 542595),
  list(recurrent = "Seeker", testperf = .494711, stage = 30, median = FALSE, len = 10000, parameters = 218)
))

ggplot(results[len == 150], aes(x = as.factor(stage), y = testperf, fill = paste0(median, recurrent))) + geom_bar(stat = "identity", position = position_dodge())

ggplot(results[len == 10000], aes(x = as.factor(stage), y = testperf, fill = paste0(median, recurrent))) + geom_bar(stat = "identity", position = position_dodge())

ggplot(results[len == 150], aes(x = as.factor(stage), y = testperf, color = paste0(recurrent), alpha = median * -1)) + geom_point(size = 10)

ggplot(results[len == 10000 & testperf > .9 & stage != 0], aes(x = as.factor(stage), y = testperf, color = recurrent, alpha = median * -1)) + geom_point(size = 10)



ggplot(results[recurrent != "Seeker" & len == 10000 & stage != 20 & !median], aes(x = (parameters), y = testperf, color = recurrent)) + scale_x_log10() + geom_point(size = 10)

ggplot(results[recurrent != "Seeker" & len == 150 & stage %in% c(6, 30) & !median], aes(x = (parameters), y = testperf, color = recurrent)) + scale_x_log10() + geom_point(size = 10)



