#!/usr/bin/env Rscript

filepath <- commandArgs(trailingOnly = TRUE)

load(filepath)

subsetOptPath <- function(opt.path, which) {
  checkmate::assertNames(names(opt.path$env), permutation.of = c("error.message", "exec.time", "extra", "dob", "eol", "path"))
  opt.path$env$error.message <- opt.path$env$error.message[which]
  opt.path$env$exec.time <- opt.path$env$exec.time[which]
  opt.path$env$extra <- opt.path$env$extra[which]
  opt.path$env$dob <- opt.path$env$dob[which]
  opt.path$env$eol <- opt.path$env$eol[which]
  opt.path$env$path <- opt.path$env$path[which, , drop = FALSE]
}

keep <- data.table::as.data.table(opt.state$opt.path)[, y > 0 | dob == 0 | !is.na(error.message)]

subsetOptPath(opt.state$opt.path, keep)

all(data.table::as.data.table(opt.state$opt.path)[, y > 0 | dob == 0 | !is.na(error.message)])

save(opt.state, file = filepath)
