#!/usr/bin/env Rscript

# Copies an optpath save file to a new location and removes all evaluations except for the initial design.
# Usage: resetRuns.R <from> <to>

filepath <- commandArgs(trailingOnly = TRUE)

checkmate::assertCharacter(filepath, len = 2, any.missing = FALSE, pattern = "^[^']+$")

if (file.exists(filepath[[2]])) stop("Output file exists.")
filepath[[2]] <- system(sprintf("realpath '%s'", filepath[[2]]), intern=TRUE)

load(filepath[[1]])

subsetOptPath <- function(opt.path, which) {
  checkmate::assertNames(names(opt.path$env), permutation.of = c("error.message", "exec.time", "extra", "dob", "eol", "path"))
  opt.path$env$error.message <- opt.path$env$error.message[which]
  opt.path$env$exec.time <- opt.path$env$exec.time[which]
  opt.path$env$extra <- opt.path$env$extra[which]
  opt.path$env$dob <- opt.path$env$dob[which]
  opt.path$env$eol <- opt.path$env$eol[which]
  opt.path$env$path <- opt.path$env$path[which, , drop = FALSE]
}

keep <- data.table::as.data.table(opt.state$opt.path)[, dob == 0]

subsetOptPath(opt.state$opt.path, keep)
opt.state$loop <- 1
opt.state$opt.problem$control$save.file.path <- filepath[[2]]
save(opt.state, file = filepath[[2]])
