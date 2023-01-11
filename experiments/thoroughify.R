#!/usr/bin/env Rscript

filename <- commandArgs(trailingOnly = TRUE)
load(filename)
opt.state$opt.problem$control$infill.opt.focussearch.points <- 100000
opt.state$opt.problem$control$infill.opt.focussearch.maxit <- 4
opt.state$opt.problem$control$infill.opt.restarts <- 10
save(opt.state, file = filename)

