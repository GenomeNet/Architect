
# Optimization Settings:
# Which combinations of hyperparameters to optimize for, and which fidelity to use.

# All combinations of the following
MAXLEN <- 250  # c(150, 10000)
TYPE <- c("recurrent", "gap")
RESIDUAL_BLOCK <- c(0, 1)

runs <- data.table::CJ(maxlen = MAXLEN, type = TYPE, residual_block = RESIDUAL_BLOCK)



## typical experiment sizes so far, lets say:
# ~ 40 to 50 initial design
# ~ 330 small runs
# ~ 230 medium runs
# ~ 90 big runs
## we change this to
# ~ 44 to 53 initial design (accounting for possible failures)
# ~ 150 small runs
# ~ 100 medium runs
# ~ all remaining: big runs

fidelity <- data.table::rbindlist(list(
    list(walltimehrs = 2, iters = 150),
    list(walltimehrs = 6, iters = 100),
    list(walltimehrs = 20, iters = Inf)
))
