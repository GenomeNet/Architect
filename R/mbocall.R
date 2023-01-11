#' @title Initialize MBO by evaluating the initial design
#'
#' @description
#' `initMboRun()` creates an `mlrMBO` `OptProblem` that optimizes the hyperparameters of the model created by `create_model_genomenet()`.
#'
#"  `evaluateMboRun()` is given the path of the created savefile and runs the optimization.
#'
#' Initial design size is 4% larger than the default, which would be 2 * number
#' of hyperparameters, to account for an approximately 5% failure rate from the
#' start.
#'
#' After the initial design, multiple points are proposed to make use of available parallelization.
#'
#' `evaluateMboRun()` runs in an endless loop; to end optimization, the R process needs to be interrupted.
#'
#' @param save.file.path (`character(1)`)\cr
#'   Path of the MBO save file. Will not be overwritten if it exists, instead
#'   the call will fail.
#' @param maxlen (integer `numeric(1)`)\cr
#'   Sequence length.
#' @param residual_block (`numeric(1)`)\cr
#'   Must be eiter 0 or 1, indicating whether to use residual block or not.
#' @param type (`character(1)`)\cr
#'   Type of network to use. One of `"gap"` or `"recurrent"`.
#' @param fidelity (`data.table`)\cr
#'   `data.table` with columns `walltimehrs` and `iters`, indicating the
#'   value, in hours, to set for `walltime` after `iters`
#'   optimization iterations were performed. The initial design is not counted
#'   here.
#' @param getEpochsDesired (`function`)\cr
#'   A `function` that maps a `numeric(1)`, indicating walltime in number of seconds, to an integer
#'   valued `numeric(1)`, indicating the number of epochs that should approximately be evaluated.
#' @param path (`character(1)`)\cr
#'   Training data path.
#' @param path.val (`character(1)`)\cr
#'   Validation data path.
#' @param labels (`character`)\cr
#'   Class labels.
#' @param plgpath (`character(1)`)\cr
#'   Path of a preloaded generator that is read with [`readPLG()`].
#' @param outputdir (`character(1)`)\cr
#'   Output directory. The tensorboard output is put in the `tensorboard_opt/` subdirectory of this.
#' @param parallel (integer `numeric(1)`)\cr
#'   Parallelization: number of points to propose each iteration for multi-point proposal.
#' @param testrun (`logical(1)`)\cr
#'   Whether to perform a testrun with an objective function that does not use the GPU and has fewer hyperparameters.
#' @return An `OptProblem` created by `mlrMBO:::initOptProblem`
#' @export
initMboRun <- function(save.file.path, maxlen, residual_block, type, fidelity, getEpochsDesired, path, path.val, labels, plgpath, outputdir, parallel = 1, testrun = FALSE) {
# obsolete parameter comment:
## @param plginfo (`data.table`)\cr
##   `data.table` with at least columns `maxlen` and `path`, where `path` points to a preloaded generator with `maxlen` sequence length that is read with [`readPLG()`].
##   The key of this `data.table` must be `maxlen`.

  if (file.exists(save.file.path)) stop(sprintf("Cannot init %s: file exists.", save.file.path))

  WALLTIME.hrs <- min(fidelity$walltimehrs)
  WALLTIME <- WALLTIME.hrs * 3600
  cat(sprintf("initializing %s
MAXLEN: %s
TYPE: %s
RESIDUAL_BLOCK: %s
Initial WALLTIME: %s hrs\n", save.file.path, maxlen, type, residual_block, WALLTIME.hrs))

  # Create the objective function
  if (testrun) {
    obj <- makeSingleObjectiveFunction(name = "genomenet.demo",
      fn = function(x) {
        ret <- rnorm(1, sd = .05) - (log(x$walltime) / 20 - x$batch_norm_momentum)^2  # optimum is at log(walltime)
        attr(ret, "extras") <- list(hostname = Sys.getenv("HOSTNAME"))
        ret
      },
      par.set = pSS(
        dense_layer_units: numeric[4, 11] [[trafo = function(x) floor(2^x)]],
        batch_norm_momentum: numeric[0, 0.99],
        leaky_relu_alpha: numeric[0, 1],
        dense_activation: integer[0, 1] # 0 is relu, 1 is tanh, sigmoid was removed
      ),
      noisy = TRUE,
      has.simple.signature = FALSE,
      minimize = FALSE
    )
  } else {
    obj <- makeGenomeNetObjective(maxlen = maxlen, type = type,
      getEpochsDesired = getEpochsDesired, path = path,
      path.val = path.val, labels = labels, plgpath = plgpath, outdir = outputdir)
  }
  searchspace <- getParamSet(obj)
  attr(obj, "par.set") <- c(searchspace,
    pSS(
        walltime: numeric[log(WALLTIME), log(WALLTIME)] [[trafo = function(x) exp(x)]],
        maxlen: numeric[log(maxlen), log(maxlen)] [[trafo = function(x) exp(x)]],
        residual_block: integer[residual_block, residual_block]
    ))

  # we made the experiment: how many runs from the initial sample fail?
  # 727 runs
  # 37 failed:
  #  - 9 'no non-missing arguments to max'-error (being investigated)
  #  - 28 out of memories (7 segfaults)
  failfactor <- 1.04  # how many more do we launch in the beginning and expect to fail? Empirically 5% fail

  designsize <- ceiling(getParamNr(getParamSet(obj), devectorize = TRUE) * 2 / (parallel - 1e-5) * parallel * failfactor)


  ctrl <- makeMBOControl(
      propose.points = parallel,
      impute.y.fun = function(x, y, opt.path, ...) {
        res = 0
        attr(res, "extras") = list(.train_result = list(), batchsize = 0, paramcount = 0, stepsperepoch = 0, validationsteps = 0)
        res
      },
      suppress.eval.errors = TRUE,
      save.on.disk.at = seq_len(2),
      save.file.path = save.file.path,
      on.surrogate.error = "warn") %>%
    setMBOControlInfill(
      crit = makeMBOInfillCritCB(),
      opt.focussearch.maxit = 10,
      opt.focussearch.points = 2000) %>%
    setMBOControlMultiPoint(method = "cb") %>%
    setMBOControlTermination(iters = 1)


  opt.problem <- mlrMBO:::initOptProblem(
    fun = obj,
    design = generateRandomDesign(n = designsize, par.set = getParamSet(obj)),
    control = ctrl,
    show.info = TRUE,
    learner = NULL,
    more.args = list()
  )

  class(opt.problem$learner$next.learner) <- c("regr.nuggetkm", class(opt.problem$learner$next.learner))

  opt.state <- opt.problem

  save(opt.state, file = save.file.path)  # this is cheating a bit: opt.state is not an opt.problem. we have to treat this accordingly in the 'run' loop
  cat("Done.\n")
  invisible(opt.state)
}

#' @rdname initMboRun
#' @export
evaluateMboRun <- function(save.file.path, fidelity, parallel = 1) {

  cat(sprintf("Running %s\n", save.file.path))
  fidelity[order(walltimehrs), cumiters := cumsum(iters)]

  repeat {
    cat(sprintf("Loading %s\n", save.file.path))
    load(save.file.path)  # loads to the 'opt.state' variable
    if ("OptProblem" %in% class(opt.state)) {
      cat(sprintf("Evaluating initial design %s\n", save.file.path))
      opt.state <- mlrMBO:::mboTemplate.OptProblem(opt.state)
      save(opt.state, file = save.file.path)
      next
    }
    designsize <- nrow(opt.state$opt.problem$design)
    pointsevald <- ParamHelpers::getOptPathLength(opt.state$opt.path)
    if (pointsevald < designsize) {
      stop("This should not happen: evaluated points exist, but fewer than the initial design")
    }
    pointsevald.relative <- pointsevald - designsize
    # reset terminator to break at next fidelity jump
    nextbreak <- fidelity[J(pointsevald.relative + 1), x.cumiters, on = "cumiters", roll = -Inf]
    iters.finished <- max(ParamHelpers::getOptPathDOB(opt.state$opt.path))

    # note nextbreak.absolute is in terms of 'iterations', which start at 0 (design) and count in steps of `parallel`
    nextbreak.relative <- min(nextbreak - pointsevald.relative, 100000)
    iters.remaining <- ceiling(nextbreak.relative / parallel)
    iters.absolute <- iters.remaining + iters.finished
    opt.state <- resetOptStateItersTerminator(opt.state, iters = iters.absolute, save.on.disk.at = seq_len(iters.absolute + 1))

    # change walltime
    WALLTIME.hrs <- fidelity[J(pointsevald.relative + 1), walltimehrs, on = "cumiters", roll = -Inf]
    WALLTIME <- WALLTIME.hrs * 3600
    ps.in <- ParamHelpers::getParamSet(opt.state$opt.problem$fun)
    ps.in$pars$walltime <- pSS(walltime: numeric[log(WALLTIME), log(WALLTIME)] [[trafo = function(x) exp(x)]])$pars$walltime
    opt.state <- resetOptStateParamSet(opt.state, ps.in)

    cat(sprintf("Running %s evals (%s iters) of %s hr fidelity.\n", iters.remaining * parallel, iters.remaining, WALLTIME.hrs))
    mboContinue(opt.state)
    cat(sprintf("Fidelity step %s hrs finished.\n", WALLTIME.hrs))
  }
}
