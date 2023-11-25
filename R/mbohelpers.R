# helper function for multi-fidelity in mlrMBO

#' @title Set MBO Control Object to Terminate after a Certain Number of Iterations
#'
#' @description
#' Resets the termination state of a given `opt.state` to terminate after a certain number of iterations.
#'
#' @param opt.state (`OptState`)\cr
#'   The `OptState` to reset.
#' @param iters (integer `numeric(1)`)\cr
#'   The (total) number of iterations to run. Note that this is not the number of iterations to run after the reset, but
#'   the total number of iterations after which to terminate.
#' @param save.on.disk.at (integer `numeric`)\cr
#'   The number of iterations after which to save the `OptState` to disk. Can be a vector of integers, in which case the
#'   `OptState` is saved to disk after each of the given number of iterations.
#' @return the modified `opt.state`.
#' @export
resetOptStateItersTerminator <- function(opt.state, iters, save.on.disk.at) {
  assertClass(opt.state, "OptState")
  assertInt(iters)
  assertIntegerish(save.on.disk.at)
  opt.state$opt.problem$control <- setMBOControlTermination(opt.state$opt.problem$control, iters = iters)
  opt.state$opt.problem$control$save.on.disk.at <- save.on.disk.at
  opt.state$progress <- 0
  opt.state$state <- "iter"
  opt.state
}

#' @title Enlarges the Search Space of an Optimization State
#'
#' @description
#' Changes the search space to the given `ParamSet` and adds the required columns to the evaluation `OptPath`.
#' To fill in values, the 'default' value, or `NA`, is used if the `defaults` argument is not given.
#'
#' Removing search space components is not supported.
#'
#' @param opt.state (`OptState`)\cr
#'   The `OptState` to modify.
#' @param par.set (`ParamSet`)\cr
#'   The new `ParamSet` to use.
#' @param defaults (list)\cr
#'   A list of default values to use for the new parameters. If not given, each parameter's default (or `NA`, if not
#'   present) is used.
#' @return the modified `opt.state`.
#' @export
resetOptStateParamSet <- function(opt.state, par.set, defaults = getDefaults(par.set)) {
  assertClass(opt.state, "OptState")
  assertClass(par.set, "ParamSet")
  assertList(defaults)
  if (length(defaults)) assertNames(names(defaults), subset.of = getParamIds(par.set))

  op <- opt.state$opt.path
  ## virgin.op <- makeOptPathDF(par.set = par.set, y.names = op$y.names, minimize = op$minimize,
  ##   add.transformed.x = op$add.transformed.x, include.error.message = !is.null(op$env$error.message),
  ##   include.exec.time = !is.null(op$env$exec.time), include.extra = !is.null(op$env$extra))
  virgin.op <- makeOptPathDF(par.set = par.set, y.names = op$y.names, minimize = op$minimize)

  addOptPathEl(virgin.op,
    x = insert(lapply(par.set$pars, ParamHelpers:::getParamNA), defaults),
    y = NA, check.feasible = FALSE)
  assertSubset(colnames(op$env$path), colnames(virgin.op$env$path))  # error if params are subtracted
  op$env$path <- insert(virgin.op$env$path[rep(1, nrow(op$env$path)), ], op$env$path)
  opt.state$opt.path$par.set <- par.set
  attributes(opt.state$opt.problem$fun)$par.set <- par.set
  opt.state
}

#' @title Changes the Objective of an Optimization State
#'
#' @description
#' Modifies the objective of a given `OptState`, taking care to update the `OptState`'s `ParamSet` using
#' [resetOptStateParamSet()].
#'
#' @param opt.state (`OptState`)\cr
#'   The `OptState` to modify.
#' @param fun (`smoof_function`)\cr
#'   The new objective function.
#' @param defaults (list)\cr
#'   A list of default values to use for the new parameters. If not given, each parameter's default (or `NA`, if not
#'   present) is used.
#' @return the modified `opt.state`.
#' @export
resetOptStateObjective <- function(opt.state, fun, defaults = getDefaults(getParamSet(fun))) {
  assertClass(opt.state, "OptState")
  assertClass(fun, "smoof_function")
  opt.state <- resetOptStateParamSet(opt.state, getParamSet(fun), defaults = defaults)
  opt.state$opt.problem$fun <- fun
  opt.state
}
