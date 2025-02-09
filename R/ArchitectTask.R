#' @title ArchitectTask Constructor
#'
#' @description
#' The `ArchitectTask` class describes the overall optimization progress.
ArchitectTask <- function(problem.descriptor) {

}



#' @title SearchSpaceDescriptor Constructor
#'
#' @description
#' The `SearchSpaceDescriptor` class describes the search space of the optimization problem.
#' @param searchspace ([`ParamSet`])\cr
#'   The [`ParamSet`] to sample from.
#' @param trafo ([`function`])\cr
#'   A transformation function to be applied to the sampled values.
#'   Must map a named `list` to a named `list`.
#' @param design.fixed (named `list` of `character(1)` or `numeric(1)`)\cr
#'   A named list of fixed arguments to be passed to the model function.
#'   It is added *before* the `trafo` transformation and will be recorded in the
#'   optimization archive.\cr
#'   If you are running different experiments that you plan to combine, you should
#'   use this argument so that the experiments are distinguishable in the combined
#'   archive.\cr
#'   In the default setup, where modelfun is [`create_model_genomenet`],
#'   it is recommended to set this to a list which, at least, has the entry `maxlen`, set to the input sequence length.\cr
#'   Must not contain an entry named "`num_targets`", or entries clashing with components in `searchspace`.
#' @param args.fixed (named `list`)\cr
#'   A named list of fixed arguments to be passed to the model function.
#'   It is added *after* the `trafo` transformation and is not recorded in the
#'   optimization archive. Use these arguments to fix things that are constant
#'   across your experiments (e.g. setting up classification vs. regression in the `modelfun`).\cr
#'   Must contain an entry named "`num_targets`".
#' @param modelfun ([`function`])\cr
#'   A function taking arguments sampled from `searchspace`, transformed by `trafo` and augmented by `args.fixed`,
#'   returning a keras model.
#'   Besides the arguments sampled from `searchspace`, the function must accept the named argument `num_targets` (number of outputs).
#' @param verify.searchspace (`logical(1)`)\cr
#'   Whether to verify the search space by sampling a few points, transforming them, and checking if the resulting values pass basic checks.
#'   Results are checked for being a named list, and for having the right names.
#' @param verify.model (`logical(1)`)\cr
#'   Whether to verify the model construction by checking that the model can be created with a few points sampled from `searchspace`,
#'   transformed by `trafo` and augmented by `args.fixed` and `verify.model.with`.
#'   This happens in a separate R process.
#'   Must be `FALSE` when `verify.searchspace` is `FALSE`.
#' @param verify.model.with (named `list` with entry `num_targets`)\cr
#'   Additional arguments to be passed to the model function when verifying the model construction.
#'   This is ignored if `verify.searchspace` is `FALSE`.
#'   Entries besides `num_targets` are currently ignored, but future versions may use them.
#' @return a [`SearchSpaceDescriptor`].
#' @export
SearchSpaceDescriptor <- function(searchspace, trafo = identity,
    design.fixed = list(),
    args.fixed = list(metrics = "acc", loss = "categorical_crossentropy"), modelfun = create_model_genomenet,
    verify.searchspace = TRUE, verify.model = FALSE, verify.model.with = list(num_targets = 2)) {

  assertClass(searchspace, "ParamSet")
  assertFunction(trafo)
  assertList(design.fixed, names = "unique")
  assertNames(names(design.fixed), disjunct.from = c("num_targets", getParamIds(searchspace))
  for (design.fixed.element in design.fixed) {
    assert(
      checkString(design.fixed.element), checkNumeric(design.fixed.element),
      .var.name = "an element of design.fixed"
    )
  }
  assertList(args.fixed, names = "unique")
  assertNames(names(args.fixed), disjunct.from = c("num_targets", names(design.fixed)))
  assertFunction(modelfun)
  assertFlag(verify.searchspace)
  assertFlag(verify.model)
  if (!verify.searchspace && verify.model) stop("verify.model must be FALSE when verify.searchspace is FALSE")
  if (verify.model) {
    assertList(verify.model.with, names = "unique")
    assertNames(names(verify.model.with), must.include = "num_targets")
    assertInt(verify.model.with$maxlen, tol = 0)
    assertInt(verify.model.with$num_targets, tol = 0)
  }
  if (verify.searchspace) {
    xorig <- sampleValues(searchspace, 5, trafo = TRUE)
    xs <- lapply(xorig, function(x) {
      # TODO turn this into its own function, also use it for the actual search
      x[names(design.fixed)] <- design.fixed
      x <- trafo(x)
      assertList(x, names = "unique", .var.name = "trafo(<value sampled from searchspace>)")
      assertNames(names(x), disjunct.from = c("num_targets", names(args.fixed)), .var.name = "trafo(<value sampled from searchspace>)")
      x[names(args.fixed)] <- args.fixed
      if (verify.model) {
        x[names(verify.model.with)] <- verify.model.with
      }
      x
    })
    if (verify.model) {
      # check if the search space is valid
      on.exit(session$close())
      session <- callr::r_session$new(wait_timeout = 120000)
      while (session$get_state() == "starting") Sys.sleep(1)
      result <- session$run(args = list(xs = xs, modelfun = modelfun), function(xs, modelfun) {
        Sys.setenv("CUDA_VISIBLE_DEVICES" = "")
        options(warning.length = 8170)
        library("checkmate")
        library("magrittr")
        for (i in seq_along(xs)) {
          worked <- FALSE
          message <- tryCatch(withCallingHandlers({
              x <- xs[[i]]
              model <- do.call(function(...) modelfun(...), x)
              worked <- TRUE
              NULL
            }, error = function(e) {
              text <- unlist(.traceback(4))
              cutoffs <- grep("^modelfun\\(\\.\\.\\.\\)$", text)
              if (length(cutoffs)) {
                text <- head(text, max(cutoffs))
              }
              stop(
                sprintf("Model construction failed with error:\n%s\nTraceback:\n  %s\n",
                  e$message,
                  paste(text, collapse = "\n  ")
                ),
                call. = FALSE
              )
            }),
          error = function(e) {
            e$message
          })
          if (!worked) return(list(i = i, message = message))
        }
        return(NULL)
      })
      if (!is.null(result)) {
        on.exit(options(oldopts))
        oldopts <- options(warning.length = 8170)
        if (!testNames(names(result), permutation.of = c("i", "message"))) stop("Unexpected result from model construction verification")
        repr <- paste(capture.output(print(as.data.table(xorig[[result$i]]))), collapse = "\n")
        stop(sprintf("Model construction failed for sampled search space point:\n%s\nError:\n%s", repr, result$message))
      }
    }
  }
  structure(list(
      searchspace = searchspace,
      trafo = trafo,
      args.fixed = args.fixed,
      modelfun = modelfun
  ), class = "SearchSpaceDescriptor")
}

#' @title Default GenomeNet Architect Search Space
#'
#' @description
#' Constructs the default search space for GenomeNet Architect.
#'
#' @param maxlen (`integer(1)`)\cr
#'   The sequence length under consideration.
#'   This influences the upper bounds of some of the search intervals.
#' @param type (`character(1)`)\cr
#'   One of `"gap"` or `"recurrent`.
#' @param residual_block (`logical(1)`)\cr
#'   Whether to use residual blocks.
#' @return a [`SearchSpaceDescriptor`].
#' @export
makeDefaultSearchSpaceDescriptor <- function(maxlen, type, residual_block, ...) {
  assertCount(maxlen)
  assertChoice(type, c("gap", "recurrent"))
  assertFlag(residual_block)
  SearchSpaceDescriptor(searchspace = getSearchspaceDefault(maxlen, type),
    trafo = trafoSearchSpaceDefault, design.fixed = list(maxlen = maxlen), ...)
}