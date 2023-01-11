

# behaves like a generator, generates elements of l with each call, cycling through to the beginning at the end.
# batchmultiplier: increase batchsize by this factor by abind()-ing multiple elements
listGenerator <- function(l, batchsize, numbatches, reverseComplementEncoding = FALSE) {
  force(l)
  stopifnot(batchsize %% length(l) == 0)
  stopifnot(batchsize %% 1 == 0 && batchsize > 0)
  stopifnot(numbatches %% 1 == 0 && numbatches > 0)
  stopifnot(numbatches * batchsize <= length(l) * NROW(l[[1]]))
  stopifnot(isTRUE(reverseComplementEncoding) || isFALSE(reverseComplementEncoding))

  i <- 0

  batchstep <- batchsize / length(l)

  function() {
    i <<- i + 1
    if (i > numbatches) {
      cat("Resetting generator.\n")
      i <<- 1
    }
    userows = seq(to = i * batchstep, length.out = batchstep)
    X <- do.call(abind::abind, c(
      list(along = 1),
      lapply(l, function(lmat) {
        lmat[userows, , , drop = FALSE]
      })
    ))
    if (reverseComplementEncoding) {
      X <- list(X, X[, rev(seq_len(NCOL(X))), 4:1, drop = FALSE])
    }
    list(
      X = X,
      Y = sapply(seq_along(l), function(yval) {
        matrix(as.numeric(seq_along(l) == yval),
          nrow = batchstep, ncol = length(l), byrow = TRUE
        )
      })
    )
  }
}

# helper function that calls the generator `loadstep` times and formats its data.
getPlgData <- function(gen, loadsteps) {
  rawin <- replicate(loadsteps, gen(), simplify = FALSE)
  indices <- names(rawin[[1]])
  rawin <- sapply(indices, function(idx) {
    do.call(abind::abind, c(
      list(along = 1),
      lapply(rawin, `[[`, idx)
    ))
  }, simplify = FALSE)

  yvals <- ncol(rawin$Y)
  lapply(seq_len(yvals), function(ycol) {
    rawin$X[rawin$Y[, ycol] == 1, , , drop = FALSE]
  })
}

#' @title Create a preloaded Generator Directly
#'
#' @description
#' Preloaded generators read inputs through a [`deepG`][deepG::deepG-package]
#' generator and store the resulting data (on disk, or in memory). They can
#' then be used to train models, or as validation data, without needing to use
#' (slow) `deepG` generators. This has an added benefit for validation
#' generators, which become deterministic if `numbatches` equals the number of
#' validation batches.
#'
#' `preloadPLG()` calls the generator `gen` multiple times and saves the result
#' as `file`. This file is then used by `readPLG()`, which creates a generator.
#'
#' `preloadPLG()` creates a generator directly from the given generator `gen`;
#' this is equivalent to calling `preloadPLG()`, followed by `readPLG()` on the
#' same file, but keeps the data in memory instead of saving it to a file.
#'
#' @param file (`character(1)`)\cr
#'   Path of `.rds`-file to use to store data.
#' @param gen (`function`)\cr
#'   [`deepG`][deepG::deepG-package] generator from which to extract samples.
#' @param loadsteps (integer `numeric(1)`)\cr
#'   Number of times to call `gen` to create samples.
#' @param batchsize (integer `numeric(1)`)\cr
#'   Batch size of the resulting generator. This can be different from the
#'   batch size of `gen`.
#' @param numbatches (integer `numeric(1)`)\cr
#'   Number of batches to return before which to start from the beginning.
#'   `numbatches * batchsize` must be smaller or equal to the batchsize of
#'   `gen` times `loadsteps`, otherwise not sufficient data is present.
#' @param reverseComplementEncoding (`logical(1)`)\cr
#'   Whether to create reverse complement encoded batches.
#'   Reverse complement encoding should be switched off for `gen`. Instead,
#'   `reverseComplementEncoding` of `preloadedGenerator()` / `readPLG()`
#'   should be used.
#' @return A generator `function` or `NULL` (in the case of `preloadPLG()`).
#' @export
preloadedGenerator <- function(gen, loadsteps, batchsize, numbatches, reverseComplementEncoding = FALSE) {
  listGenerator(getPlgData(gen, loadsteps), batchsize = batchsize, numbatches = numbatches,
    reverseComplementEncoding = reverseComplementEncoding)
}

#' @rdname preloadedGenerator
#' @export
preloadPLG <- function(gen, loadsteps, file) {
  saveRDS(getPlgData(gen, loadsteps), file)
  invisible(NULL)
}

#' @rdname preloadedGenerator
#' @export
readPLG <- function(file, batchsize, numbatches, reverseComplementEncoding) {
  listGenerator(readRDS(file), batchsize = batchsize, numbatches = numbatches,
    reverseComplementEncoding = reverseComplementEncoding)
}
