#' @title ProblemDescriptor Constructor
#'
#' @description
#' A `ProblemDescriptor` object identifies the machine learning task that is to be solved.
#' It describes the given data and how it is loaded, as well as whether the task is a classification or regression task.
#'
#' It is not necessary that paths indicated in the `ProblemDescriptor` exist at the time of construction.
#' @param target.source.type (`character(1)`)\cr
#'   The source of the labels / regression target. One of `"header"`, `"folder"`, or `"csv"`.
#'   When this is `"header"`, classification labels are read from FASTA headers.\cr
#'   When this is `"folder"`, labels correspond to folder names: For each class, a folder with the given class
#'   must exist inside `path.train`.\cr
#'   When `target.source` is `"csv"`, the labels are read from a CSV file. The CSV file must have a column named `file`,
#'   as well as other entries that are used as output targets.
#' @param path.train (`character(1)`)\cr
#'   (base-)path to the training data.
#'   When `target.source` is `"folder"`, this must contain subfolders with the class names, in which corresponding FASTA files are located.
#'   Otherwise, `path.train` should contain FASTA files (or the folder `subfolder.train`) directly.
#'   See the section "Determining File Locations" for more information.
#' @param path.val (`character(1)`)\cr
#'   (base-)path to the validation data -- same function as `path.train`.
#'   Defaults to the same value as `path.train`.
#'   Note that `path.val` may only be equal to `path.train` when at least one of `subfolder.train` and `subfolder.val` is not `NULL`.
#' @param subfolder.train (`character(1)`)\cr
#'   Subfolder of `path.train` that contains the FASTA files.
#'   Defaults to `NULL`, in which case the FASTA files are directly in `path.train`.
#' @param subfolder.val (`character(1)`)\cr
#'   Subfolder of `path.val` that contains the FASTA files.
#'   Defaults to `NULL`, in which case the FASTA files are directly in `path.val`.
#' @param sample.description (`SampleDescription`)\cr
#'   A `SampleDescription` object describing how samples are loaded from FASTA-files for training.
#' @param sample.description.val (`SampleDescription`)\cr
#'   A `SampleDescription` object describing how samples are loaded from FASTA-files for validation.
#'   Defaults to the same value as `sample.description`.\cr
#'   When this is given, it must have the same `maxlen` and `vocabulary` as `sample.description`.
#' @param target.csv (`character(1)` | `NULL`)\cr
#'   Path to the CSV file containing the labels. Must be `NULL` when `target.source` is not `"csv"`.
#'   Note that this path is relative to the working directory, not to `path.train` or `path.val`.
#' @param target.labels (`character` | `NULL`)\cr
#'   The labels of the classes (or singular regression target).
#'   Can be `NULL` when `target.source` is not `"header"`, in which case the labels are inferred:\cr
#'   When `target.source` is `"folder"`, the labels are the names of the subfolders in `path.train`.\cr
#'   When `target.source` is `"csv"`, the labels are the names of the columns in `target.csv` with the exception of the
#'   `file` column.
#' @param log.dir (`character(1)` | `NULL`)\cr
#'   Path to the directory where tensorboard logs are written.
#'   Can be `NULL` (default) to disable tensorboard logging.
#' @param runname.prefix (`character(1)`)\cr
#'   Prefix for the run name. Ignored when `log.dir` is `NULL`.
#' @param allow.validate.on.training (`logical(1)`)\cr
#'   Whether to allow validation on the training set, which is typically not a good idea.
#'   When this is `FALSE`, then an error is thrown when `path.val` / `subfolder.val` are equal to `path.train` / `subfolder.train`, respectively.
#'   Set this flag to `TRUE` to override this behavior.
#'   Note that setting validation data to training data is not the same as using the training error as optimizatoin objective:
#'   Trianing error is accumulated over an epoch, whereas validation error is calculated at the end of an epoch.
#'   Even if validation data is set to training data, validation error is still calculated on a set of samples at the end of each epoch.
#'
#' @return `ProblemDescriptor` object. This is a list with elements corresponding to the function arguments.
#'
#' @section Determining File Locations:
#'
#' When the `target.source.type` is `"header"` or `"csv"`, the files for all targets are assumed to be split into two folders,
#' one for training and one for validation. The files for training need to be found in `file.path(path.train, subfolder.train)`,
#' and the files for validation need to be found in `file.path(path.val, subfolder.val)`. If `subfolder.train` or `subfolder.val`
#' are `NULL`, the files are assumed to be directly in `path.train` or `path.val`, respectively. Thus, if training and validation
#' data are in folders `/data/fasta/train` and `/data/fasta/val`, the `ProblemDescriptor` could be constructed with either
#' ````
#' path.train = "/data/fasta", subfolder.train = "train", subfolder.val = "val"
#' ```
#' (taking the default value of `path.val = path.train`), or with
#' ```
#' path.train = "/data/fasta/train", path.val = "/data/fasta/val"
#' ```
#'
#' When the `target.source.type` is `"folder"`, the folders in `path.train` and `path.val` need to correspond to target names.
#' For example, if the targets are `c("virus", "bacteria")` and the training data is in `/data/fasta/train/virus` and
#' `/data/fasta/train/bacteria`, respectively, with validation data in corresponding folders in `/data/fasta/validation/{virus,bacteria}`,
#' the `ProblemDescriptor` could be constructed with
#' ```
#' path.train = "/data/fasta/train", path.validation = "/data/fasta/validation"
#' ```
#' taking the default value of `subfolder.train = NULL` and `subfolder.val = NULL`.
#' If there are *only* the `virus` and `bacteria` folders inside `/data/fasta/train`, then the `target.labels` argument can be omitted.
#'
#' Suppose, however, the distinction between training and validation files were made at a higher level
#' than the distinction between classes. For example, training data in `"/data/fasta/virus/train"` and `"/data/fasta/bacteria/train"`
#' and validation data in `"data/fasta/virus/validation"` and `"/data/fasta/bacteria/validation"`. Then the `ProblemDescriptor`
#' could be constructed with
#' ```
#' path.train = "/data/fasta", subfolder.train = "train", subfolder.val = "validation"
#' ```
#' taking the default value of `path.val = path.train`.
#'
#' Notice how, when the `target.source.type` is `"header"` or `"csv"`, the having `path.val = "/a/b/c"` and `subfolder.val = "d"`
#' is equivalent to having `path.val = "/a/b/c/d"` and `subfolder.val = NULL` or `path.val = "/a/b/"` and `subfolder.val = "c/d"`.
#' This is *not* the case when `target.source.type` is `"folder"`, since the folders below `path.val` must correspond to
#' target names.
#'
#' @export
ProblemDescriptor <- function(target.source.type, path.train, path.val = path.train, subfolder.train = NULL, subfolder.val = NULL, sample.description,
    sample.description.val = sample.description,
    target.csv = NULL, target.labels = NULL, log.dir = NULL, runname.prefix = NULL, allow.validate.on.training = FALSE) {
  assertChoice(target.source.type, c("header", "folder", "csv"))
  assertString(path.train, min.chars = 1)
  assertString(path.val, min.chars = 1)
  assertString(subfolder.train, null.ok = TRUE, min.chars = 1)
  assertString(subfolder.val, null.ok = TRUE, min.chars = 1)
  assertClass(sample.description, "SampleDescription")
  assertClass(sample.description.val, "SampleDescription")

  if (target.source.type == "csv") {
    assertString(target.csv)
  } else {
    assertNull(target.csv)
  }

  if (!allow.validate.on.training && path.train == path.val && identical(subfolder.train, subfolder.val)) {
    stop("path.train and path.val are equal and subfolder.train and subfolder.val are equal. This is typically not a good idea; set allow.validate.on.training = TRUE to override.")
  }

  assertCharacter(target.labels, min.len = 1, any.missing = FALSE, null.ok = TRUE, unique = TRUE)
  if (target.source.type == "header" && is.null(target.labels)) {
    stop("target.labels must be given when target.source.type is 'header'")
  }
  if (target.source.type == "csv" && "file" %in% target.labels) {
    stop("target.labels must not contain 'file' when target.source.type is 'csv'")
  }
  if (sample.description$maxlen != sample.description.val$maxlen) {
    stop("sample.description and sample.description.val must have the same `$maxlen` entry.")
  }
  if (!identical(sample.description$vocabulary, sample.description.val$vocabulary)) {
    stop("sample.description and sample.description.val must have the same `$vocabulary` entry.")
  }
  if (!identical(sample.description$targets, sample.description.val$targets)) {
    if (is.na(sample.description$targets) && !is.na(sample.description.val$targets)) {
      sample.description.val$targets <- sample.description$targets
    } else if (!is.na(sample.description$targets) && is.na(sample.description.val$targets)) {
      sample.description$targets <- sample.description.val$targets
    } else {
      stop("If the number of targets is given in both sample.description and sample.description.val, it must be the same.")
    }
  }
  if (!is.null(target.labels) && !is.na(sample.description$targets) &&
      length(target.labels) != sample.description$targets) {
    stop(sprintf("sample.description(.val) is specified for %d targets, but %d target.labels were given.",
        sample.description$targets, length(target.labels)))
  }

  if (target.source.type != "folder" && !is.na(sample.description$targets) && sample.description$targets != 1) {
    stop(sprintf("When sample.description(.val) specifies parameters for multiple targets, target.source.must be 'folder'. Instead, it is '%s'.",
        target.source.type))
  }

  assertString(log.dir, null.ok = TRUE)
  if (!is.null(log.dir)) {
    assertString(runname.prefix)
  } else {
    runname.prefix <- NULL
  }
  structure(list(
    target.source.type = target.source.type,
    path.train = path.train,
    path.val = path.val,
    subfolder.train = subfolder.train,
    subfolder.val = subfolder.val,
    sample.description = sample.description,
    sample.description.val = sample.description.val,
    target.csv = target.csv,
    target.labels = target.labels,
    log.dir = log.dir,
    runname.prefix = runname.prefix
  ), class = "ProblemDescriptor")
}

#' @export
print.ProblemDescriptor <- function(x, ...) {

  cat("ProblemDescriptor\n")
  for (item in names(x)) {
    printstring <- x[[item]] %??% "NULL"
    if (item %in% c("sample.description", "sample.description.val")) {
      printstring <- class(x[[item]])[[1]]
    }
    if (length(printstring) > 1) {
      printstring <- paste0("[", paste(printstring, collapse = ","), "]")
    }
    cat(sprintf("  %s: %s\n", item, printstring))
  }
  invisible(x)
}

# check paths in ProblemDescriptor and fill in missing information
reifyProblemDescriptor <- function(problem.descriptor) {
  assertClass(problem.descriptor, "ProblemDescriptor")

  if (problem.descriptor$target.source.type != "folder") {
    problem.descriptor$path.train <- file.path2(problem.descriptor$path.train, problem.descriptor$subfolder.train)
    problem.descriptor$path.val <- file.path2(problem.descriptor$path.val, problem.descriptor$subfolder.val)
  }

  assertDirectoryExists(problem.descriptor$path.train)
  assertDirectoryExists(problem.descriptor$path.val)

  if (!is.null(problem.descriptor$log.dir)) {
    assertDirectoryExists(problem.descriptor$log.dir, access = "w")
  }

  switch(problem.descriptor$target.source.type,
    header = {
      # nothing else to check
    },
    folder = {
      labels.possible <- list.dirs(problem.descriptor$path.train, full.names = FALSE, recursive = FALSE)
      labels.possible2 <- list.dirs(problem.descriptor$path.val, full.names = FALSE, recursive = FALSE)
      if (is.null(problem.descriptor$target.labels)) {
        # labels were not given. In this case the folders in train and val must be the same, otherwise we assume this
        # is a mistake.
        if (!testSetEqual(labels.possible, labels.possible2)) {
          stop("When target.source.type is 'folders' and target.labels is not given, then subfolders in path.train and path.val must be the same.")
        }
        problem.descriptor$target.labels <- labels.possible
      } else {
        # labels were given. In this case, we only make sure they exist as subfolders in train and val.
        all.found <- intersect(labels.possible, labels.possible2)
        if (!testSubset(problem.descriptor$target.labels, all.found)) {
          stop("target.source.type is 'folders', but not all target.labels exist as subfolders in path.train.\n",
            sprintf("Missing labels: %s", paste(setdiff(problem.descriptor$target.labels, all.found), collapse = ", "))
          )
        }
      }
      # if subfolder.train / subfolder.val are given, we check that the required subfolder exists for each target
      if (!is.null(problem.descriptor$subfolder.train)) {
        for (tl in problem.descriptor$target.labels) {
          assertDirectoryExists(file.path(problem.descriptor$path.train, tl, problem.descriptor$subfolder.train))
        }
      }
      if (!is.null(problem.descriptor$subfolder.val)) {
        for (tl in problem.descriptor$target.labels) {
          assertDirectoryExists(file.path(problem.descriptor$path.val, tl, problem.descriptor$subfolder.val))
        }
      }
    },
    csv = {
      assertFileExists(problem.descriptor$target.csv)
      prototbl <- fread(problem.descriptor$target.csv, nrows = 0)
      labels <- colnames(prototbl)
      if (!"file" %in% labels) {
        stop("target.source.type is 'csv', but target.csv does not contain a column named 'file'.")
      }
      if (is.null(problem.descriptor$target.labels)) {
        problem.descriptor$target.labels <- setdiff(labels, "file")
      } else {
        if (!testSubset(problem.descriptor$target.labels, labels)) {
          stop("target.source.type is 'csv', but not all target.labels exist as columns in target.csv.\n",
            sprintf("Missing labels: %s", paste(setdiff(problem.descriptor$target.labels, labels), collapse = ", "))
          )
        }
      }
      if (!all(vapply(prototbl[, problem.descriptor$target.labels, with = FALSE], is.numeric, logical(1)))) {
        stop("target.source.type is 'csv', but not all target.labels are numeric columns in target.csv.")
      }
    }
  )

  if (!identical(problem.descriptor$sample.description$targets, problem.descriptor$sample.description.val$targets)) {
    stop("sample.description and sample.description.val must have the same `$targets` entry.")
  }

  # fill in missing information about number of targets in sample.description
  if (is.na(problem.descriptor$sample.description$targets)) {
    problem.descriptor$sample.description$targets <- length(problem.descriptor$target.labels)
    problem.descriptor$sample.description.val$targets <- length(problem.descriptor$target.labels)
  } else {
    if (length(problem.descriptor$target.labels) != problem.descriptor$sample.description$targets) {
      stop(sprintf("sample.description(.val) is specified for %d targets, but %d target.labels were given.",
          problem.descriptor$sample.description$targets, length(problem.descriptor$target.labels)))
    }
  }
  problem.descriptor
}

#' @title SampleDescription Constructor
#'
#' @description
#' A `SampleDescription` object describes how samples are loaded from FASTA-files.
#'
#' Many parameters can have length either 1 or, if different folders are used for different targets, a length equal to
#' the number of output targets.
#'
#' @param maxlen (integer `numeric(1)`)\cr
#'   Sequence length.
#' @param vocabulary (`character`)\cr
#'   The vocabulary of the sequences. Defaults to `c("a", "c", "g", "t")`.
#' @param stepsize (integer `numeric(1)` | `numeric(targets)`)\cr
#'   Step size for sliding window. Defaults to `maxlen`.
#' @param file.order (`character(1)` | `character(targets)`)\cr
#'   What order to read files in. One of `"random.uniform"`, `"random.filesize"`, or `"sequential"`.
#'   `"random.uniform"` corresponds to `shuffle_file_order` being `TRUE` in `deepG`.
#'   `"random.filesize"` corresponds to `sample_by_file_size` being `TRUE` in `deepG`.
#'   `"sequential"` corresponds to both `shuffle_file_order` and `sample_by_file_size` being `FALSE` in `deepG` and iterates through
#'   files in alphabetical order.
#'   Default `"random.uniform"`.
#' @param shuffle.entries (`logical(1)` | `logical(targets)`)\cr
#'   Whether to shuffle the order of entries within each file.
#'   Default `TRUE`.
#'   Corresponds to `shuffle_input` in `deepG`.
#' @param proportion.per.seq (`numeric(1)` | `numeric(targets)`)\cr
#'   Proportion of a given sequence to use.
#'   Defaults to 1, i.e. use the whole sequence.
#' @param max.samples.per.file (integer `numeric(1)` | `numeric(targets)`)\cr
#'   Maximum number of samples to read from each file consecutively.
#'   This is not the total number of samples read from a file, but the number of samples read before moving to the next file.
#'   If the file is selected again, the count is reset.
#'   Default `Inf`: no limit.
#' @param reverse.complements (`logical(1)` | `logical(targets)`)\cr
#'   Whether to randomly reverse-complement sequences. Default `TRUE`.
#' @param ambiguous.nuc (`character(1)` | `character(targets)`)\cr
#'   How to handle ambiguous nucleotides. One of `"zero"`, `"equal"`, `"discard"`, or `"empirical"`.
#'   `"zero"` replaces ambiguous nucleotides with zeros.\cr
#'   `"equal"` replaces ambiguous nucleotides with equal probabilities for all nucleotides.\cr
#'   `"discard"` discards sequences with ambiguous nucleotides.\cr
#'   `"empirical"` replaces ambiguous nucleotides with the empirical nucleotide distribution of the given file.
#'   Default `"zero"`.
#' @param skip.amb.nuc (numeric `numeric(1)` | `numeric(targets)`)\cr
#'   Fraction of ambiguous nucleotides to accept in a sequence.
#'   Sequences with more ambiguous nucleotides are discarded regardless of the value of `ambiguous.nuc`.
#'   Default 1.
#'
#' @return `SampleDescription` object. This is a list with elements corresponding to the function arguments, and an additional
#' element `targets` that indicates the number of targets. This is typically `NA` when the parameters
#' all have length 1 (i.e. an arbitrary number of targets is supported). If any parameter has a length different from 1, then
#' `targets` reflects that length.
#'
#' @export
SampleDescription <- function(maxlen, vocabulary = c("a", "c", "g", "t"), stepsize = maxlen, file.order = "random.uniform", shuffle.entries = TRUE,
    proportion.per.seq = 1, max.samples.per.file = Inf, reverse.complements = TRUE, ambiguous.nuc = "zero",
    skip.amb.nuc = 1) {
  assertCount(maxlen, tol = 0)
  assertCharacter(vocabulary, min.len = 1, any.missing = FALSE)

  assertIntegerish(stepsize, any.missing = FALSE, min.len = 1, tol = 0, lower = 1)
  assertCharacter(file.order, any.missing = FALSE, min.len = 1)
  assertSubset(file.order, c("random.uniform", "random.filesize", "sequential"))
  assertLogical(shuffle.entries, any.missing = FALSE, min.len = 1)
  assertNumeric(proportion.per.seq, any.missing = FALSE, min.len = 1, lower = 0, upper = 1)
  assertNumeric(max.samples.per.file, any.missing = FALSE, min.len = 1, lower = 1)
  assertIntegerish(max.samples.per.file[is.finite(max.samples.per.file)], tol = 0)
  assertLogical(reverse.complements, any.missing = FALSE, min.len = 1)
  assertCharacter(ambiguous.nuc, min.len = 1, any.missing = FALSE)
  assertSubset(ambiguous.nuc, c("zero", "equal", "discard", "empirical"))

  parameters <- list(
    stepsize = stepsize,
    file.order = file.order,
    shuffle.entries = shuffle.entries,
    proportion.per.seq = proportion.per.seq,
    max.samples.per.file = max.samples.per.file,
    reverse.complements = reverse.complements,
    ambiguous.nuc = ambiguous.nuc,
    skip.amb.nuc = skip.amb.nuc
  )

  lengths <- vapply(parameters, length, numeric(1))
  if (any(lengths != 1)) {
    if (length(unique(lengths[lengths != 1])) > 1) {
      stop("Parameters must either have length 1 or the same length as the number of targets.")
    }
    targets <- lengths[lengths != 1][[1]]
    parameters <- lapply(parameters, function(x) if (length(x) == 1) rep(x, targets) else x)
    parameters$targets <- targets
  } else {
    parameters$targets <- NA_integer_
  }
  parameters$maxlen <- maxlen
  parameters$vocabulary <- vocabulary
  structure(parameters, class = "SampleDescription")
}

#' @export
print.SampleDescription <- function(x, ...) {
  cat("SampleDescription\n")
  for (item in names(x)) {
    printstring <- x[[item]] %??% "NULL"
    if (length(printstring) > 1) {
      printstring <- paste0("[", paste(printstring, collapse = ","), "]")
    }
    cat(sprintf("  %s: %s\n", item, printstring))
  }
  invisible(x)
}