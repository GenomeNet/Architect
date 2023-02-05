
#' @title Create an Objective Function to be Optimized with MBO
#'
#' @description
#' Creates the objective function that should be optimized.
#'
#' The objective function automatically adjusts batch size to fit in a given GPU,
#' and adjusts epoch size so that an approximate target number of desired epochs can be
#' evaluated in the requested `walltime`.
#'
#' The function should be evaluated inside its own R-session.
#'
#' @param maxlen (integer `numeric(1)`)\cr
#'   Input sequence length of models to evaluate.
#' @param type (`character(1)`)\cr
#'   One of `"gap"` or `"recurrent`.
#' @param getEpochsDesired (`function`)\cr
#'   A `function` that maps a `numeric(1)`, indicating walltime in number of seconds, to an integer
#'   valued `numeric(1)`, indicating the number of epochs that should approximately be evaluated.
#' @param path (`character`)\cr
#'   Training data paths.
#' @param path.val (`character`)\cr
#'   Validation data paths.
#' @param labels (`character`)\cr
#'   Class labels.
#' @param plgpath (`character(1)`)\cr
#'   Path of a preloaded generator that is read with [`readPLG()`].
#' @param outdir (`character(1)`)\cr
#'   Output directory. The tensorboard output is put in the `tensorboard_opt/` subdirectory of this.
#' @return `function`.
#' @export
makeGenomeNetObjective <- function(maxlen, type, getEpochsDesired, path, path.val, labels, plgpath, outdir) {
  assertFunction(getEpochsDesired)
  assertInt(getEpochsDesired(3600))
  assertCharacter(path, min.len = 1, any.missing = FALSE)
  assertCharacter(path.val, len = length(path), any.missing = FALSE)
  
  obj.genomenet.fn <- function(x) {

    #  - performance: maximum of validation points
    #    - but limit number of points of which we take the maximum

    ########
    # Data setup
    source("experiments/config/experimentinfo.R")
    run.name <- sprintf("run_opt4_%s_%s",
      gsub("[ :]", "_", sprintf("%s", Sys.time())),
      paste(sample(letters, 10, replace = TRUE), collapse = "")
    )

    # Adapt the inputs from search space
    walltime <- x$walltime

    hour <- 3600

    # epochs desired:
    # 1-2h -> 20
    # 3-6h -> 100
    # 10+h -> 200
    epochs.desired <- getEpochsDesired(walltime)

    x <- cleanUpX(x, num_targets = length(labels))

    cat("Evaluating: ")
    cat(deparse(x))
    cat("\n")


    defaultargs <- list(
      train_type = "label_folder",
      path = path,
      path.val = path.val,
      run.name = run.name,
      patience = 100000, # large number --> don't use
      step = maxlen, # 150 or 10000
      randomFiles = TRUE,
      vocabulary = c("a", "c", "g", "t"),
      shuffleFastaEntries = TRUE,
      labelVocabulary = labels,
      reverseComplements = FALSE,
      reverseComplementEncoding = x$reverse_encoding,
      ambiguous_nuc = "discard", # CHANGED as compared to previous MBO runs (see code Anil)
      proportion_per_file = c(0.1, 0.9,0.9),
      seed = c(645, 456), # ANIL CHECK: Before we had random seeds; what do you think?
      skip_amb_nuc = 0.001,
      lr.plateau.factor = 0.5,
      reduce_lr_on_plateau = FALSE,
      validation_only_after_training = FALSE,
      padding = FALSE,
      tensorboard.log = file.path(outdir, "tensorboard_opt")
    )

    # get batch size
    batch.steps <- estimateBatchSize(x, defaultargs)$batchmultiplier
    defaultargs$batch.size <- batch.steps * length(labels)

    ########
    # Setup of GPU's
    connectGpu()

    gpumem <- getGpuMemFree()

    defaultargs$model <- do.call(create_model_genomenet, x)

    defaultargs$max_samples <- ifelse(maxlen == 150, 100, 20)

    valsizetarget <- ifelse(maxlen == 150, 27000, 4000)

    validation.steps <- round(valsizetarget / defaultargs$batch.size)

    gen.val <- readPLG(plgpath, batchsize = defaultargs$batch.size,
      numbatches = validation.steps, reverseComplementEncoding = x$reverse_encoding)


    time.per.epoch <- walltime / epochs.desired

    steps.per.epoch <- estimateStepsPerEpoch(time.per.epoch, defaultargs, gen.val = gen.val, validationsteps = validation.steps)

    validation.split <- (validation.steps - 0.5) / steps.per.epoch

    trained <- invoke(.f = trainNetwork, .args = defaultargs,
      epochs = 1e6,  # a large number, because we stop depending on runtime
      steps.per.epoch = steps.per.epoch,
      output = list(none = FALSE, checkpoints = FALSE, tensorboard = TRUE, log = FALSE, serialize_model = FALSE, full_model = FALSE),
      validation.split = validation.split,
      early_stopping_time = walltime,
      gen.val = gen.val
    )

    na.skip <- function(x) x[!is.na(x)]

    trained$metrics$val_loss <- c(na.skip(trained$metrics$val_loss), trained$val_loss)
    trained$metrics$val_acc <- c(na.skip(trained$metrics$val_acc), trained$val_acc)

    res <- evaluatePerf(trained$metrics$val_acc, epochs.desired)
    attr(res, "extras") = list(.train_result = trained,
      batchsize = defaultargs$batch.size,
      stepsperepoch = steps.per.epoch,
      validationsteps = validation.steps,
      paramcount = defaultargs$model$count_params()
    )

    return(res)
  }


  makeSingleObjectiveFunction(name = "genomenet",
    fn = obj.genomenet.fn,
    par.set = get_searchspace(maxlen = maxlen, type = type),
    noisy = TRUE,
    has.simple.signature = FALSE,
    minimize = FALSE # We are maximizing the validation accuracy
  )
}
