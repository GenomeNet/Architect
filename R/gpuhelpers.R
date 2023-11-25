
# Connect to the first GPU
connectGpu <- function() {
  devs <- tensorflow::tf$config$experimental$list_physical_devices("GPU")
  if (!length(devs)) stop("Currently must use exactly one GPU")
  if (length(devs) != 1) {
    # restrict to just one gpu
    tensorflow::tf$config$set_visible_devices(devs[1], "GPU")
  }
  tensorflow::tf$config$experimental$set_memory_growth(devs[[1]], TRUE)

  ## gpu <- tensorflow::tf$device(devs[[1]]$name)
  ## gpu[["_device_name"]]
  ## data.table::fread("nvidia-smi --query-gpu=index,memory.free --format=csv")

}

#' @title Get free memory of GPU in use
#'
#' @description
#' Uses `nvidia-smi` and parses the result.
#' Only works when a single `GPU` is present or assigned via
#' `CUDA_VISIBLE_DEVICES`.
#'
#' @return `numeric(1)`.
#' @export
getGpuMemFree <- function() {
  cuda_id <- Sys.getenv("CUDA_VISIBLE_DEVICES")
  if (cuda_id == "") cuda_id <- "0"
  cuda_id <- as.numeric(cuda_id)
  memtable <- data.table::fread("nvidia-smi --query-gpu=index,memory.free --format=csv,nounits")
  memtable[J(cuda_id), 2, on = "index"][[1]]
}


# tries to see if batchsize * length(labels) works; returns TRUE if so, FALSE otherwise.
# run for 2 epochs, each 2 training steps + 1 validation step
# (as short as possible while still somewhat realistic)
tryModelMemory <- function(batchmultiplier, session) {
  result <- tryCatch({
    output <- session$run_with_output(args = list(batchmultiplier = batchmultiplier),
      function(batchmultiplier) {
        tensorflow::tf$keras$backend$clear_session()
        gc$collect()
        tensorflow::tf$keras$backend$clear_session()
        GNArchitect:::invoke(.f = deepG::trainNetwork, .args = defaultargs,
          model = model,
          batch.size = batchmultiplier * length(defaultargs$labelVocabulary),
          epochs = 2,
          steps.per.epoch = 2,
          output = list(none = TRUE, checkpoints = FALSE, tensorboard = FALSE,
            log = FALSE, serialize_model = FALSE, full_model = FALSE),
          validation.split = 0.5,
          max_samples = NULL  # no max_samples, we don't care about the data.
        )
        gc() ; gc() ; gc() ; gc()
      })
    TRUE
  }, error = function(e) {
    cat(sprintf("error caught; status of process: %s\n", session$get_state()))
    FALSE
  })
  if (!result) return(FALSE)

  if (!is.null(output$error)) {
    cat(sprintf("Error thrown by model; status of process: %s\n\n%s\n", session$get_state(),
      if (testString(output$error$message)) {
        tt <- strsplit(output$error$message, "\n")[[1]]
        if (length(tt) < 10) output$error$message else paste(c(head(tt, 5), "...", tail(tt, 5)), collapse = "\n")
      } else "" ))
    return(FALSE)
  }

  if (any(grepl(": W .*performance gains if more memory were available|W .*OOM",
    c(output$stdout, output$stderr)))) {
    cat("performance warning caught\n")
    return(FALSE)
  }

  return(TRUE)
}


# Creates and initializes a callr-session.
newRSession <- function(x, defaultargs) {
  session <- callr::r_session$new(wait_timeout = 120000)
  while (session$get_state() == "starting") Sys.sleep(1)

  session$run(args = list(x = x, defaultargs = defaultargs), function(x, defaultargs) {
    source("R/gpuhelpers.R")
    library("GNArchitect")
    .GlobalEnv$model <- do.call(create_model_genomenet, x)
    .GlobalEnv$defaultargs <- defaultargs
    .GlobalEnv$gc <- reticulate::import("gc")
  })
  session
}

#' @title Estimate Batch Size to be used for GenomeNet Model
#'
#' @description
#' Try out batchsizes for models that evaluate a single batch.
#' If the model is too large (i.e. an error is generated), a larger batchsize is tried.
#' If the model fits in the GPU, a larger batchsize is tried.
#' If the relative difference between the largest known good model size, and the smallest
#' known bad model size, is less than `tolerance` (or if the absolute difference is 1),
#' the search stops.
#' The resultin proposed model size is then multiplied by `downgrade` as a safety measure,
#' since models too close to the memory limit may sometimes fail later.
#' The minimum value returned is 1.
#'
#' Search happens in two stages: At first, models being tried are grown exponentially
#' between 2 and `upper.limit`, followed by binary search.
#'
#' Binary search experiments are run on an external `callr`-session, to avoid crashes on the
#' GPU bringing down the master R session.
#'
#' @param x (`list`)\cr
#'   Arguments given to [`create_model_genomenet()`] to create the model to be tried.
#' @param defaultargs (`list`)\cr
#'   Arguments given to [`deepG::trainNetwork`] to train the model. The following arguments
#'   should not be given: `model` (as this will be the result of `create_model_genomenet(x)`),
#'   `batch.size` (as this is determined by search), `epochs` (set to 2), `steps.per.epoch` (set to 2),
#'   `output` (set to `none`), `validation.split` (set to 0.5), `max_samples` (set to `NULL`).
#' @param tolerance (`numeric(1)`)\cr
#'   Relative tolerance.
#' @param downgrade (`numeric(1)`)\cr
#'   Value to multiply the value of the largest known good model with before returning. This should
#'   be < 1 and is recommended since training for many epochs sometimes uses more memory and saturates
#'   the GPU later.
#' @param upper.limit (`numeric(1)`)\cr
#'   Largest model to try. If not given, it is set to 400 if `x$maxlen` is > 1000, and 2048 otherwise.
#' @return `numeric(1)`: Recommended, reasonably safe, batch size.
#' @export
estimateBatchSize <- function(x, defaultargs, tolerance = 0.05, downgrade = 0.9, upper.limit = NULL) {
  session <- newRSession(x, defaultargs)
  on.exit(session$close())

  upper.limit <- upper.limit %??% if (x$maxlen > 1000) 400 else 2048  # highest batchmultiplier to try
  downgrade <- 0.9  # safety factor to apply to highest batchmultiplier that worked
  tolerance.factor <- 1 - tolerance
  highestWorking <- 1
  lowestNotWorking <- Inf
  trying <- 2
  results = list(worked = numeric(0), worked.not = numeric(0))
  while ({ cat(sprintf("Exponential Growth Phase: Trying size %s...", trying)) ; tryModelMemory(trying, session)}) {
    cat("worked.\n")
    results$worked[[length(results$worked) + 1]] <- trying
    highestWorking <- trying
    if (trying >= upper.limit) {
      rval <- max(floor(highestWorking * tolerance.factor), 1)
      cat(sprintf("Reached upper.limit; returning %s\n", rval))
      return(list(batchmultiplier = rval, results = results))
    }

    trying <- trying * 6
    if (session$get_state() != "idle") {
      session$close()
      session <- newRSession(x, defaultargs)
    }
  }
  cat("Did not work.\n")
  results$worked.not[[length(results$worked.not) + 1]] <- trying
  lowestNotWorking <- trying

  while (
      lowestNotWorking * tolerance.factor > highestWorking &&
      lowestNotWorking - 1 > highestWorking
  ) {
    trying <- round(sqrt(highestWorking * lowestNotWorking))

    if (session$get_state() != "idle") {
      session$close()
      session <- newRSession(x, defaultargs)
    }
    cat(sprintf("Bisection phase: Trying size %s...", trying))
    if (tryModelMemory(trying, session)) {
      cat("worked.\n")
      results$worked[[length(results$worked) + 1]] <- trying
      highestWorking <- trying
    } else {
      cat("Did not work.\n")
      results$worked.not[[length(results$worked.not) + 1]] <- trying
      lowestNotWorking  <- trying
    }
  }
  rval <- max(round(highestWorking * downgrade), 1)
  cat(sprintf("Reached relative tolerance; returning %s\n", rval))
  return(list(batchmultiplier = rval, results = results))
}

# try not to use too many points for max-evaluating, otherwise we favour longer runs because of noise
#' @title Evaluate performance of a model fitting run
#'
#' @description
#' Performance is evaluated as the maximum of all validation values reached.
#' However, if very many epochs were evaluated, this has an optimistic bias
#' since validation performance also has a random component.
#' Performance for runs that have more validation values than 50, and more than
#' 40% of `epochs_desired`, is therefore calculated as the second-highest
#' value of the last `0.2 * epochs_desired` validation values encountered.
#'
#' @param x (`numeric`)\cr
#'   Vector of validation values. Larger values should be better.
#' @param epochs_desired (`numeric(1)`)\cr
#'   Target number of epochs used by the evaluation.
#'   See also `getEpochsDesired` parameter of [`makeGenomeNetObjective()`].
#' @return `numeric(1)` the performance value.
#' @export
evaluatePerf <- function(x, epochs_desired) {
  if (epochs_desired <= 50 || length(x) <= epochs_desired * 0.4) {
    max(x)
  } else {
    x <- tail(x, round(epochs_desired * 0.2))
    x[[order(x, decreasing = TRUE)[[2]]]]
  }
  #  max(tail(x, epochs_desired / 2))
  # if (length(x) < 6) return(tail(x, 1))
  # predict(stats::loess(x ~ y, as.data.frame(cbind(x = x, y = seq_along(x)))), length(x))
}

####################
## time estimation

# evaluate `steps` steps and `validations` validations and measure runtime.
measureEpochTime <- function(steps, defaultargs, gen.val = NULL, validations = 0) {
  defaultargs$validation_only_after_training <- TRUE
  on.exit({gc() ; gc() ; gc() ; gc()})
  system.time({
    tryCatch({
      GNArchitect:::invoke(.f = trainNetwork, .args = defaultargs,
        epochs = 1,
        steps.per.epoch = steps,
        output = list(none = TRUE, checkpoints = FALSE, tensorboard = FALSE,
          log = FALSE, serialize_model = FALSE, full_model = FALSE),
        validation.split = validations / steps,
        gen.val = if (validations) gen.val
      )
      NULL
    }, error = function(e) {
      if (isTRUE(grepl("Empty training data", e$message))) {
        return(NULL)
      }
      stop(e)
    })
  })[[3]]
}

#' @title Estimate Steps per Epoch to do in a Given Time
#'
#' @description
#' Estimates the number of steps that can be done approximately in a given time.
#'
#' @param time.per.epoch (`numeric(1)`)\cr
#'   Time in seconds that a single epoch should take.
#' @param defaultargs (`list`)\cr
#'   Arguments of `trainNetwork`. Values that should not be present
#'   are `epochs` (set to 1), `steps.per.epoch` (set to various values for
#'   careful estimation), `output` (set to `none`), `validation.split`
#'   (set directly), and `gen.val` (given to `estimateStepsPerEpoch()` directly).
#' @param gen.val (`function`)\cr
#'   Validation generator. Since validation is sometimes switched off in the experiments
#'   conducted, this should be given separately.
#' @param validationsteps (integer `numeric(1)`)\cr
#'   Number of validation steps that are being performed. The time that a validation step
#'   takes is estimated separately and taken into account when determining te number of steps
#'   to make.
#' @return `numeric(1)`: The recommended number of steps per epoch.
#' @export
estimateStepsPerEpoch <- function(time.per.epoch, defaultargs, gen.val, validationsteps) {
  corefactor <- defaultargs$generator.cores  # need to respect number of cores used for generator, othrewise overhead drowns out everything else
  # We expect steps to take at least 1ms / sample
  time.theoretical <- defaultargs$batch.size * .0005
  time.per.validation.samples <- 40  # tps evaluation will take about 60 seconds; worst observed case 2 minutes.
  time.per.validation.samples <- min(time.per.validation.samples, validationsteps * 2)  # if validationsteps is small we don't need that much accuracy
  time.validation.theoretical <- 5 * time.per.validation.samples
  max.eval.fraction <- 1 / 3  # at most this much time should be spent evaluating
  stopifnot(is.numeric(time.theoretical) && length(time.theoretical == 1))
  cat(sprintf("Time measurement -- trying to spend %.3fs per epoch, expect to use at least %.3fs per step\n", time.per.epoch, time.theoretical))
  if (time.theoretical > time.per.epoch / 1.5) {
    cat("Estimated lower limit on time dictates one step per epoch.\n")
    return(1)
  }
  # warmup:
  cat("Time measurement -- Warmup: load model to gpu etc...\n")
  defaultargs$seed <- defaultargs$seed + 1  # adjust training data seed
  time <- measureEpochTime(corefactor, defaultargs = defaultargs) / corefactor
  cat(sprintf("Warmup took %.3fs\n", time))

  if (time > time.per.epoch) {
    cat("warmup alone took more than we want to use per epoch --> just use one step per epoch.\n")
    return(corefactor)
  }
  cat("Time measurement -- single step (overhead).\n")
  defaultargs$seed <- defaultargs$seed + 1
  time <- measureEpochTime(1, defaultargs = defaultargs)
  time.1.original <- time
  cat(sprintf("Single step took %.3fs\n", time))
  if (time < time.theoretical) {
    cat(sprintf("Ignoring this because of lower time limit. Correcting %.3fs --> %.3fs.\n", time, time.theoretical))
    time <- time.theoretical
  }
  if (corefactor > 1) {
    cat("Time measurement -- single step per core.\n")
    corefactor.timing.raw <- measureEpochTime(corefactor, defaultargs = defaultargs)
    time <- (corefactor.timing.raw - time.1.original) / corefactor
    cat(sprintf("Single step took %.3fs\n", time))
    if (time < time.theoretical) {
      cat(sprintf("Ignoring this because of lower time limit. Correcting %.3fs --> %.3fs.\n", time, time.theoretical))
      time <- time.theoretical
    }
  }

  steps.estimate <- max(1, round(time.per.epoch / time))
  if (time > time.per.epoch / 5) {
    cat(sprintf("Single step took more than 20%% of what we want to use per epoch --> Assume we don't need more accuracy here, using %s steps.\n", steps.estimate))
    return(steps.estimate)
  }

  steps.timing <- max(steps.estimate, 10 * corefactor)
  cat(sprintf("Time measurement -- Current estimate: %s steps per epoch. Next time measurement round with %s steps.\n", steps.estimate, steps.timing))
  defaultargs$seed <- defaultargs$seed + 1
  if (steps.timing <= corefactor) {
    cat(sprintf("Time measurement -- suggested timing steps %s not more than corefactor %s -- skipping.\n", steps.timing, corefactor))
    time.total <- corefactor.timing.raw
    steps.timing <- corefactor
  } else {
    time.total <- measureEpochTime(steps.timing, defaultargs = defaultargs)
  }
  cat(sprintf("Measured %s, subtracting %.3fs 1x-time against constant offset.\n", time.total, time.1.original))
  if (time.1.original > time.total) {
    time.1.original <- time.total
    # If time for one iter is > than for N iters, then one of these measurements is broken.
    # We modify time.1.original for the estimation of validation-time which comes later.
  }
  time.total <- time.total - time.1.original
  steps.timing <- steps.timing - 1
  time <- time.total / steps.timing
  cat(sprintf("%s steps took %.3fs -- %.3fs per step\n", steps.timing, time.total, time))

  if (time < time.theoretical) {
    cat(sprintf("Ignoring this because of lower time limit. Correcting %.3fs --> %.3fs.\n", time, time.theoretical))
    time <- time.theoretical
  }


  cat(sprintf("Measuring validation step timing with %s steps. Expecting at most %.3fs.\n", time.per.validation.samples, time.validation.theoretical))
  defaultargs$seed <- defaultargs$seed + 1
  time.validation <- measureEpochTime(1, defaultargs = defaultargs, gen.val = gen.val, validations = time.per.validation.samples)
  cat(sprintf("Took %.3f seconds.\n", time.validation))
  time.validation <- time.validation - time.1.original
  cat(sprintf("Subtracted one-sample-time %.3fs to get %.3fs seconds for validation only.\n", time.1.original, time.validation))
  if (time.validation > time.validation.theoretical || time.validation < 0) {
    time.validation <- min(time.validation.theoretical, max(0, time.validation))
    cat(sprintf("Corrected this to %.3fs on theoretical grounds.\n", time.validation))
  }

  time.validation <- time.validation / time.per.validation.samples
  time.validation.total <- time.validation * validationsteps

  cat(sprintf("Assuming single step validation time to be %.3fs, validation per epoch: %.3fs.\n", time.validation, time.validation.total))


  if (time.validation.total > max.eval.fraction * time.per.epoch) {
    time.per.epoch <- time.validation.total * (1 / max.eval.fraction - 1)
    cat(sprintf("Aiming to spend at most fraction of %.3f evaluating, therefore making epochs longer; (training) time.per.epoch is %.3fs for a total estimated epoch time of %.3fs.\n", max.eval.fraction, time.per.epoch, time.per.epoch + time.validation.total))
  } else {
    time.per.epoch <- time.per.epoch - time.validation.total
    cat(sprintf("Subtracting validation time from total epoch time; (training) time.per.epoch is %.3fs for a total estimated epoch time of %.3fs.\n", time.per.epoch, time.per.epoch + time.validation.total))
  }

  steps.estimate <- max(1, round(time.per.epoch / time))

  cat(sprintf("Estimating %s steps per epoch.\n", steps.estimate))

  return(steps.estimate)
}
