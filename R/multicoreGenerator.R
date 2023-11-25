
#' @title Create a multicore Generator
#'
#' @description
#' Launches a multicore generator that loads data from disk in parallel.
#' @param gen.constructor (`function`)\cr
#'   A function that returns a generator. This function is called in each
#'   process. It should take a single argument `seed` and return a generator.
#'   This generator is itself a function with no arguments that returns a
#'   new batch of data each time it is called.
#' @param seed (integer `numeric(1)`)\cr
#'   The seed to use. Individual workers are seeded using `seed + 0:(n.workers - 1)`.
#' @param n.workers (integer `numeric(1)`)\cr
#'   The number of workers to use. If `NULL`, the number of workers is
#'   determined using `parallelly::availableCores()`.
#' @param queue.depth (integer `numeric(1)`)\cr
#'   The number of batches to load in advance. Making this larger prevents
#'   stalls due to variations in individual `gen()`-calls' runtimes (which
#'   can happen due to variations in FASTA file sizes), but also increases
#'   memory usage.
#'   Defaults to `NULL`: Use `cache.size.mb` instead. Note that exactly one of
#'   `queue.depth` and `cache.size.mb` must be given.
#' @param `cache.size.mb` (`numeric(1)`)\cr
#'   Approximate memory usage of the queue datastructure on the master process in megabytes. This
#'   only counts the content of batches, not the overhead of the queue, so actual
#'   memory usage is slightly higher. This is independent of individual worker
#'   process memory usage, which is determined by the generators used and will include
#'   at least one batch of data.
#'   Defaults to `NULL`: Use `queue.depth` instead.
#'   Note that exactly one of `queue.depth` and `cache.size.mb` must be given.
#' @param cluster.constructor (`cluster`)\cr
#'   Function that creates the cluster to use. It must be a `parallel` or `parallelly` PSOCK cluster.
#'   By default, a cluster is created using `parallelly::makeClusterPSOCK()`.
multicoreGenerator <- function(gen.constructor, seed, n.workers = NULL, queue.depth = NULL, cache.size.mb = NULL, cluster.constructor = NULL, packages = "deepG") {
  assertFunction(gen.constructor)
  n.workers <- n.workers %??% availableCores()
  assertCount(n.workers, positive = TRUE)
  seed <- assertInt(seed)
  assertCount(queue.depth, positive = TRUE, null.ok = TRUE)
  assertNumber(cache.size.mb, lower = 0, null.ok = TRUE)
  if (is.null(queue.depth) == is.null(cache.size.mb)) stop("Exactly one of queue.depth and cache.size.mb must be given.")
  assertCharacter(packages, any.missing = FALSE)

  assertFunction(cluster.constructor, args = "n.workers", null.ok = TRUE)
  if (is.null(cluster.constructor)) {
    tries <- getOption("parallelly.makeNodePSOCK.tries", 60)
    delay <- getOption("parallelly.makeNodePSOCK.tries.delay", 1)
    cluster.constructor <- crate(function(n.workers) {
      parallelly::makeClusterPSOCK(
        n.workers,
        autoStop = TRUE,
        tries = tries,
        delay = delay
      )
    }, tries, delay)
  }

  queue.cluster <- cluster.constructor(1)
  parallel::clusterEvalQ(queue.cluster, library("checkmate"))
  parallel::clusterExport(queue.cluster, "processGenerator", envir = topenv())

  parallel:::sendCall(queue.cluster[[1]],
    processQueue,
    list(gen.constructor = gen.constructor, seed = seed, n.workers = n.workers,
      queue.depth = queue.depth, cache.size.mb = cache.size.mb, cluster.constructor = cluster.constructor,
      packages = packages
    )
  )

  msg <- list(type = "EXEC", data = list(fun = crate(function() NULL), args = list(), return = TRUE, tag = NULL), tag = NULL)
  if (inherits(queue.cluster[[1]], "SOCK0node")) {
    rawmsg <- serialize(msg, NULL, xdr = FALSE)
  } else {
    rawmsg <- serialize(msg, NULL)
  }
  crate(function() {
    # send dummy function call, since we only want to tell the queue process that we want another batch.
    writeBin(rawmsg, queue.cluster[[1]]$con)
    parallel:::checkForRemoteErrors(list(parallel:::recvResult(queue.cluster[[1]])))[[1]]
  }, queue.cluster, rawmsg)
}

processQueue <- function(gen.constructor, seed, n.workers, queue.depth, cache.size.mb, cluster.constructor, packages) {
  # not user-facing, so the following would be defects, not user-errors:
  assertFunction(gen.constructor)
  assertInt(seed)
  assertCount(n.workers, positive = TRUE)
  assertCount(queue.depth, positive = TRUE, null.ok = TRUE)
  assertNumber(cache.size.mb, lower = 0, null.ok = TRUE)
  assertTRUE(is.null(queue.depth) != is.null(cache.size.mb))
  assertCharacter(packages, any.missing = FALSE)

  master <- dynGet("master")  # get this from socket worker loop, which is calling us

  cluster <- cluster.constructor(n.workers)
  on.exit(parallel::stopCluster(cluster))

  for (l in c(packages, "checkmate")) parallel::clusterCall(cluster, library, l, character.only = TRUE)

  assertClass(cluster, "SOCKcluster")

  connections <- lapply(seq_along(cluster), function(idx) {
    parallel:::sendCall(
      con = cluster[[idx]],
      fun = processGenerator,
      args = list(gen.constructor = gen.constructor, seed = seed + idx - 1)
    )
    cluster[[idx]]$con
  })


  connections[[length(connections) + 1]] <- master$con

  wants.batch <- FALSE
  queue <- NULL
  queue.position <- NULL
  # queue full: do not wait for given connection
  # master is the n+1'st connection and never full.
  full <- vector("logical", n.workers + 1)

  repeat {
    # wait for any message from master or those workers that are not full
    valid.connections <- connections[!full]
    # socketSelect gives logical vector of length 'valid.connections'.
    # which(!full)[<logical>] maps that to the indices inside 'connections'
    incoming <- which(!full)[socketSelect(valid.connections, write = FALSE)]
    if (!length(incoming)) next  # maybe select() sometimes returns for no reason?

    # handle message by master: set a flag that we want a batch, and record the time
    # we do this first to keep master latency low
    if (length(connections) %in% incoming) {
      msg <- parallel:::recvData(master)
      if (msg$type != "EXEC") stop("Received message that is not a query -> stopping")
      if (wants.batch) stop("Unexpected double-request")
      wants.batch <- TRUE
    }

    # should we service master? Only if
    # - master wants something
    # - we have at least received one blob and therefore the queue is initialized
    # - the queue position is not empty.
    if (wants.batch && !is.null(queue)) {
      # we have a batch request from master
      ## -> pop blob from queue
      cur.beginning <- queue[[queue.position]]$beginning
      cur.end <- queue[[queue.position]]$end

      # check if queue is empty. We also need to check the `full`-status, because full queues also have beginning == end
      if (cur.beginning != cur.end || full[[queue.position]]) {
        # queue is not empty: pop blob and send to master
        full[[queue.position]] <- FALSE
        # pop from queue.position and increment queue.position
        blob <- queue[[queue.position]]$data[[cur.beginning]]
        queue[[queue.position]]$data[cur.beginning] <- list(NULL)
        queue[[queue.position]]$beginning <- cur.beginning %% queue.depth + 1L

        writeBin(blob, master$con)

        queue.position <- (queue.position %% n.workers) + 1L
        wants.batch <- FALSE
        # check if master wants another batch
        next
      }
    }

    # we *could* do this in a loop, but we want to keep latency low:
    # after each received blob, we check again if master wants something
    idx <- incoming[[1]]
    if (idx == length(connections)) next  # happens when master requested something but queues are empty

    # handle everything else: just queue things, bro
    blobsize <- parallel:::recvData(cluster[[idx]])

    assertCount(blobsize, positive = TRUE)
    if (is.null(queue.depth)) {
      # first time we see a blob: determine queue.depth, assuming all blob have the same size
      queue.depth <- as.integer(ceiling(cache.size.mb * 1024^2 / (blobsize * n.workers)))
      if (queue.depth > 1L) queue.depth <- queue.depth - 1L  # count batch inside (blocking) worker as well
    }
    if (is.null(queue)) {
      # first time we see a blob: initialize queue
      queue <- list()
      queue.position <- 1
      for (idx.w in seq_len(n.workers)) {
        queue[[idx.w]] <- list(
          data = vector("list", queue.depth),
          beginning = 1L,
          end = 1L
        )
      }
    }
    ## push blob to queue
    # we can trust that the respective queue is not full because we 'socketSelect()' only for non-full queues
    queue[[idx]]$data[[queue[[idx]]$end]] <- readBin(connections[[idx]], what = "raw", n = blobsize)
    queue[[idx]]$end <- queue[[idx]]$end %% queue.depth + 1L
    if (queue[[idx]]$end == queue[[idx]]$beginning) {
      full[[idx]] <- TRUE
    }
  }
}

processGenerator <- function(gen.constructor, seed) {
  assertFunction(gen.constructor)
  assertInt(seed)
  generator <- gen.constructor(seed)
  master <- dynGet("master")
  # iterate as long as there are no other messages (i.e. no stop message)

  while (!socketSelect(list(master$con), write = FALSE, timeout = 0)) {
    t1 <- proc.time()
    batch <- generator()
    t2 <- proc.time()
    batch.serial <- serialize(list(
      type = "VALUE",
      value = batch,
      success = TRUE,
      time = t2 - t1,
      tag = NULL
    ), NULL, xdr = FALSE)
    rm(batch); gc(); gc(); gc(); gc()  # clean up before we send to the queue, since that might block for a long time
    parallel:::sendData(master, length(batch.serial))  # send this as serialized data first, so that recvData on the other end works
    writeBin(batch.serial, master$con)
  }

}


crate <- function(.fn, ..., .parent = topenv(parent.frame())) {
  nn <- vapply(substitute(list(...)), as.character, "")[-1L]
  environment(.fn) <- list2env(structure(list(...), names = nn), parent = .parent)
  .fn
}


if (FALSE) {
  # test code
 # library("parallelly")
 # library("checkmate")
 # library("deepG")

  gen <- multicoreGenerator(
    function(seed) {
    ctr <- seed * 100
    function() {
      ctr <<- ctr + 1
      ctr
    }
    },
    seed = 3,
    n.workers = 2,
    queue.depth = 3,
    packages = character(0)
  )

  print(replicate(10, gen()))


  gen <- multicoreGenerator(
    function(seed) {
    ctr <- seed * 100
    function() {
      Sys.sleep(1)
      ctr <<- ctr + 1
      c(ctr, runif(1000*4))
    }
    },
    seed = 3,
    n.workers = 5,
    queue.depth = 20,
    packages = character(0)
  )

  cat("1st\n")
  print(system.time(gen()))
  print(system.time(gen()))
  print(system.time(gen()))
  print(system.time(gen()))
  print(system.time(gen()))
  cat("2nd\n")
  print(system.time(gen()))
  print(system.time(gen()))

  Sys.sleep(30)
  cat("1st\n")
  print(system.time(gen()))
  print(system.time(gen()))
  print(system.time(gen()))
  print(system.time(gen()))
  print(system.time(gen()))

  cat("2nd\n")
  print(system.time(gen()))
  print(system.time(gen()))
  print(system.time(gen()))
  print(system.time(gen()))
  print(system.time(gen()))

  cat("3rd\n")
  print(system.time(gen()))
  print(system.time(gen()))
  print(system.time(gen()))
  print(system.time(gen()))
  print(system.time(gen()))

  cat("4th\n")
  print(system.time(gen()))
  print(system.time(gen()))
  print(system.time(gen()))
  print(system.time(gen()))
  print(system.time(gen()))

  cat("5th\n")
  print(system.time(gen()))
  print(system.time(gen()))
  print(system.time(gen()))
  print(system.time(gen()))
  print(system.time(gen()))


#  for (i in 1:10) {
#    print(gen()$X)
#  }
}