

# extended do.call()
invoke <- function(.f, ..., .args = list()) {
  assertList(.args)
  do.call(.f, c(list(...), .args))
}

# file.path(), but ignore NULL elements
file.path2 <- function(...) {
  paths <- Filter(function(x) !is.null(x), list(...))
  do.call(file.path, paths)
}