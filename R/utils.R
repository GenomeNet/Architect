

# extended do.call()
invoke <- function(.f, ..., .args = list()) {
  assertList(.args)
  do.call(.f, c(list(...), .args))
}
