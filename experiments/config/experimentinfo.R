
# Data and Objective Settings:
# - Where to find data and preloaded generators etc.
# - How many epochs to evaluate at what walltime value

genseed <- c(645, 456)
datapaths <- sort(list.dirs("trainingdata", full.names = TRUE, recursive = FALSE))
path <- paste0(datapaths, "/train")
path.val <- paste0(datapaths, "/validation")

labels <- basename(dirname(path))  # could also be something like `c("virus", "bacteria")`
proportion_per_file <- NULL

plginfo <- data.table::data.table(
  maxlen = c(150, 10000),
  samplesize = c(1e4, 1e4),
  path = c("trainingdata/plg150_bnp.rds", "trainingdata/plg10k_bnp.rds")
)
data.table::setkeyv(plginfo, "maxlen")

## plginfo <- data.table::data.table(
##   maxlen = c(150, 10000),
##   samplesize = c(3e3, 1e3),
##   path = c("plg150_bnp.rds", "plg10k_bnp.rds")
## )
## data.table::setkeyv(plginfo, "maxlen")


getEpochsDesired <- function(walltimes) {
  hour <- 60 * 60
  ifelse(walltimes <= 2.5 * hour, 20, ifelse(walltimes <= 8 * hour, 100, 200))
}
