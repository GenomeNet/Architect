
library("GNArchitect")
library("deepG")
source("experiments/config/experimentinfo.R")  # loads path, path.val, genseed, plginfo

getvalgen <- function(maxlen, batch.size, proportion_per_file = NULL) {
  initializeGenerators(directories = path.val, format = "fasta", batch.size = batch.size, maxlen = maxlen,
    vocabulary = c("a", "c", "g", "t"), verbose = FALSE, randomFiles = TRUE, step = maxlen,
    showWarnings = FALSE, seed = genseed[[2]], shuffleFastaEntries = TRUE, skip_amb_nuc = 0.001,
    numberOfFiles = NULL, fileLog = NULL, reverseComplements = FALSE, reverseComplementEncoding = FALSE, val = TRUE,
    ambiguous_nuc = "discard", proportion_per_file = proportion_per_file, read_data = FALSE,
    use_quality_score = FALSE, padding = FALSE, max_samples = 1,
    split_seq = FALSE, concat_seq = NULL, added_label_path = NULL,
    add_input_as_seq = NULL, use_coverage = NULL, set_learning = NULL,
    proportion_entries = NULL, sample_by_file_size = TRUE, n_gram = NULL)

  labelByFolderGeneratorWrapper(val = TRUE, path = path.val, new_batch_size = batch.size,
    samples_per_target = NULL, batch.size = batch.size, voc_len = 4, maxlen = maxlen,
    reshape_mode = NULL, buffer_len = NULL, concat_maxlen = NULL)
}

makePLG <- function(maxlen, proportion_per_file = proportion_per_file_default) {
  ml <- maxlen # avoid nameclash in data.table
  system.time(preloadPLG(getvalgen(maxlen, 1e2 * length(path.val), proportion_per_file = proportion_per_file), plginfo[J(ml), samplesize] / 1e2, plginfo[J(ml), path]))
}

#### Create preloaded generator for maxlen 150:
#> makePLG(150)
## is the same as: system.time(preloadPLG(getvalgen(150, 1e2 * length(path.val)), plginfo[J(150), samplesize] / 1e2, plginfo[J(150), path]))
# we'd want 4e6 ideally, but that takes too much time; instead we get 1e4 because we never need more than this.
# OLD (was 1e4 samples):
##     user   system  elapsed
## 3163.558  145.685 3303.166
# OLD (3e4 samples, but wrong setup):
##     user   system  elapsed
## 8250.658   57.204 7964.912
# NEW (3e4 samples):
##     user   system  elapsed
## 8303.763   59.060 8015.936

#### Create preloaded generator for maxlen 10000:
#> makePLG(10000)
## is the same as: system.time(preloadPLG(getvalgen(10000, 1e2 * length(path.val)), plginfo[J(10000), samplesize] / 1e2, plginfo[J(10000), path]))
# we'd want 6e4...
# OLD (wrong setup)
##      user    system   elapsed
## 10948.188   202.761 10825.622
# NEW:
##     user   system  elapsed
## 11017.50   217.85 10880.76

# typical validation set size used so far: 200 steps, batchsize 150 ->  1200, 10k -> 30


# we care about .1% --> want to sample so that SE of accuracy is less than this
# say binomial: variance = n * p * (1-p); p probability to be correct (~ accuracy), say 75% for 150k
# --> variance = n * .75 * .25 --> standard error = sqrt(.75 * .25 / n), set this to 1e-4
# --> .75 * .25 / 1e-4^2 ==> 2e7

# accuracy for 10k: 98% --> .02 * .98 / 1e-4^2 ==> 2e6

# 1 sample * 1000 * 100 ==> 1.5 gigabyte --> limits to 4e6 (length 150)
# gpu memory limit: about 50 gigabytes

# length 10k --> 6e4
# (expected s.e.: sqrt(.02 * .98 / 6e4) ~ .06%)


# otoh: we want validation to finish in finite time. validation takes ~ 6ms / sample.
# we expect to have 1e5 samples total, take the last 20% of these (2e4) and try to get 40

# another problem: some of the batch sizes are 2332, validation evals are multiples of this.
# --> how about
# * aim for 4000 samples / evaluation
# * eval steps = round(4000 / batchsize)
# * get between 3332 and 4797 eval samples (( ==> max(round(4000 / 1:2332) * 1:2332) ))
# * 4000samples * .0058ms/sample ==> 23.2 seconds / evaluation

# suppose we overestimate validation time, but we have 3e4 samples fix now
# ## THIS IS WRONG: ==> use 28850 validation target size
#                   ==> max(round(28850 / 1:2332) * 1:2332)
# ## because batch size vs. batch steps
# use 27000 validation target size
# ==> max(round(27000 / ((1:2332) * 3)) * (1:2332) * 3)

# some empirical data on this:
# * batchfactor 33 --> 291 validation steps ==> 351s validation time
# * batchfactor 952 --> 10 validation steps ==> ~ 3s
# * batchfactor 64 --> 150 validation steps ==> 216s
# (remember, we'd like to have 24 s....)


# time estimation seems to take 530s (8.8 min), with standard deviation 157s (2.6 min)

# 1h-runs:
#  - take best
#  - aim for 20 evals == 8 min

# 3h-runs:
#  - take 2nd best from 20 last samples
#  - if <= 40 evals were made, take top 1
#  - aim for 100 evals == 40 min

# 10h-runs:
#  - take 2nd best from 40 last samples
#  - if <= 80 evals were made, take top 1
#  - aim for 200 evals == 80 min

## ------- scrap this, instead:

# set validation set size to 4000 for maxlen 10k, 27000 for maxlen 150
# (for 150 the validation steps problem is small most of the time. otoh we get the
# problem of uneven validation sizes if we have too few validation steps)

# 2h-runs:
#  - take best
#  - aim for 20 evals

# 6h-runs:
#  - take 2nd best from 20 last samples
#  - if <= 40 evals were made, take top 1
#  - aim for 100 evals == 40 min

# 20h-runs:
#  - take 2nd best from 40 last samples
#  - if <= 80 evals were made, take top 1
#  - aim for 200 evals == 80 min

