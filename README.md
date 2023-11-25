# GenomeNet Architect

## Installation

First, install `tensorflow`, as it is needed by `deepG`.

```r
install.packages("tensorflow")
tensorflow::install_tensorflow()
```

Evaluation currently needs a few modified versions of `deepG` and other packages:

```r
remotes::install_github("mlr-org/mlrMBO@imputefun_metainfo")  # mlrMBO bugfix
remotes::install_github("mlr-org/parallelMap@batchtoolsfix")  # parallelMap batchtools bugfix
remotes::install_github("GenomeNet/deepG@always_validate")  # deepG evaluation patch
install.packages("snow")  # used for default parallelization
```

Then install the GNArchitect package:

```r
remotes::install_github("GenomeNet/Architect")
```

GenomeNet Architect requires that R is able to access GPUs.
You can check this by running
```r
tensorflow::tf$config$list_physical_devices("GPU")
```
The result should list one or more "`PhysicalDevice`"s.
If the result is an empty list, even though your machine does have a GPU, then something went wrong while installing tensorflow.

## Running Optimization

All of the following assumes that you have cloned this repository, and that your current working directory is the base of this repository.

```sh
git clone 'https://github.com/GenomeNet/Architect'
cd Architect
```

### Prepare Data

Training data should be saved in the `trainingdata/` folder.
There should be one folder for each class.
Each of these folders in return should contain a folder `train/` for training and `validation/` for validation.

For a small demo run, you can use the mini-dataset provided in this repository.
It consists of a small sample from the NCBI Genome database:

```sh
tar xJf trainingdata/virus_bacteria_mini.tar.xz -C trainingdata
```

If you want to use training data located in a different folder, you need to modify the `path`, `path.val` and `labels` variables in the `experiments/config/experimentinfo.R` file; see the section on [Data Location](#data-location) below.
It may also be a good idea to adjust the `proportion_per_file` variable; it corresponds to the [`proportion_per_seq` argument of deepG's `train_model()` function](https://deepg.de/reference/train_model.html).

### Creating Validation Cache Files

GenomeNet Architect pre-processes the validation data and saves the result in cache files, which saves time during performance evaluation since pre-processing does not need to be done again for every model fit (see `?readPLG()`).
Note that this step needs to be done again if the validation data changes.

The following creates the cache files for sequence lengths 150 and 10000 sequentially.
However, since each of the `makePLG()` calls can take many hours, it is recommended to run both `makePLG()` steps in parallel sessions.

```r
source("experiments/createValidationData.R")
makePLG(150)
makePLG(10000)
```

### Run MBO Optimization

Experiments are conducted for both "recurrent" and "gap" type networks, both with and without residual blocks, and for sequence lengths of 150 and 10'000 nucleotides -- 8 setups in total.
The setups can be listed through the `runs` variable in `experiments/config/mboruns.R`.
```r
source("experiments/config/mboruns.R")
runs
#>    maxlen      type residual_block
#> 1:    150       gap              0
#> 2:    150       gap              1
#> 3:    150 recurrent              0
#> 4:    150 recurrent              1
#> 5:  10000       gap              0
#> 6:  10000       gap              1
#> 7:  10000 recurrent              0
#> 8:  10000 recurrent              1
```

Each experiment has an *experiment index*, which corresponds to a row in this table.
E.g. the experiment with index 1 has `maxlen` set to 150, `type` set to `"gap"`, and `residual_block` set to 0.

Each of these experiments needs to be run separately, either one after the other, or on different computers.

The following uses the `experiments/runMbo.R` script to initialize an experiment file (which is stored in `data/`) and then starts its execution.
Change the `EXPERIMENT_INDEX` variable to run a different experiment.

Do the following from the shell:

```sh
EXPERIMENT_INDEX=1

echo Initializing file for experiment $EXPERIMENT_INDEX
Rscript experiments/runMbo.R init data/opt_local_${EXPERIMENT_INDEX}.RData $EXPERIMENT_INDEX

echo Running experiment $EXPERIMENT_INDEX
Rscript experiments/runMbo.R run data/opt_local_${EXPERIMENT_INDEX}.RData
```

## Analysis of Results

The `analysis` folder contains scripts that convert the resulting optimization databases (`data/opt_<n>.RData`, in our case) more fit to analysis and extract a few results and plots.

### `collect_run_results.R`

`analysis/collect_run_results.R` is used to collect the results of all optimization runs into a single file, `data/optruns.rds`. It should be executed from the root folder. The script assumes that results are in the `data` folder with the name pattern `data/opt_<i>.RData`. `<i>` should range from 1 to the number of rows of `runs` in `experiments/config/experimentinfo.R`. Adjust the `basepath` variable in this script to use other data.

```sh
RScript analysis/collect_run_results.R
```

The `data/optruns.rds` file then contains a named list with the following entries:

* **`INFO`**: Short string explaining all other entries.
* **`optpath.x`**: Evaluated hyperparameters, pre-transformation (i.e. mostly on log-scale).
* **`optpath.x.transformed`**: Evaluated hyperparameters, transformed to arguments of `create_model_genomenet()`.
* **`paramsets`**: `ParamHelpers` `ParamSet`s (i.e. search spaces) of individual runs
* **`metainfo`**: Further information about each evaluation:
    - Performance: `y` -- the performance seen by MBO; `perfo.acc`, `perf.loss` -- offline calculated values
    - Evaluation statistics: `error.message`, `exec.time` -- runtime of model evaluation, including time for batch size estimation
    - MBO-internal details: `cb`, ..., `lambda`, see `mlrMBO` documentation
    - information about the model and its invocation: `batchsize`, ..., `numepochs`
    - `runid`: index into `runs` table in `experiments/config/mboruns.R`
    - `walltime`: time limit given to trainNetwork

## Changing the Experimental Setup

Depending on the environment in which you are running GenomeNet Architect, or depending on the task you are trying to solve, you may need to adjust some of the evaluation settings.
The following describes how to change various settings.

### Parallelization and Number of GPUs

The current setup runs performance evaluations locally and parallelizes automatically to all GPUs that are listed in the `CUDA_VISIBLE_DEVICES` environmental variable.

If you want to use a different number of parallel processes, you need to change two files:

1. Set the `MAX.PARALLEL` entry in the `experiments/config/parallelization.R` file to the desired number of parallel threads.
2. Set the `ncpus` argument of the `makeClusterFunctionsSocket()` call in the `batchtools.conf.R` file to the same value.
   Ignore the `default.resources` entry: here `ncpus` should remain 1!

Internally, GenomeNet Architect uses [`batchtools`](https://mllg.github.io/batchtools/index.html) for parallelization.
`batchtools` makes it possible to parallelize on a compute cluster, i.e. use different GPUs on different compute nodes at the same time.
For this, you need to modify the `batchtools.conf.R` file and use a different `makeClusterFunctions*()`.
See the [documentation of `batchtools`](https://mllg.github.io/batchtools/articles/batchtools.html) about this.
You also need to set the `MAX.PARALLEL` entry in the `experiments/config/parallelization.R` manually to the number of parallel processes you want to use.

#### Test Runs

To check your parallelization setup, you can create "demo"-experimental setups.
Initialize a demo-experiment by appending `--demo` to the `runMbo.R --init` invocation:

```sh
EXPERIMENT_INDEX=1

Rscript experiments/runMbo.R init data/opt_local_demo_${EXPERIMENT_INDEX}.RData $EXPERIMENT_INDEX --demo
```

If you evaluate this, individual evaluations will run very quickly, so you can check that parallelization works:
```sh
Rscript experiments/runMbo.R run data/opt_local_demo_${EXPERIMENT_INDEX}.RData
```

### Data Location

The location of the training / validation data is determined in the `experiments/config/experimentinfo.R` file.
By default, all folders found in the `trainingdata/` folder are used, and it is assumed that these folders each have a `train/` and `validation/` subfolder (see the content of the `trainingdata/virus_bacteria_mini.tar.xz` file for an example).

If your data is in a different folder, you should modify the `path`, `path.val`, and `labels` variables:

* `path` and `path.val` should both be vectors, containing one entry for each class, pointing to folders containing FASTA files.
`path` is used for training, `path.val` for validation.
* `labels` should contain the labels for the classes found in the directories; there must be one label for each folder in `path`.
  By default, it uses the directory names of the `path` where the `/train` folder is located.
  If you are using a different folder setup, you likely need to adjust the `labels` as well.
* It may also be a good idea to adjust the `proportion_per_file` variable; it corresponds to the [`proportion_per_seq` argument of deepG's `train_model()` function](https://deepg.de/reference/train_model.html).

When the content of your validation data changes, you need to re-create the validation cache files, see the [`Creating Validation Cache Files`](#creating-validation-cache-files) section above.

### Sequence Lengths

In its current setup, GenomeNet Architect will perform optimization for networks that have an input sequence length of 150 nt, as well as 10'000 nt.
To change the sequence lengths being used, you need to modify two files:

1. Change the `MAXLEN` variable in the `experiments/config/mboruns.R` file.
  This variable can be a vector of any length; To do experiments with only 150 nucleotides, for example, set `MAXLEN <- 150`.
2. If necessary, change the `maxlen` entries in the `plginfo` table found in the `experiments/config/experimentinfo.R`.
  `maxlen` in `plginfo` must contain the value(s) you entered in `MAXLEN` above.
  If you change `maxlen` in `plginfo`, the validation cache files need to be re-created, see the [`Creating Validation Cache Files`](#creating-validation-cache-files) section above.

The `MAXLEN` variable in `mboruns.R` determines which lengths are *actually* evaluated, while `plginfo` in `experimentinfo.R` determines which validation data is cached.
The latter is allowed to be a superset of the former.
This means that if you want to modify `MAXLEN` to only evaluate 150 nt sequences, there is no need to modify `plginfo`.

### Network Types

The `TYPE` and `RESIDUAL_BLOCK` variables in `experiments/config/mboruns.R` determine which experiments are conducted.
By default, both recurrent networks and GAP-networks are optimized, both with and without residual blocks.
To restrict the kinds of evaluations being done, you can change these variables.
E.g. to only optimize GAP-networks, set `TYPE <- "gap"`.

Alternatively, you can also modify the `runs` variable. The default setup uses both "recurrent" and "gap" type networks, bot with and without residual blocks, and for sequence lengths of 150 and 10'000 nucleotides.
The `runs` table in the `experiments/config/mboruns.R` has therefore 8 lines.

### Fidelity Steps

The `fidelity` table in `experiments/config/mboruns.R` controls how the walltime timeout changes over the course of optimization.
MBO first evaluates an initial design, around 50 points, with the topmost `walltimehrs` as timeout (in hours).
It then traverses this table from top to bottom.
In the default setup, it runs 150 evaluations with 2 hours timeout, then 100 configurations with 6 hours timeout etc.
Modify this table to change the timeout of point evaluations.

### TensorBoard

GenomeNet Architect makes it possible to inspect performance evaluation runs via [TensorBoard](https://www.tensorflow.org/tensorboard).
TensorBoard logs are written to the `logs/tensorboard_opt` folder.
You can change the `OUTPUTDIR` variable in the `experiments/config/run.conf` file to set tensorboard output to a different location.
GenomeNet Architect will write to the `tensorboard_opt` folder found in the directory indicated by `OUTPUTDIR`.

## Experiment Script Overview

Scripts are present in the `experiments/` folder to do various things. All of these can be executed by running them directly (e.g. `experiments/runMbo.R <args..>`) or using `RScript` (e.g. `Rscript experiments/runMbo.R <args..>`). Note that all of these must be run from the root folder of this repository.

- **`evaluateRandomConfig.R`**: Evaluate a sampled configuration point. This can e.g. be useful to test the optimization environment. Run as `experiments/evaluateRandomConfig.R <seed>`, with an integer value as random seed.
- **`runMbo.R`**: Initialize an MBO run (i.e. create the optimization configuration file) or run the optimization.

    For optimization, at first a database file needs to be created, by calling `experiments/runMbo.R init <filename> <experiment-index>`. `<filename>` is the database file created (which must not already exist), `<experiment-index>` is an index into the `runs` `data.table` in `experiments/config/mboruns.R`. It is possible to append `--demo` to create a toy-problem to test the parallelization setup.

    Optimization is the evaluated by calling `experiments/runMbo.R run <filename>`. `<filename>` is the database file created using `experiments/runMBO.R init` before.
- **`resetRuns.R`**: Small helper-file: copy an MBO database file and remove all runs except for the random initialization. Thit can e.g. be used to compare different fidelity-setups or using `thoroughify.R`.
- **`thoroughify.R`**: Set the MBO surrogate optimization method to a more thorugh method. This takes more time than the default method and its benefits are not proven; this file was mostly used for internal experiments. Usage is not recommended.
- **`retryRuns.R`**: Remove runs from the MBO database that failed. This can be used if runs failed for technical reasons (e.g. the cluster killed a job that would otherwise have succeeded) but should not be used as a datapoint for the surrogate model. Will never remove runs from the initial design.

A note on mbo database files (the ones created / used by `runMbo.R`): MBO *always* saves to the *original* path of the mbo database. Renaming, moving or copying the file is therefore not possible. The only exception is when a partial copy is created using `resetRuns.R`. You should *never* create a copy of an existing mbo database and run `runMbo.R run` on this file, since the original file will then be *overwritten*.

