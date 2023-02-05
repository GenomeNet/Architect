# GenomeNet Architect

## Installation

First, install `tensorflow`, as it is needed by `deepG`.

```r
install.packages("tensorflow")
tensorflow::install_tensorflow()
```

Evaluation currently needs an old version of `deepG`, and a few other other packages with bugfix-patches applied that are not yet upstream:

```r
remotes::install_github("mb706/mlrMBO@manualmultifid")  # warmstart-patc of mlrMBO
remotes::install_github("mlr-org/parallelMap@batchtoolsfix")  # parallelMap with batchtools
remotes::install_github("GenomeNet/deepG@always_validate")  # deepG evaluation patch
```

Then install the GNArchitect package:

```r
remotes::install_github("GenomeNet/Architect")
```

## Running Optimization

### Prepare Data

Training data should be saved in the `trainingdata/` folder.
There should be one folder for each class.
Each of these folders in return should contain a folder `train/` for training and `validation/` for validation.
You can modify the `experiments/config/experimentinfo.R` file to use data in a different location.



Scripts that help conducting the experiments are located in the `experiments/` folder.

### Prepare Experiments

To conduct experiments, do the following:

1. Clone this repository.
2. Create a file `experiments/config/run.conf` with the line `OUTPUTDIR = "###"`, where `"###"` should instead be a path with sub-directory `###/tensorboard_opt/`, where the tensorboard output will be saved.
3. Adjust the `experiments/config/experimentinfo.R` file to your specific needs: Where is your data stored, what are the sample sizes for the preloaded validation data generators, how many epochs should be evaluated when model evaluation takes a given amount of runtime? If you use one of the sequence lengths of 150 or 10000, then you only need to adjust the paths, since the other values for sample sizes etc. should work reasonably well.
4. Adjust the `experiments/config/mboruns.R` to your needs. The `runs` `data.table` should list all the combinations of sequence length (`maxlen`), model type and residual block preference that you want to run. The default is to run a fully crossed design with lengths 150 and 10000. The `fidelity` `data.table` indicates for how many iterations each fidelity is evaluated. Our experiments show that 20 hour runs do not typically add much final model performance, so actual model optimization runs can be aborted before this stage is reached without much loss.
5. Adjust `experiments/config/parallelization.R` to your parallelization setting. In our experiments we used `batchtools` parallelization, which we recommend, but it is also possible to use other parallelization (or none at all). For this, use a different `parallelMap::parallelStart***()` function, or omit the call. The only value in this file required elsewhere is `MAX.PARALLEL`, which should be set to the number of parallel workers available (1 if parallelization is not used).

    It is recommended to use `batchtools`. Read the [`batchtools` vignette](https://mllg.github.io/batchtools/articles/batchtools.html) on how to set it up for your cluster environment.
6. Create pre-generated validation files (see `?readPLG()`) which are used for validation during optimization. Using pre-generated validation speeds up optimization, but creating these can take several hours. Change your working directory to `experiments/`, run an interactive R session, and source `createValidationData.R`. Then run `makePLG()` with the different values of `MAXLEN` you are using (the same values used in `experiments/config/experimentinfo.R` and `experiments/config/mboruns.R`). Since these calls individually take a long time, it is recommended that you run these in parallel manually, most easily by running two separate R sessions.

### Experiment Scripts

Your setup is now ready for experiments. Scripts are present in the `experiments/` folder to do various things. All of these can be executed by running them directly (e.g. `./runMbo.R <args..>`) or using `RScript` (e.g. `RScript runMbo.R <args..>`). However, note that all of these must be run from within the `experiments/` folder.

- **`evaluateRandomConfig.R`**: Evaluate a sampled configuration point. This can e.g. be useful to test the optimization environment. Run as `./evaluateRandomConfig.R <seed>`, with an integer value as random seed.
- **`runMbo.R`**: Initialize an MBO run (i.e. create the optimization configuration file) or run the optimization.

    For optimization, at first a database file needs to be created, by calling `./runMbo.R init <filename> <experiment-index>`. `<filename>` is the database file created (which must not already exist), `<experiment-index>` is an index into the `runs` `data.table` in `config/mboruns.R`. It is possible to append `--demo` to create a toy-problem to test the parallelization setup.

    Optimization is the evaluated by calling `runMbo.R run <filename>`. `<filename>` is the database file created using `runMBO.R init` before.
- **`resetRuns.R`**: Small helper-file: copy an MBO database file and remove all runs except for the random initialization. Thit can e.g. be used to compare different fidelity-setups or using `thoroughify.R`.
- **`thoroughify.R`**: Set the MBO surrogate optimization method to a more thorugh method. This takes more time than the default method and its benefits are not proven; this file was mostly used for internal experiments. Usage is not recommended.
- **`retryRuns.R`**: Remove runs from the MBO database that failed. This can be used if runs failed for technical reasons (e.g. the cluster killed a job that would otherwise have succeeded) but should not be used as a datapoint for the surrogate model. Will never remove runs from the initial design.

A note on mbo database files (the ones created / used by `runMbo.R`): MBO *always* saves to the *original* path of the mbo database. Renaming, moving or copying the file is therefore not possible. The only exception is when a partial copy is created using `resetRuns.R`. You should *never* create a copy of an existing mbo database and run `runMbo.R run` on this file, since the original file will then be *overwritten*.

### Run MBO Optimization

When you are confident that your setup is correct, run the experiments. For our experiments, we tried both "recurrent" and "gap" type networks, bot with and without residual blocks, and for sequence lengths of 150 and 10'000 nucleotides. The `runs` table in the `experiments/config/mboruns.R` has therefore 8 lines, and the `runMbo.R` script therefore needs to be executed for run-indices 1 to 8. This amounts to running the following commands (although distriuted to different sessions to run them simultaneously, in our case).
```sh
cd experiments
./runMbo.R init ../data/opt_1.RData 1 ; ./runMbo.R run ../data/opt_1.RData
./runMbo.R init ../data/opt_2.RData 2 ; ./runMbo.R run ../data/opt_2.RData
# [...]
./runMbo.R init ../data/opt_8.RData 8 ; ./runMbo.R run ../data/opt_8.RData
```

## Analysis of Results

The `analysis` folder contains scripts that convert the resulting optimization databases (`data/opt_<n>.RData`, in our case) more fit to analysis and extract a few results and plots.

### `collect_run_results.R`

`analysis/collect_run_results.R` is used to collect the results of all optimization runs into a single file, `data/optruns.rds`. It should be executed from the root folder. The script assumes that results are in the `data` folder with the name pattern `data/opt_<i>.RData`. `<i>` should range from 1 to the number of rows of `runs` in `experiments/config/experimentinfo.R`. Adjust the `basepath` variable in this script if it is not the case.

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

