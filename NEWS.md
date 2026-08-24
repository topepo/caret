# News for Package caret

## Changes in version 7.1.0

- The `earth` model now defines a `trim` method, so `trainControl(trim = TRUE)` reduces the size of fitted `earth` models (dropping the stored `x`, `y`, and `call`) without affecting predictions.
- Fixed three bugs in the model-diversity filters used by adaptive resampling: the concordance matrix (`cccmat`) was never populated and always returned a matrix of ones; both `cccmat` and `diffmat` scrambled their entries (breaking symmetry) for four or more models; and `filter_on_corr`/`filter_on_diff` removed the wrong models due to an index-permutation error.
- `confusionMatrix()` now gives its intended "the table must nrow = ncol" error for non-square tables, instead of an "invalid argument to unary operator" error from a mis-written check.
- `print.nullModel()` no longer swaps the model-type label; a classification null model now prints "Null Classification Model" (and regression prints "Null Regression Model").
- Fixed `svmBag$pred` so that bagging with `svmBag` works: it called an undefined `lev()` and dispatched `predict()` without loading **kernlab**; both now use the `kernlab` namespace explicitly.
- `modelLookup()` now gives its intended "Model '...' is not in the set of existing models" error for an unknown model, instead of an "object 'method' not found" error from a mistyped variable name.
- The formula interface for `icr()` now works; it previously forwarded an unused `weights` argument (and an undefined `thresh`) into `fastICA`, which errored with "unused argument (weights = ...)".
- The table methods for `sensitivity()`, `specificity()`, `posPredValue()`, `negPredValue()`, `precision()`, and `recall()` now give their intended "the table must have nrow = ncol" and "the table must the same groups in the same order" errors, instead of an "invalid argument type" error from the same mis-written `!all.equal()` check that was fixed for `confusionMatrix()`.
- `bagEarth()` and `bagFDA()` now accept a one-column matrix or data frame as the outcome. The code meant to reduce such an outcome to its first column, but the guard around it tested `is.vector()` after calling `as.vector()`, which is always true (a list is a vector), so the reduction never ran and a one-column data frame failed with "invalid type (list) for variable '.outcome'".
- Fixed two bugs in `filterVarImp()`: a data set with exactly one factor predictor errored during the factor-to-numeric conversion (a missing `drop = FALSE`), and the documented `...` options were never actually forwarded to `lm()` or `loess()` for regression outcomes.
- Adaptive resampling now says why it cannot continue when every candidate model fails in the burn-in resamples, for both the x/y and the recipe interface. The racing code was reached with nothing to compare and died inside `diffmat()` with "missing value where TRUE/FALSE needed".
- The `testing` argument of `train()` reaches the adaptive and recipe resampling workflows again. All four of their `foreach` bodies opened with `testing <- FALSE`, which overwrote the argument and made every debug block in them unreachable.
- The Bradley-Terry racer no longer prints "Loading required namespace: BradleyTerry2" the first time it runs; it now loads the package the way the rest of caret does, and says "package BradleyTerry2 is required" if it is missing.
- Fixed the Bradley-Terry racer's handling of models that win no comparisons: the result of `skunked()` was kept only when it *failed* (an inverted `inherits(x, "try-error")` test), so those models were never dropped, and a failure would have assigned the error object to the scores.
- `predict.plsda()` and `predict.bagFDA()` no longer fail when the data being predicted has repeated row names. This made `trainControl(method = "optimism_boot")` and `method = "boot_all"` unusable with `method = "pls"` (and `"bagFDA"`), since those resampling schemes score the resampled training set itself: the fit ended with "duplicate 'row.names' are not allowed" [(issue 1515)](https://github.com/topepo/caret/issues/1515).
- `safs()` now really measures each iteration's internal fitness against the first one when the fitness has a single metric. The baseline row was reduced to a vector by a missing `drop = FALSE`, so the loop that subtracts it (driven by that row's column names) had nothing to iterate over and the values were returned uncorrected.
- `varImp()` on a `safs` or `gafs` object works again when the metric is minimized (the default for regression). It passed `drop = FALSE` to `` `[<-` ``, which a data frame does not accept, so the call failed with "unused argument (drop = FALSE)".
- `gafs()` accepts a function for `pmutation` again, for a mutation probability that varies over the generations. The call passed an undefined `object`, so any function failed with "object 'object' not found"; it is now given the generation number. `print()` on the result already described this case as "Mutation probability: variable".
- `rfe()` works with a recipe and `rfeControl(method = "LOOCV")`. Choosing the subset size screened the candidates on a per-size resample count, which leave-one-out does not produce (it pools every prediction), so the call failed with "object 'Num_Resamples' not found".
- `rfe()` with a recipe now really makes columns with a `"performance var"` role available to the summary function. They were selected and passed down to the internal worker, which ignored them, so a summary function could not see them (`train()`, `gafs()` and `safs()` were unaffected).
- Giving `safs()` a starting variable subset (a vector for the `initial` element of `functions`) no longer warns "argument is not a function". The check called `formals()` on every element before allowing for that case, and the result was never used.
- `resamples()` now really checks that the models were resampled the same way. The check compared the number of distinct row counts against one using `length()`, which is always one, so it never fired; it also examined the first resample every time instead of each in turn. Models built from different resamples are now refused with "The samples indices are not equal across resamples" rather than silently compared.
- `levelplot()` on a `diff.resamples` object works when a model is named `"A"` or `"B"`. The method added columns of those names next to one column per model, and the collision left the reshaping step with nothing to work on ("replacement has 0 rows").
- `train(preProcess = options)` now really rejects a method it does not know. The check read the *names* of the argument, which an ordinary character vector does not have, so an unknown method was accepted and the fit failed later with "Something is wrong; all the ... metric values are missing".
- `rfe()` with a recipe now says "there are only N predictors remaining. The `sizes` values are inconsistent with this" when every requested subset size is larger than what the recipe produces. That message could not be reached: it was guarded by an earlier `all(sizes < 2)` test, which is `TRUE` for the empty vector such sizes reduce to, so the less helpful "less than two predictors remaining" was reported instead.
- `rfe()` works with a recipe and `rfeControl(method = "boot632")`. The apparent-error pass fits on every row, and the rows to score were taken as the complement of those, which is empty; the call failed with "arguments imply differing number of rows: 0, 1". This is the same defect that [(issue 1468)](https://github.com/topepo/caret/issues/1468) fixed for `sbf()`.
- `varImp()` on an `sbf` object works again. It built the result with `as.data.frame()` on a table, which yields a column of variable names and a column of counts, and then renamed only the first of them "Overall" -- so the sort was applied to the names and failed with "invalid argument to unary operator".
- `sbf()` works with `sbfControl(method = "LOOCV")` and with `method = "boot632"`. Both passed their arguments to the internal worker positionally, so the control object landed in the `testPerf` slot and the control fell back to its own default -- which self-references, so the calls failed with "promise already under evaluation: recursive default argument reference or earlier problems?". ([(issue 1468)](https://github.com/topepo/caret/issues/1468) fixed the same failure for the recipe interface's `boot632` path.)
- `rfe()` with a recipe that gives a column the `"performance var"` role works for any number of rows. The check that discovers the metric names builds a dry run of at most ten rows and bound the performance columns to it whole, so the call failed with "arguments imply differing number of rows" unless the data happened to have a multiple of ten rows (where `cbind()` recycled instead). `safs()` and `gafs()` already sampled those rows down to match.
- `train()` builds a model's default tuning grid from the pre-processed predictors for single-parameter models too, and honours `preProcOptions = list(pcaComp = )` when it does. Only models with two or more parameters got this treatment, so `train(method = "rf", preProcess = "pca")` chose `mtry` values from the original predictor count and **randomForest** then warned "invalid mtry: reset to within valid range".
- Removed an unreachable branch in `rfe()` that tried to build a resampled confusion matrix from the aggregated performance table, which never carries the per-cell counts (they are kept in the `resample` element, which is where `confusionMatrix()` reads them). The `resampledCM` element of an `rfe` object was always empty and still is.
- Removed the unused `predict.PLS()` method. It was left over from an internal `PLS()` function that caret dropped before version 6.0: nothing in the package creates objects of class `"PLS"`, the fields it reads (`isRegression`, `yLevels`, `bestIter$.ncomp`) exist nowhere else, and **pls** models are of class `"mvr"`, so no object could ever dispatch to it.
- Removed the unused `print.bmars()` method, for the same reason: nothing in the package creates objects of class `"bmars"`, so no object could dispatch to it.
- Removed the internal, unexported `additivePlot()` function, which nothing in the package called.
- Removed the internal, unexported `altTrainWorkflow()` function, an identity function that nothing in the package called.
- `resampleHist()` now really checks for the resampled results it needs. The check used `object$resample`, which partially matches the `resampledCM` element that classification fits carry, so a fit without resampled results got past it and failed later instead of giving the intended "No resample values were found" message.
- The documentation for `summary.bagEarth()` named a `bmarsCall` element in its return value; the function returns `bagEarthCall`.
- `checkInstall()` can install Bioconductor packages again. It sourced `biocLite.R`, which Bioconductor retired with release 3.8, and now uses [**BiocManager**](https://CRAN.R-project.org/package=BiocManager) instead (installing it first if needed).
- `update()` on a `train` object from caret 5.17-7 or earlier now looks the model type up by its exact name. It used regular-expression matching, so updating an `"lm"` object attached **arm**'s `"bayesglm"` information instead.
- `update()` on a model that used `preProcess` no longer warns that pre-processing methods "were eliminated". It passed the fitted pre-processing object's method list back to `preProcess()`, empty entries included.
- `update()` now re-fits with the original case weights when the model was created with the formula interface. The formula method re-saved `trainingData` without the `.weights` column that the x/y interface keeps, so the updated model was fit unweighted.
- `predict()` on a `preProcess` object from an older caret version that used `method = "invHyperbolicSine"` no longer fails with "$ operator is invalid for atomic vectors". The internal conversion of the old character vector of methods read the transformation's columns from that vector as if it were already a list.
- `print()` on a `preProcess` object that applies the spatial sign to independent components (`method = list(ica = ..., spatialSign = "_IC_")`) no longer fails with "replacement has length zero". It counted the components with the element that holds the number of *principal* components, which such an object does not have.
- `probFunction()` and its recipe counterpart now really order the class-probability columns by the observed class levels for models whose `prob` function returns a matrix. In both, the reordered result was assigned to a misspelled variable (`classprob`) and discarded, so the original column order was returned instead.
- `preProcess(method = "corr")` now really does add the zero-variance filter it promises. The check that adds it ran after `method` had been converted from a character vector to a named list, so it never fired, and a zero-variance predictor made the correlation matrix undefined: the call failed with "missing value where TRUE/FALSE needed". If that filter leaves a single predictor there is nothing left to correlate, which is now handled rather than passed to `findCorrelation()` (an "only one variable given" error).
- Adaptive resampling now attributes the results of the resamples it finishes up with (`adaptive = list(complete = TRUE)`) to the right tuning parameters when the model has sub-models. Those resamples are only run once a single candidate is left, but its sub-models were looked up in the grid the race started from, so predictions for candidates that had already been eliminated were scored and then labelled with the surviving parameters ("row names were found from a short variable and have been discarded"). Both the x/y and the recipe interface were affected, and the sub-model handling that pass carried -- unreachable, since the race stops at one candidate -- has been removed [(issue 1533)](https://github.com/topepo/caret/issues/1533).
- Adaptive resampling with a recipe and `trainControl(savePredictions = "all")` now records which candidate made each prediction for the resamples the race starts from. That phase was the only one that did not merge the tuning parameters into the predictions it saved, so those rows came back with `NA` for every parameter. The x/y interface was unaffected [(issue 1534)](https://github.com/topepo/caret/issues/1534).
- Adaptive resampling with a recipe now keeps the resampled confusion-matrix counts for up to fifty classes throughout. The resamples the race starts from stopped at five (a mistyped threshold; the seven other places that gate those counts use fifty), so with six or more classes `resampledCM` held `NA` counts for them [(issue 1534)](https://github.com/topepo/caret/issues/1534).
- Removed the exported `resampleWrapper()` function, which could not run. It passed `data` to `createModel()`, a signature that has not existed in this repository's history -- `createModel()` has always taken `x`/`y` -- so every call failed with "argument "x" is missing, with no default". Nothing in the package called it, and no reverse dependency mentions it [(issue 1518)](https://github.com/topepo/caret/issues/1518).

## Changes in version 7.0-1

- CRAN mandated update.
- caret will be 20 years old in March of 2026. The package is currently in maintenance mode; the author will fix bugs and make CRAN releases as needed, but there will not be any major features in the package. It will stay on CRAN long-term; it's not going away.

## Changes in version 6.0-94

- Bug fix in how some S3 signatures were designed (for R-devel).
- Adrián Panella fixed a bug with \`method = "svmRadial" that occured when the SVM probability model failed [(issue 1327)](https://github.com/topepo/caret/issues/1327)
- Theodore Pak fixed a bug in glmnet prediction when sparse matrices are used [(issue 1315)](https://github.com/topepo/caret/issues/1315)
- Carlos Cinelli fixed two bugs (one for GAMs and another for ranger models) where an error would occur when using `trainControl(method = "none")` [(issue 1307)](https://github.com/topepo/caret/issues/1307) [(issue 1308)](https://github.com/topepo/caret/issues/1308)

## Changes in version 6.0-93

- CRAN required changes for legacy S typedef Sint.
- Xabriel J. Collazo Mojica added check for class probabilities when PR curves are requested [(issue 1274)](https://github.com/topepo/caret/issues/1274).

## Changes in version 6.0-92

- Small maintenance release with required C changes for R 4.2.

## Changes in version 6.0-91

- Small maintenance release with required changes for R 4.2.

## Changes in version 6.0-90

- A `ptype` element was added to `train` objects that records the trianing set predictor columns and their types using a zero-row slice.
- Updated the internal object containing the subsampling information. The old package dependencies were being used.

## Changes in version 6.0-89

- SMOTE subsampling is now computed via the [**themis**](https://CRAN.R-project.org/package=themis) package [(issue 1226)](https://github.com/topepo/caret/issues/1226).
- For SA feature selection, a better warning is given when there are too few iterations for computing differences [(issue 1247)](https://github.com/topepo/caret/issues/1247).
- Better error message when pacakges are missing [(issue 1246)](https://github.com/topepo/caret/issues/1246).
- [**e1071**](https://CRAN.R-project.org/package=e1071) was promoted from Suggests to Imports [(issue 1238)](https://github.com/topepo/caret/issues/1238).

## Changes in version 6.0-88

- Fixed cases where the "corr" filter was not run in `preProcess()`.
- Prediction type bug for Poisson glm model was fixed [(issue 1231)](https://github.com/topepo/caret/issues/1231).
- Fixed a `PreProcess()` bug related to a single PCA component [(issue 1181)](https://github.com/topepo/caret/issues/1181).
- Fixed random forest bugs related to `rfe()` [(issue #1077)](https://github.com/topepo/caret/issues/#1077), [(issue 1062)](https://github.com/topepo/caret/issues/1062).
- RuleFit was added via the [**pre**](https://CRAN.R-project.org/package=pre) package [(issue 1218)](https://github.com/topepo/caret/issues/1218).
- Bugs were fixed where MAE was not treated as a minimization metric [(issue 1224)](https://github.com/topepo/caret/issues/1224).

## Changes in version 6.0-87

- The [**ordinalNet**](https://CRAN.R-project.org/package=ordinalNet) model was given an additional tuning parameter `modeltype`.

## Changes in version 6.0-86

- Small release for `stringsAsFactors = TRUE` in R-4.0

## Changes in version 6.0-85

- Internal changes required by r-devel for new matrix class structure.
- Michael Mayer contributed a faster version of `groupKFold()`. [(issue 1108)](https://github.com/topepo/caret/issues/1108)
- A typo in a variable name was fixed. [(issue 1087)](https://github.com/topepo/caret/issues/1087)
- Removed some warnings related to contrasts.
- Temporarily moved ROC calculations back to the [**pROC**](https://CRAN.R-project.org/package=pROC) package related to `JackStat/ModelMetrics#30`. [(issue 1105)](https://github.com/topepo/caret/issues/1105)

## Changes in version 6.0-84

- Another new version was related to character encodings.

## Changes in version 6.0-83

- A new version was requested by CRAN since en dashes were used in the documentation.
- A bug was fixed where, for some recipes that involve class imbalance sampling, the resampling indicies were computed incorrectly [(issue 1030)](https://github.com/topepo/caret/issues/1030).
- `train` now removes duplicate models in the tuning grid. Duplicates could occur for models with discrete parameters.

## Changes in version 6.0-82

- Immediate and required updates related to the different behavior of `sample` in R >= 3.6.
- `sbf`, `gafs`, and `safs` now accept recipes as inputs. A few sections of documentation were added to the [bookdown site](https://topepo.github.io/caret/)
- A bug was fixed in simulated annealing feature selection where the number of variables perturbed was based on the total number of variables instead of the more appropriate number of variables in the current subset.
- Convenience functions `ggplot.gafs` and `ggplot.safs` were added.
- `learning_curve_dat` now has a real name.
- The `earth` and `ctree` models were udpdate to fix bugs [(issue 1022)](https://github.com/topepo/caret/issues/1022)[(issue 1018)](https://github.com/topepo/caret/issues/1018).
- When a model has the same value for its resamples, `plot.resamples` and `ggplot.resamples` now produce an estimate and missing values for the intervals (instead of failing) [(issue 1007)](https://github.com/topepo/caret/issues/1007)

## Changes in version 6.0-81

- The `blackboost` code gained a dependency on [**partykit**](https://CRAN.R-project.org/package=partykit) due to changes in [**mboost**](https://CRAN.R-project.org/package=mboost).
- The internals were updated to work with the latest version of the [**recipes**](https://CRAN.R-project.org/package=recipes) package.
- Jason Muhlenkamp added better error messages for misspecified tuning parameters [(issue 956)](https://github.com/topepo/caret/issues/956).
- Two bugs in random forests with RFE were fixed in [(issue 942)](https://github.com/topepo/caret/issues/942).
- When correlation filters are used in `preProcess`, constant (i.e. zero-variance) columns are first removed to avoid errors [(issue 966)](https://github.com/topepo/caret/issues/966).
- A bug was fixed when `train` models using weights were updated [(issue 935)](https://github.com/topepo/caret/issues/935).
- Benjamin Allévius added more statistics to the output of `thresholder` [(issue 938)](https://github.com/topepo/caret/issues/938).
- A bug was fixed that occurred when `indexFinal` was used where the recipe that was saved was created using the entire training set [(issue 928)](https://github.com/topepo/caret/issues/928).

## Changes in version 6.0-80

- Two bugs associated with `varImp` in `lm` [(issue 858)](https://github.com/topepo/caret/issues/858) and in `bartMachine` were fixed by `hadjipantelis`.
- SMOTE sampling now works with tibbles [(issue 875)](https://github.com/topepo/caret/issues/875)
- [**rpart**](https://CRAN.R-project.org/package=rpart) now a dependency due to new CRAN policies.
- Added a `ggplot` method for `resamples` that produces confidence intervals.
- `hadjipantelis` added some fixes for `mxnet` models [(issue 887)](https://github.com/topepo/caret/issues/887).

## Changes in version 6.0-79

- `keras` and `mxnet` models have better initialization of parameters [pr 765](https://github.com/topepo/caret/pull/765/files)
- The range preprocessing method can scale the data to an arbitrary range. Thanks to Sergey Korop.
- The spatial sign transformation will now operation on all non-missing predictors. Thanks to Markus Peter Auer [(issue 789)](https://github.com/topepo/caret/issues/789).
- A variety of small changes were made to work with the new version of the [**gam**](https://CRAN.R-project.org/package=gam) package [(issue 828)](https://github.com/topepo/caret/issues/828).
- The package vignette was changed to HTML.
- A big was fixed for computing variable importance scores with the various PLS methods [(issue 848)](https://github.com/topepo/caret/issues/848).
- Fixed a `drop = FALSE` bug that occurred when computing class probabilities [(issue 849)](https://github.com/topepo/caret/issues/849).
- An issue with predicting probabilities with `multinom` and one observation was fixed [(issue 827)](https://github.com/topepo/caret/issues/827).
- A bug in the threshold calculation for choosing the number of PCA components was resolved [(issue 825)](https://github.com/topepo/caret/issues/825).
- Models `mlpML` and `mlpWeightDecayML` now ignore layers with zero units. For example, if the number of layers was specified to be `c(5, 0, 3)` a warning is issued and the architecture is converted to `c(5, 3)` [(issue 829)](https://github.com/topepo/caret/issues/829).
- `svmLinearWeights2` and `svmLinear3` may have chosen the incorrect SVM loss function. This was found by Dirk Neumann [(issue 826)](https://github.com/topepo/caret/issues/826)
- [**bnclassify**](https://CRAN.R-project.org/package=bnclassify) models `awtan` and `awnb` were updated since they previously used deprecated functions. All [**bnclassify**](https://CRAN.R-project.org/package=bnclassify) models now require version 0.3.3 of that package or greater [(issue 815)](https://github.com/topepo/caret/issues/815).
- `confusionMatrix.default` not requires `data` and `reference` to be factors and will throw an error otherwise. Previously, the vectors were converted to factors but this resulted in too many bug reports and misuse.
- `xyplot.resample` did not pass the dots to the underlying plot function [(issue 853)](https://github.com/topepo/caret/issues/853).
- A bug with model `xgbDART` was fixed by `hadjipantelis`.

## Changes in version 6.0-78

- A number of changes were made to the underlying model code to repair problems caused by the previous version. In essence, unless the modeling package was formally loaded, the model code would fail in some cases. In the vast majority of cases, `train` will not load the package (but will load the namespace). There are some exceptions where this is not possible, including `bam`, `earth`, `gam`, `gamLoess`, `gamSpline`, `logicBag`, `ORFlog`, `ORFpls`, `ORFridge`, `ORFsvm`, `plsRglm`, `RSimca`, `rrlda`, `spikeslab`, and others. These are noted in `?models` and in the model code itself. The regression tests now catch these issues.
- The option to control the minimum node size to models `ranger` and `Rborist` was added by `hadjipantelis` [(issue 732)](https://github.com/topepo/caret/issues/732).
- The rule-based model `GFS.GCCL` was removed from the model library.
- A bug was fixed affecting models using the **sparsediscrim** package (i.e. `dda` and `rlda`) where the class probability values were reversed. [(issue 761)](https://github.com/topepo/caret/issues/761).
- The `keras` models now clear the session prior to each model fit to avoid problems. Also, on the last fit, the model is serialized so that it can be used between sessions. The `predict` code will automatically undo this encoding so that the user does not have to manually intervene.
- A bug in `twoClassSummary` was fixed that prevents failure when the class level includes "y" [(issue 770)](https://github.com/topepo/caret/issues/770).
- The `preProcess` function can now scale variables to a range where the user can set the high and low values [(issue 730)](https://github.com/topepo/caret/issues/730). Thanks to Sergey Korop.
- Erwan Le Pennec fixed some issues when `train` was run using some parallel processing backends (e.g. `doFuture` and `doAzureParallel`) [(issue 748)](https://github.com/topepo/caret/issues/748).
- Waleed Muhanna found and fixed a bug in `twoClassSim` when irrelevant variables were generated. [(issue 744)](https://github.com/topepo/caret/issues/744).
- `hadjipantelis` added the DART model (aka "Dropouts meet Multiple Additive Regression Trees") with the model code `xgbDART` [(issue 742)](https://github.com/topepo/caret/issues/742).
- Vadim Khotilovich updated `predict.dummyVars` to run faster with large datasets with many factors [(issue 727)](https://github.com/topepo/caret/issues/727).
- `spatialSign` now has the option of removing missing data prior to computing the norm [(issue 789)](https://github.com/topepo/caret/issues/789).
- The various [**earth**](https://CRAN.R-project.org/package=earth) models have been updated to work with recent versions of that package, including multi-class `glm` models [(issue 779)](https://github.com/topepo/caret/issues/779).

## Changes in version 6.0-77

- Two neural network models (containing up to three hidden layers) using `mxnet` were added; `mxnet` (optimiser: SGD) and `mxnetAdam` (optimiser: ADAM).
- A new method was added for `train` so that [**recipes**](https://CRAN.R-project.org/package=recipes) can be used to specify the model terms and preprocessing. Alexis Sardá provided a great deal of help converting the bootstrap optimism code to the new workflows. A new chapter was added to the package website related to recipes.
- The Yeo-Johnson transformation parameter estimation code was rewritten and not longer requires the `car` package.
- The leave-one-out cross-validation workflow for `train` has been harmonized with the other resampling methods in terms of fault tolerance and prediction trimming.
- `train` now uses different random numbers to make resamples. Previously, setting the seed prior to calling `train` should result in getting the same resamples. However, if `train` loaded or imported a namespace from another package, and that startup process used random numbers, it could lead to different random numbers being used. See [(issue 452)](https://github.com/topepo/caret/issues/452) for details. Now, `train` gets a separate (and more reproducible) seed that will be used to generate the resamples. However, this may effect random number reproducibility between this version and previous versions. Otherwise, this change should increase the reproducibility of results.
- Erwan Le Pennec conducted the herculean task of modifying all of the model code to call by namespace (instead of fully loading each required package). This should reduce naming conflicts [(issue 701)](https://github.com/topepo/caret/issues/701).
- MAE was added as output metric for regression tasks through `postResample` and `defaultSummary` by hadjipantelis. The function is now exposed to the users. [(issue 657)](https://github.com/topepo/caret/issues/657).
- More average precision/recall statistics were added to `multiClassSummary` [(issue 697)](https://github.com/topepo/caret/issues/697).
- The package website code was updated to use version 4 of the D3 JS library and now uses [**heatmaply**](https://CRAN.R-project.org/package=heatmaply) to make the interactive heatmap.
- Added a `ggplot` method for lift objects (and fixed a bug in the `lattice` version of the code) for [(issue 656)](https://github.com/topepo/caret/issues/656).
- Vadim Khotilovich made a change to speed up `predict.dummyVars` [(issue 727)](https://github.com/topepo/caret/issues/727).
- The model code for `ordinalNet` was updated for recent changes to that package.
- `oblique.tree` was removed from the model library.
- The default grid generation for rotation forest models now provides better values of `K`.
- The parameter ranges for `AdaBag` and `AdaBoost.M1` were changed; the number of iterations in the default grids have been lowered.
- Switched to non-formula interface in ranger. Also, another tuning parameter was added to ranger (`splitrule`) that can be used to change the splitting procedure and includes extremely randomized trees. This requires version 0.8.0 of the [**ranger**](https://CRAN.R-project.org/package=ranger) package. [(issue 581)](https://github.com/topepo/caret/issues/581)
- A simple "null model" was added. For classification, it predictors using the most prevalent level and, for regression, fits an intercept only model. [(issue 694)](https://github.com/topepo/caret/issues/694)
- A function `thresholder` was added to analyze the resample results for two class problems to choose an appropriate probability cutoff a la <https://topepo.github.io/caret//using-your-own-model-in-train.html#Illustration5> [(issue 224)](https://github.com/topepo/caret/issues/224).
- Two neural network models (containing a single hidden layers) using `tensorflow`/`keras` were added. `mlpKerasDecay` uses standard weight decay while `mlpKerasDropout` uses dropout for regularization. Both use RMSProp optimizer and have a lot of tuning parameters. Two additional models, `mlpKerasDecayCost` and `mlpKerasDropoutCost`, are classification only and perform cost-sensitive learning. Note that these models will not run in parallel using [**caret**](https://CRAN.R-project.org/package=caret)'s parallelism and also will not give reproducible results from run-to-run (see <https://github.com/rstudio/keras3/issues/42>).
- The range for one parameter (`gamma`) was modified in the `mlpSGD` model code.
- A bug in classification models with all missing predictions was fixed (found by andzandz11). [(issue 684)](https://github.com/topepo/caret/issues/684)
- A bug preventing preprocessing to work properly when the preprocessing transformations are related to individual columns only fixed by Mateusz Kobos in [(issue 679)](https://github.com/topepo/caret/issues/679).
- A prediction bug in `glm.nb` that was found by jpclemens0 was fixed [(issue 688)](https://github.com/topepo/caret/issues/688).
- A bug was fixed in Self-Organizing Maps via `xyf` for regression models.
- A bug was fixed in `rpartCost` related to how the tuning parameter grid was processed.
- A bug in negative-binomial GLM models (found by jpclemens0) was fixed [(issue 688)](https://github.com/topepo/caret/issues/688).
- In `trainControl`, if `repeats` is used on methods other than `"repeatedcv"` or `"adaptive_cv"`, a warning is issued. Also, for method other than these two, a new default (`NA`) is given to `repeats`. [(issue 720)](https://github.com/topepo/caret/issues/720).
- `rfFuncs` now computes importance on the first and last model fit. [(issue 723)](https://github.com/topepo/caret/issues/723)

## Changes in version 6.0-76

- Monotone multi-layer perceptron neural network models from the [**monmlp**](https://CRAN.R-project.org/package=monmlp) package were added [(issue 489)](https://github.com/topepo/caret/issues/489).
- A new resampling function (`groupKFold`) was added [(issue 540)](https://github.com/topepo/caret/issues/540).
- The bootstrap optimism estimate was added by Alexis Sarda [(issue 544)](https://github.com/topepo/caret/issues/544).
- Bugs in `glm`, `glm.nb`, and `lm` variable importance methods that occur when a single variable is in the model [(issue 543)](https://github.com/topepo/caret/issues/543).
- A bug in `filterVarImp` was fixed where the ROC curve AUC could be much less than 0.50 because the directionality of the predictor was not taken into account. This will artificially increase the importance of some non-informative predictors. However, the bug might report the AUC for an important predictor to be 0.20 instead of 0.80 [(issue 565)](https://github.com/topepo/caret/issues/565).
- `multiClassSummary` now reports the average F score [(issue 566)](https://github.com/topepo/caret/issues/566).
- The `RMSE` and `R2` are now (re)exposed to the users [(issue 563)](https://github.com/topepo/caret/issues/563).
- A [**caret**](https://CRAN.R-project.org/package=caret) bug was discovered by Jiebiao Wang where `glmboost`, `gamboost`, and `blackboost` models incorrectly reported the class probabilities [(issue 560)](https://github.com/topepo/caret/issues/560).
- Training data weights support was added to `xgbTree` model by schistyakov.
- Regularized logistic regression through Liblinear (`LiblineaR::LiblineaR`) using L1 or L2 regularization were added by hadjipantelis.
- A bug related to the ordering of axes labels in the heatmap plot of training results was fixed by Mateusz Dziedzic in [(issue 620)](https://github.com/topepo/caret/issues/620).
- A variable importance method for model averaged neural networks was added.
- More logic was added so that the `predict` method behaves well when a variable is subtracted from a model formula from [(issue 574)](https://github.com/topepo/caret/issues/574).
- More documentation was added for the `class2ind` function ([(issue 592)](https://github.com/topepo/caret/issues/592)).
- Fixed the formatting of the design matrices in the `dummyVars` man file.
- A note was added to `?trainControl` about using custom resampling methods ([(issue 584)](https://github.com/topepo/caret/issues/584)).
- A bug was fixed related to SMOTE and ROSE sampling with one predictor ([(issue 612)](https://github.com/topepo/caret/issues/612)).
- Due to changes in the [**kohonen**](https://CRAN.R-project.org/package=kohonen) package, the `bdk` model is no longer available and the code behind the `xyf` model has changes substantially (including the tuning parameters). Also, when using `xyf`, a check is conducted to make sure that a recent version of the [**kohonen**](https://CRAN.R-project.org/package=kohonen) package is being used.
- Changes to `xgbTree` and `xgbLinear` to help with sparse matrix inputs for [(issue 593)](https://github.com/topepo/caret/issues/593). Sparse matrices are not allowed when preprocessing or subsampling are used.
- Several PLS models were using the classical orthogonal scores algorithm when discriminant analysis was conducted (despite using `simpls`, `widekernelpls`, or `kernelpls`). Now, the PLSDA model estimation method is consistent with the method requested ([(issue 610)](https://github.com/topepo/caret/issues/610)).
- Added Multi-Step Adaptive MCP-Net (`method = "msaenet"`) for [(issue 561)](https://github.com/topepo/caret/issues/561).
- The variable importance score for linear regression was modified so that missing values in the coefficients are converted to zero.
- In `train`, `x` is now required to have column names.

## Changes in version 6.0-73

- Negative binomial generalized linear models (`MASS:::glm.nb`) were added [(issue 476)](https://github.com/topepo/caret/issues/476)
- `mnLogLoss` now returns a named vector ([(issue 514)](https://github.com/topepo/caret/issues/514), bug found by Jay Qi)
- A bunch of method/class related bugs induced by the previous version were fixed.

## Changes in version 6.0-72

- The inverse hyperbolic sine transformation was added to `preProcess` [(issue 56)](https://github.com/topepo/caret/issues/56)
- Tyler Hunt moved the ROC code from the [**pROC**](https://CRAN.R-project.org/package=pROC) package to the [**ModelMetrics**](https://CRAN.R-project.org/package=ModelMetrics) package which should make the computations more efficient [(issue 482)](https://github.com/topepo/caret/issues/482).
- `train` does a better job of respecting the original format of the input data [(issue 474)](https://github.com/topepo/caret/issues/474)
- A bug in `bdk` and `xyf` models was fixed where the appropriate number of parameter combinations are tested during random search.
- A bug in `rfe` was fixed related to neural networks found by david-machinelearning [(issue 485)](https://github.com/topepo/caret/issues/485)
- Neural networks via stochastic gradient descent (`method = "mlpSGD"`) was adapted for classification and a variable importance calculation was added.
- [h2o](https://h2o.ai/) versions of glmnet and gradient boosting machines were added with methods `"glmnet_h2o"` and `"gbm_h2o"`. These methods are not currently optimized. [(issue 283)](https://github.com/topepo/caret/issues/283)
- The fuzzy rule-based models (`WM`, `SLAVE`, `SBC`, `HYFIS`, `GFS.THRIFT`, `GFS.LT.RS`, `GFS.GCCL`, `GFS.FR.MOGUL`, `FS.HGD`, `FRBCS.W`, `FRBCS.CHI`, `FIR.DM`, `FH.GBML`, `DENFIS`, and `ANFIS`) were modified so that the user can pass in the predictor ranges using the `range.data` argument to those functions. [(issue 498)](https://github.com/topepo/caret/issues/498)
- A variable importance method was added for boosted generalized linear models [(issue 493)](https://github.com/topepo/caret/issues/493)
- `preProcess` now has an option to filter out highly correlated predictors.
- `trainControl` now has additional options to modify the parameters of near-zero variance and correlation filters. See the `preProcOptions` argument.
- The `rotationForest` and `rotationForestCp` methods were revised to evaluate only *feasible* values of the parameter `K` (the number of variable subsets). The underlying `rotationForest` function reduces this parameter until values of `K` divides evenly into the number of parameters.
- The `skip` option from `createTimeSlices` was added to `trainControl` [(issue 491)](https://github.com/topepo/caret/issues/491)
- `xgb.train`'s option `subsample` was added to the `xgbTree` model [(issue 464)](https://github.com/topepo/caret/issues/464)

## Changes in version 6.0-71

- Precision, recall, and F measure functions were added along with one called `prSummary` that is analogous to `twoClassSummary`. Also, `confusionMatrix` gains an argument called `mode` that dictates what output is shown.
- schistyakov added additional tuning parameters to the robust linear model code [(issue 454)](https://github.com/topepo/caret/issues/454). Also for `rlm` and `lm` schistyakov added the ability to tune over the intercept/no intercept model.
- Generalized additive models for very large datasets (`bam` in [**mgcv**](https://CRAN.R-project.org/package=mgcv)) was added [(issue 453)](https://github.com/topepo/caret/issues/453)
- Two more linear SVM models were added from the [**LiblineaR**](https://CRAN.R-project.org/package=LiblineaR) package with model codes `svmLinear3` and `svmLinearWeights2` ([(issue 441)](https://github.com/topepo/caret/issues/441))
- The `tau` parameter was added to all of the least square SVM models ([(issue 415)](https://github.com/topepo/caret/issues/415))
- A new data set (called `scat`) on animal droppings was added.
- A significant bug was fixed where the internals of how R creates a model matrix was ignoring `na.action` when the default was set to `na.fail` [(issue 461)](https://github.com/topepo/caret/issues/461). This means that `train` will now immediately fail if there are any missing data. To use imputation, use `na.action = na.pass` and the imputation method of your choice in the `preProcess` argument. Also, a warning is issued if the user asks for imputation but uses the formula method and excludes missing data in `na.action`

## Changes in version 6.0-70

- Based on a comment by Alexis Sarda, `method = "ctree2"` does not fix `mincriterion = 0` and tunes over this parameter. For a fixed depth, `mincriterion` can further prune the tree [(issue 409)](https://github.com/topepo/caret/issues/409).
- A bug in KNN imputation was fixed (found by saviola777) that occurred when a factor predictor was in the data set [(issue 404)](https://github.com/topepo/caret/issues/404).
- Infrastructure changes were made so that `train` tries harder to respect the original class of the outcome. For example, if an ordered factor is used as the outcome with a modeling function that treats is as an unordered factor, the model still produces an ordered factor during prediction.
- The `ranger` code now allows for case weights [(issue 414)](https://github.com/topepo/caret/issues/414).
- `twoClassSim` now has an option to compute ordered factors.
- High-dimensional regularized discriminant analysis and, regularized linear discriminant analysis, and several variants of diagonal discriminant analysis from the [**sparsediscrim**](https://CRAN.R-project.org/package=sparsediscrim) package were added (`method = "hdrda"`, `method = "rlda"`, and `method = "dda"`, respectively) [(issue 313)](https://github.com/topepo/caret/issues/313).
- A neural network regression model optimized by stochastic gradient decent from the [**FCNN4R**](https://CRAN.R-project.org/package=FCNN4R) package was added. The model code is `mlpSGD`.
- Several models for ordinal outcomes were added: `rpartScore` (from the [**rpartScore**](https://CRAN.R-project.org/package=rpartScore) package), `ordinalNet` ([**ordinalNet**](https://CRAN.R-project.org/package=ordinalNet)), `vglmAdjCat` ([**VGAM**](https://CRAN.R-project.org/package=VGAM)), `vglmContRatio` ([**VGAM**](https://CRAN.R-project.org/package=VGAM)), and `vglmCumulative` ([**VGAM**](https://CRAN.R-project.org/package=VGAM)). Note that, for models that load [**VGAM**](https://CRAN.R-project.org/package=VGAM), there is a conflict such that the `predictors` class code from [**caret**](https://CRAN.R-project.org/package=caret) is masked. To use that method, you can use `caret:::predictors.train()` instead of `predictors()`.
- Another high performance random forest package ([**Rborist**](https://CRAN.R-project.org/package=Rborist)) was exposed through [**caret**](https://CRAN.R-project.org/package=caret). The model code is `method = "Rborist"` [(issue 418)](https://github.com/topepo/caret/issues/418)
- Xavier Robin fixed a bug related to the area under the ROC curve in [(issue 431)](https://github.com/topepo/caret/issues/431).
- A bug in `print.train` was fixed when LOO CV was used [(issue 435)](https://github.com/topepo/caret/issues/435)
- With RFE, a better error message drafted by mikekaminsky is printed when the number of importance measures is off [(issue 424)](https://github.com/topepo/caret/issues/424)
- Another bug was fixed in estimating the prediction time when the formula method was used [(issue 420)](https://github.com/topepo/caret/issues/420).
- A linear SVM model was added that uses class weights.
- The linear SVM model using the [**e1071**](https://CRAN.R-project.org/package=e1071) package (`method = "svmLinear2"`) had the `gamma` parameter for the RBF kernel removed.
- Xavier Robin committed changes to make sure that the area under the ROC is accurately estimated [(issue 431)](https://github.com/topepo/caret/issues/431)

## Changes in version 6.0-68

- `print.train` no longer shows the standard deviation of the resampled values unless the new option is used (`print.train(, showSD = TRUE)`). When shown, they are within parentheses (e.g. "4.24 (0.493)").
- An adjustment the innards of adaptive resampling was changed so that the test for linear dependencies is more stringent.
- A bug in the bootstrap 632 estimate was found and fixed by Alexis Sarda [(issue 349)](https://github.com/topepo/caret/issues/349) [(issue 353)](https://github.com/topepo/caret/issues/353).
- The `cforest` module's `oob` element was modified based on another bug found by Alexis Sarda [(issue 351)](https://github.com/topepo/caret/issues/351).
- The methods for `bagEarth`, `bagEarthGCV`, `bagFDA`, `bagFDAGCV`, `earth`, `fda`, and `gcvEarth` models have been updates so that case-weights can be used.
- The `rda` module contained a bug found by Eric Czech [(issue 369)](https://github.com/topepo/caret/issues/369).
- A bug was fixed for printing out the resampling details with LGOCV found by github user zsharpm [(issue 366)](https://github.com/topepo/caret/issues/366)
- A new data set was added (`data(Sacramento)`) with sale prices of homes.
- Another adaboost algorithm (`method = "adaboost"` from the [**fastAdaboost**](https://CRAN.R-project.org/package=fastAdaboost) package) was added [(issue 284)](https://github.com/topepo/caret/issues/284).
- Yet another boosting algorithm (`method = "deepboost"` from the [**deepboost**](https://CRAN.R-project.org/package=deepboost) package) was added [(issue 388)](https://github.com/topepo/caret/issues/388).
- Alexis Sarda made changes to the confusion matrix code for `train`, `rfe`, and `sbf` objects that more rationally normalizes the resampled tables [(issue 355)](https://github.com/topepo/caret/issues/355).
- A bug in how [**RSNNS**](https://CRAN.R-project.org/package=RSNNS) perceptron models were tuned (found by github user smlek) was fixed [(issue 392)](https://github.com/topepo/caret/issues/392).
- A bug in computing the bootstrap 632 estimate was fixed (found by Stu) [(issue 382)](https://github.com/topepo/caret/issues/382).
- John Johnson contributed an update to `xgbLinear` [(issue 372)](https://github.com/topepo/caret/issues/372).
- Resampled confusion matrices are not automatically computed when there are 50 or more classes due to the storage requirements ([(issue 356)](https://github.com/topepo/caret/issues/356)). However, the relevant functions have been updated to use the out-of-sample predictions instead (when the user asks for them to be returned by the function).
- Some changes were made to `predict.train` to error trap (and fix) cases when predictions are requested without referencing a `newdata` object [(issue 347)](https://github.com/topepo/caret/issues/347).
- Github user pverspeelt identified a bug in our model code for `glmboost` (and `gamboost`) related to the `mstop` function modifying the model object in memory. It was fixed [(issue 396)](https://github.com/topepo/caret/issues/396).
- For [(issue 346)](https://github.com/topepo/caret/issues/346), an option to select which samples are used to fit the final model, called `indexFinal`, was added to `trainControl`.
- For issue [(issue 390)](https://github.com/topepo/caret/issues/390) found by JanLauGe, a bug was fixed in `dummyVars` related to the names of the resulting data set.
- Models `rknn` and `rknnBel` were removed since their package is no longer on CRAN.

## Changes in version 6.0-66

- Model averaged naive Bayes (`method = "manb"`) from the [**bnclassify**](https://CRAN.R-project.org/package=bnclassify) package was added.
- `blackboost` was updated to work with outcomes with 3+ classes.
- A new model `rpart1SE` was added. This has no tuning parameters and resamples the internal [**rpart**](https://CRAN.R-project.org/package=rpart) procdure of pruning using the one standard error method.
- Another model (`svmRadialSigma`) tunes over the cost parameter and the RBF kernel parameter sigma. In the latter case, using `tuneLength` will, at most, evaluate six values of the kernel parameter. This enables a broad search over the cost parameter and a relatively narrow search over `sigma`.
- Additional model tags for "Accepts Case Weights", "Two Class Only", "Handle Missing Predictor Data", "Categorical Predictors Only", and "Binary Predictors Only" were added. In some cases, a new model element called "notes" was added to the model code.
- A pre-processing method called "conditionalX" was added that eliminates predictors where the conditional distribution (X|Y) for that predictor has a single value. See the `checkConditionalX` function for details. This is only used for classification. [(issue 334)](https://github.com/topepo/caret/issues/334)
- A bug in the naive Bayes prediction code was found by github user pverspeelt and was fixed. [(issue 345)](https://github.com/topepo/caret/issues/345)
- Josh Brady (doublej2) found and fixed an issue with `DummyVars` [(issue 344)](https://github.com/topepo/caret/issues/344)
- A bug related to recent changes to the [**ranger**](https://CRAN.R-project.org/package=ranger) package was fixed [(issue 320)](https://github.com/topepo/caret/issues/320)
- Dependencies on external software can now be checked in the model code. See [`pythonKnnReg`](https://github.com/topepo/caret-archive/blob/main/models/deprecated/pythonKnnReg.R) for an example. This also removes the overall package dependency on [**rPython**](https://CRAN.R-project.org/package=rPython) [(issue 328)](https://github.com/topepo/caret/issues/328).
- The tuning parameter grid for `enpls` and `enpls.fs` were changed to avoid errors.
- A bug was fixed [(issue 342)](https://github.com/topepo/caret/issues/342) where the data used for prediction was inappropriately converted from its original class.
- Matt (aka washcycle) added option to return column names to `nearZeroVar` function
- Homer Strong fixed `varImp` for `glmnet` models so that they return the absolute value of the regression coefficients [(issue 173)](https://github.com/topepo/caret/issues/173) [(issue 190)](https://github.com/topepo/caret/issues/190)
- The basic naive Bayes method (`method = "nb"`) gained a tuning parameter, `adjust`, that adjusts the bandwidth (see `?density`). The parameter is ignored when `usekernel = FALSE`.

## Changes in version 6.0-62

- From the [**randomGLM**](https://CRAN.R-project.org/package=randomGLM) package, a model of the same name was added.
- From [**monomvn**](https://CRAN.R-project.org/package=monomvn) package, models for the Bayesian lasso and ridge regression were added. In the latter case, two methods were added. `blasso` creates predictions using the mean of the posterior distributions but sets some parameters specifically to zero based on the tuning parameter called `sparsity`. For example, when `sparsity = .5`, only coefficients where at least half the posterior estimates are nonzero are used. The other model, `blassoAveraged`, makes predictions across all of the realizations in the posterior distribution without coercing any coefficients to zero. This is more consistent with Bayesian model averaging, but is unlikely to produce very sparse solutions.
- From the [**spikeslab**](https://CRAN.R-project.org/package=spikeslab) package, a regression model was added that emulates the procedure used by `cv.spikeslab` where the tuning variable is the number of retained predictors.
- A bug was fixed in adaptive resampling (found by github user elephann) [(issue 304)](https://github.com/topepo/caret/issues/304)
- Fixed another adaptive resampling bug flagged by github user elephann related to the latest version of the [**BradleyTerry2**](https://CRAN.R-project.org/package=BradleyTerry2) package. Thanks to Heather Turner for the fix [(issue 310)](https://github.com/topepo/caret/issues/310)
- Yuan (Terry) Tang added more tuning parameters to `xgbTree` models.
- Model `svmRadialWeights` was updated to allow for class probabilities. Previously, [**kernlab**](https://CRAN.R-project.org/package=kernlab) did not change the probability estimates when weights were used.
- A [**ggplot2**](https://CRAN.R-project.org/package=ggplot2) method for `varImp.train` was added [(issue 231)](https://github.com/topepo/caret/issues/231)
- Changes were made for the package to work with the next version of [**ggplot2**](https://CRAN.R-project.org/package=ggplot2) [(issue 317)](https://github.com/topepo/caret/issues/317)
- Github user `fjeze` added new models `mlpML` and `mlpWeightDecayML` that extend the existing [**RSNNS**](https://CRAN.R-project.org/package=RSNNS) models to multiple layers. `fjeze` also added the `gamma` parameter to the `svmLinear2` model.
- A function for generating data for learning curves was added.
- The range of SVM cost values explored in random search was expanded.

## Changes in version 6.0-58

- A major bug was fixed (found by Harlan Harris) where pre-processing objects created from versions of the package prior to 6.0-57 can give incorrect results when run with 6.0-57 [(issue 282)](https://github.com/topepo/caret/issues/282).
- `preProcess` can now remove predictors using zero- and near zero-variance filters via (`method` values of `"zv"` and `"nzv"`). When used, these filters are applied to numeric predictors prior to all other pre-processing operations.
- `train` now throws an error for classification tasks where the outcome has a factor level with no observed data [(issue 260)](https://github.com/topepo/caret/issues/260).
- Character outcomes passed to `train` are not converted to factors.
- A bug was found and fixed in this package's class probability code for `gbm` models when a single multinomial observation is predicted [(issue 274)](https://github.com/topepo/caret/issues/274).
- A new option to `ggplot.train` was added that highlights the optimal tuning parameter setting in the cases where grid search is used (thanks to Balaji Iyengar (github: bdanalytics)).
- In `trainControl`, the argument `savePredictions` can now be character values (`"final"`, `"all"` or `"none"`). Logicals can still be used and match to `"all"` or `"none"`.

## Changes in version 6.0-57

- Hyperparameter optimization via random search is now availible. See the new [help page](http://topepo.github.io/caret/random-hyperparameter-search.html) for examples and syntax.
- `preProcess` now allows (but ignores) non-numeric predictor columns.
- Models were added for optimal weighted and stabilized nearest neighbor classifiers from the [**snn**](https://CRAN.R-project.org/package=snn) package were added with model codes `snn` and `ownn`
- Random forests using the excellent [**ranger**](https://CRAN.R-project.org/package=ranger) package were added (`method = "ranger"`)
- An additional variation of rotation forests was added (`rotationForest2`) that also tunes over `cp`. Unfortunately, the sub-model trick can't be utilized in this instance.
- Kernelized distance weighted discriminant analysis models from [**kerndwd**](https://CRAN.R-project.org/package=kerndwd) where added (`dwdLieanr`, `dwdPoly`, and `dwdRadial`)
- A bug was fixed with `rfe` when `train` was used to generate a classification model but class probabilities were not (or could not be) generated [(issue 234)](https://github.com/topepo/caret/issues/234).
- Can Candan added a python model `sklearn.neighbors.KNeighborsRegressor` that can be accessed via `train` using the [**rPython**](https://CRAN.R-project.org/package=rPython) package. The python modules `sklearn` and `pandas` are required for this to run.
- Jason Aizkalns fixed a bunch of typos.
- MarwaNabil found a bug with `lift` and missing values [(issue 225)](https://github.com/topepo/caret/issues/225). This was fixed such that missing values are removed prior to the calculations (within each model)
- Additional options were added to `LPH07_1` so that two class data can also be simulated and predictors are converted to factors.
- The model-specific code for computing out-of-bag performance estimates were moved into the model code library [(issue 230)](https://github.com/topepo/caret/issues/230).
- A variety of naive Bayes and tree augmented naive Bayes classifier from the [**bnclassify**](https://CRAN.R-project.org/package=bnclassify) package were added. Variations include simple models (methods labeled as `"nbDiscrete"` and `"tan"`), models using attribute weighting (`"awnb"` and `"awtan"`), and wrappers that use search methods to optimize the network structure (`"nbSearch"` and `"tanSearch"`). In each case, the predictors and outcomes must all be factor variables; for that reason, using the non-formula interface to `train` (e.g. `train(x, y)`) is critical to preserve the factor structure of the data.
- A function called `multiClassSummary` was added to compute performance values for problems with three or more classes. It works with or without predicted class probabilities [(issue 107)](https://github.com/topepo/caret/issues/107).
- `confusionMatrix` was modified to deal with name collisions between this package and [**RSNNS**](https://CRAN.R-project.org/package=RSNNS) [(issue 256)](https://github.com/topepo/caret/issues/256).
- A bug in how the LVQ tune grid is filtered was fixed.
- A bug in `preProcess` for ICA and PCA was fixed.
- Bugs in `avNNet` and `pcaNNet` when predicting class probabilities were fixed [(issue #261)](https://github.com/topepo/caret/issues/#261).

## Changes in version 6.0-52

- A new model using the [**randomForest**](https://CRAN.R-project.org/package=randomForest) and [**inTrees**](https://CRAN.R-project.org/package=inTrees) packages called `rfRules` was added. A basic random forest model is used and then is decomposed into rules (of user-specified complexity). The [**inTrees**](https://CRAN.R-project.org/package=inTrees) package is used to prune and optimize the rules. Thanks to Mirjam Jenny who suggested the workflow.
- Other new models (and their packages): `bartMachine` ([**bartMachine**](https://CRAN.R-project.org/package=bartMachine)), `rotationForest` ([**rotationForest**](https://CRAN.R-project.org/package=rotationForest)), `sdwd` ([**sdwd**](https://CRAN.R-project.org/package=sdwd)), `loclda` ([**klaR**](https://CRAN.R-project.org/package=klaR)), `nnls` ([**nnls**](https://CRAN.R-project.org/package=nnls)), `svmLinear2` ([**e1071**](https://CRAN.R-project.org/package=e1071)), `rqnc` ([**rqPen**](https://CRAN.R-project.org/package=rqPen)), and `rqlasso` ([**rqPen**](https://CRAN.R-project.org/package=rqPen))
- When specifying your own resampling indices, a value of `method = "custom"` can be used with `trainControl` for better printing.
- Tim Lucas fixed a bug in `avNNet` when `bag = TRUE`
- Fixed a bug found by `ruggerorossi` in `method = "dnn"` with classification.
- A new option called `sampling` was added to `trainControl` that allows users to subsample their data in the case of a class imbalance. Another [help page](http://topepo.github.io/caret/subsampling-for-class-imbalances.html) was added to explain the features.
- Class probabilities can be computed for `extraTrees` models now.
- When PCA pre-processing is conducted, the variance trace is saved in an object called `trace`.
- More error traps were added for common mistakes (e.g. bad factor levels in classification).
- An internal function (`class2ind`) that can be used to make dummy variables for a single factor vector is now documented and exported.
- A bug was fixed in the `xyplot.lift` where the reference line was incorrectly computed. Thanks to Einat Sitbon for finding this.
- A bug related to calculating the Box-Cox transformation found by John Johnson was fixed.
- github user `EdwinTh` developed a faster version of `findCorrelation` and found a bug in the original code. `findCorrelation` has two new arguments, one of which is called `exact` which defaults to use the original (fixed) function. Using `exact = FALSE` uses the faster version. The fixed version of the "exact" code is, on average, 26-fold slower than the current version (for 250x250 matrices) although the average time for matrices of this size was only 26s. The exact version yields subsets that are, one average, 2.4 percent smaller than the other versions. This difference will be more significant for smaller matrices. The faster ("approximate") version of the code is 8-fold faster than the current version.
- github user `slyuee` found a bug in the `gam` model fitting code.
- Chris Kennedy fixed a bug in the `bartMachine` variable importance code.

## Changes in version 6.0-47

- CHAID from the R-Forge package **CHAID**
- Models `xgbTree` amd `xgbLinear` from the `xgboost` package were added. That package is not on CRAN and can be installed from github using the [**devtools**](https://CRAN.R-project.org/package=devtools) package and `install_github('dmlc/xgboost',subdir='R-package')`.
- `dratewka` enabled `rbf` models for regression.
- A summary function for the multinomial likelihood called `mnLogLoss` was added.
- The total object size for `preProces` objects that used bagged imputation was reduced almost 5-fold.
- A new option to `trainControl` called `trim` was added where, if implemented, will reduce the model's footprint. However, features beyond simple prediction may not work.
- A rarely occurring bug in `gbm` model code was fixed (thanks to Wade Cooper)
- `splom.resamples` now respects the `models` argument
- A new argument to `lift` called `cuts` was added to allow more control over what thresholds are used to calculate the curve.
- The `cuts` argument of `calibration` now accepts a vector of cut points.
- Jason Schadewald noticed and fixed a bug in the man page for `dummyVars`
- Call objects were removed from the following models: `avNNet`, `bagFDA`, `icr`, `knn3`, `knnreg`, `pcaNNet`, and `plsda`.
- An argument was added to `createTimeSlices` to thin the number of resamples
- The RFE-related functions `lrFuncs`, `lmFuncs`, and `gamFuncs` were updated so that `rfe` accepts a matrix `x` argument.
- Using the default grid generation with `train` and `glmnet`, an initial `glmnet` fit is created with `alpha = 0.50` to define the `lambda` values.
- `train` models for `"gbm"`, `"gam"`, `"gamSpline"`, and `"gamLoess"` now allow their respective arguments for the outcome probability distribution to be passed to the underlying function.
- A bug in `print.varImp.train` was fixed.
- `train` now returns an additional column called `rowIndex` that is exposed when calling the summary function during resampling.
- The ability to compute class probabilities was removed from the `rpartCost` model since they are unlikely to agree with the class predictions.
- `extractProb` no longer redundantly calls `extractPrediction` to generate the class predictions.
- A new function called `var_seq` was added that finds a sequence of integers that can be useful for some tuning parameters such as random forests `mtry`. Model modules were update to use the new function.
- `n.minobsinnode` was added as a tuning parameter to `gbm` models.
- For models using out-of-bag resampling, `train` now properly checks the `metric` argument against the names of the measured outcomes.
- Both `createDataParition` and `createFolds` were modified to better handle cases where one or more class have very low numbers of data points.

## Changes in version 6.0-41

- The license was changed to GPL (>= 2) to accommodate new code from the GA package.
- New feature selection functions `gafs` and `safs` were added, along with helper functions and objects, were added. The package HTML was updated to expand more about feature selection.
- From the [**adabag**](https://CRAN.R-project.org/package=adabag) package, two new models were added: `AdaBag` and `AdaBoost.M1`.
- Weighted subspace random forests from the [**wsrf**](https://CRAN.R-project.org/package=wsrf) package was added.
- Additional bagged FDA and MARS models were added (model codes `bagFDAGCV` and `bagEarthGCV`) were added that use the GCV statistic to prune the model. This leads to memory reductions during training.
- The model code for `ada` had a bug fix applied and the code was adapted to use the "sub-model trick" so it should train faster.
- A bug was fixed related to imputation when the formula method is used with `train`
- The old `drop = FALSE` bug was fixed in `getTrainPerf`
- A bug was fixed for custom models with no labels.
- A bug fix was made for bagged MARS models when predicting probabilities.
- In `train`, the argument `last` was being incorrectly set for the last model.
- Reynald Lescarbeau refactored `findCorrelation` to make it faster.
- The apparent performance values are not reported by `print.train` when the bootstrap 632 estimate is used.
- When a required package is missing, the code stops earlier with a more explicit error message.

## Changes in version 6.0-37

- Brenton Kenkel added ordered logistic or probit regression to `train` using `method = "polr"` from [**MASS**](https://CRAN.R-project.org/package=MASS)
- `LPH07_1` now encodes the noise variables as binary
- Both `rfe` and `sbf` get arguments for `indexOut` for their control functions.
- A reworked version of `nearZerVar` based on code from Michael Benesty was added the old version is now called `nzv` that uses less memory and can be used in parallel.
- The adaptive mixture discriminant model from the [**adaptDA**](https://CRAN.R-project.org/package=adaptDA) package was added as well as a robust mixture discriminant model from the [**robustDA**](https://CRAN.R-project.org/package=robustDA) package.
- The multi-class discriminant model using binary predictors in the [**binda**](https://CRAN.R-project.org/package=binda) package was added.
- Ensembles of partial least squares models (via the [**enpls**](https://CRAN.R-project.org/package=enpls)) package was added.
- A bug using `gbm` with Poisson data was fixed (thanks to user eriklampa)
- `sbfControl` now has a `multivariate` option where all the predictors are exposed to the scoring function at once.
- A function `compare_models` was added that is a simple comparison of models via `diff.resamples)`.
- The row names for the `variables` component of `rfe` objects were simplified.
- Philipp Bergmeir found a bug that was fixed where `bag` would not run in parallel.
- `predictionBounds` was not implemented during resampling.

## Changes in version 6.0-35

- A few bug fixes to `preProcess` were made related to KNN imputation.
- The parameter labels for polynomial SVM models were fixed
- The tags for `dnn` models were fixed.
- The following functions were removed from the package: `generateExprVal.method.trimMean`, `normalize.AffyBatch.normalize2Reference`, `normalize2Reference`, and `PLS`. The original code and the man files can be found at [https://github.com/topepo/caret-archive/tree/main/deprecated](https://github.com/topepo/caret-archive/tree/main/deprecated).
- A number of changes to comply with section 1.1.3.1 of "Writing R Extensions" were made.

## Changes in version 6.0-34

- For the input data `x` to `train`, we now respect the class of the input value to accommodate other data types (such as sparse matrices). There are some complications though; for pre-processing we throw a warning if the data are not simple matrices or data frames since there is some infrastructure that does not exist for other classes( e.g. `complete.cases`). We also throw a warning if `returnData <- TRUE` and it cannot be converted to a data frame. This allows the use of sparse matrices and text corpus to be used as inputs into that function.
- `plsRglm` was added.
- From the [**frbs**](https://CRAN.R-project.org/package=frbs), the following rule-based models were added: `ANFIS`, `DENFIS`, `FH.GBML`, `FIR.DM`, `FRBCS.CHI`, `FRBCS.W`, `FS.HGD`, `GFS.FR.MOGAL`, `GFS.GCCL`, `GFS.LTS`, `GFS.THRIFT`, `HYFIS`, `SBC` and `WM`. Thanks to Lala Riza for suggesting these and facilitating their addition to the package.
- From the [**kernlab**](https://CRAN.R-project.org/package=kernlab) package, SVM models using string kernels were added: `svmBoundrangeString`, `svmExpoString`, `svmSpectrumString`
- A function `update.rfe` was added.
- `cluster.resamples` was added to the namespace.
- An option to choose the `metric` was added to `summary.resamples`.
- `prcomp.resamples` now passed `...` to `prcomp`. Also the call to `prcomp` uses the formula method so that `na.action` can be used.
- The function `resamples` was enhanced so that `train` and `rfe` models that used `returnResamp="all"` subsets the resamples to get the appropriate values and issues a warning. The function also fills in missing model names if one or more are not given.
- Several regression simulation functions were added: `SLC14_1`, `SLC14_2`, `LPH07_1` and `LPH07_2`
- `print.train` was re-factored so that `format.data.frame` is now used. This should behave better when using [**knitr**](https://CRAN.R-project.org/package=knitr).
- The error message in `train.formula` was improved to provide more helpful feedback in cases where there is at least one missing value in each row of the data set.
- `ggplot.train` was modified so that groups are distinguished by color and shape.
- Options were added to `plot.train` and `ggplot.train` called `nameInStrip` that will print the name and value of any tuning parameters shown in panels.
- A bug was fixed by Jia Xu within the knn imputation code used by `preProcess`.

## Changes in version 6.0-30

- A missing piece of documentation in `trainControl` for adaptive models was filled in.
- A warning was added to `plot.train` and `ggplot.train` to note that the relationship between the resampled performance measures and the tuning parameters can be deceiving when using adaptive resampling.
- A check was added to `trainControl` to make sure that a value of `min` makes sense when using adaptive resampling.

## Changes in version 6.0-29

- A man page with the list of models available via `train` was added back into the package. See `?models`.
- Thoralf Mildenberger found and fixed a bug in the variable importance calculation for neural network models.
- The output of `varImp` for `pamr` models was updated to clarify the ordering of the importance scores.
- `getModelInfo` was updated to generate a more informative error message if the user looks for a model that is not in the package's model library.
- A bug was fixed related to how seeds were set inside of `train`.
- The model `"parRF"` (parallel random forest) was added back into the library.
- When case weights are specified in `train`, the hold-out weights are exposed when computing the summary function.
- A check was made to convert a `data.table` given to `train` to a data frame (see <https://stackoverflow.com/questions/23256177/r-caret-renames-column-in-data-table-after-training>).

## Changes in version 6.0-25

- Changes were made that stopped execution of `train` if there are no rows in the data (changes suggested by Andrew Ziem)
- Andrew Ziem also helped improve the documentation.

## Changes in version 6.0-24

- Several models were updated to work with case weights.
- A bug in `rfe` was found where the largest subset size have the same results as the full model. Thanks to Jose Seoane for reporting the bug.

## Changes in version 6.0-22

- For some parallel processing technologies, the package now export more internal functions.
- A bug was fixed in `rfe` that occurred when LOO CV was used.
- Another bug was fixed that occurred for some models when `tuneGrid` contained only a single model.

## Changes in version 6.0-21

- A new system for user-defined models has been added.
- When creating the grid of tuning parameter values, the column names no longer need to be preceded by a period. Periods can still be used as before but are not required. This isn't guaranteed to break backwards compatibility but it may in some cases.
- `trainControl` now has a `method = "none"` resampling option that bypasses model tuning and fits the model to the entire training set. Note that if more than one model is specified an error will occur.
- `logicForest` models were removed since the package is now archived.
- `CSimca` and `RSimca` models from the [**rrcovHD**](https://CRAN.R-project.org/package=rrcovHD) package were added.
- Model `elm` from the [**elmNN**](https://CRAN.R-project.org/package=elmNN) package was added.
- Models `rknn` and `rknnBel` from the [**rknn**](https://CRAN.R-project.org/package=rknn) package were added
- Model `brnn` from the [**brnn**](https://CRAN.R-project.org/package=brnn) package was added.
- `panel.lift2` and `xyplot.lift` now have an argument called `values` that show the percentages of samples found for the specified percentages of samples tested.
- `train`, `rfe` and `sbf` should no longer throw a warning that "executing
- A `ggplot` method for `train` was added.
- Imputation via medians was added to `preProcess` by Zachary Mayer.
- A small change was made to `rpart` models. Previously, when the final model is determined, it would be fit by specifying the model using the `cp` argument of `rpart.control`. This could lead to duplicated Cp values in the final list of possible Cp values. The current version fits the final model slightly different. An initial model is fit using `cp = 0` then it is pruned using `prune.rpart` to the desired depth. This shouldn't be different for the vast majority of data sets. Thanks to Jeff Evans for pointing this out.
- The method for estimating sigma for SVM and RVM models was slightly changed to make them consistent with how `ksvm` and `rvm` does the estimation.
- The default behavior for `returnResamp` in `rfeControl` and `sbfControl` is now `returnResamp = "final"`.
- `cluster` was added as a general class with a specific method for `resamples` objects.
- The refactoring of model code resulted in a number of packages being eliminated from the depends field. Additionally, a few were moved to exports.

## Changes in version 5.17-07

- A bug in `spatialSign` was fixed for data frames with a single column.
- Pre-processing was not applied to the training data set prior to grid creation. This is now done but only for models that use the data when defining the grid. Thanks to Brad Buchsbaum for finding the bug.
- Some code was added to `rfe` to truncate the subset sizes in case the user over-specified them.
- A bug was fixed in `gamFuncs` for the `rfe` function.
- Option in `trainControl`, `rfeControl` and `sbfControl` were added so that the user can set the seed at each resampling iteration (most useful for parallel processing). Thanks to Allan Engelhardt for the recommendation.
- Some internal refactoring of the data was done to prepare for some upcoming resampling options.
- `predict.train` now has an explicit `na.action` argument defaulted to `na.omit`. If imputation is used in `train`, then `na.action = na.pass` is recommended.
- A bug was fixed in `dummyVars` that occured when missing data were in `newdata`. The function `contr.dummy` is now deprecated and `contr.ltfr` should be used (if you are using it at all). Thanks to stackexchange user mchangun for finding the bug.
- A check is now done inside `dummyVars` when `levelsOnly = TRUE` to see if any predictors share common levels.
- A new option `fullRank` was added to `dummyVars`. When true, `contr.treatment` is used. Otherwise, `contr.ltfr` is used.
- A bug in `train` was fixed with `gbm` models (thanks to stackoverflow user screechOwl for finding it).

## Changes in version 5.16-24

- The `protoclass` function in the [**protoclass**](https://CRAN.R-project.org/package=protoclass) package was added. The model uses a distance matrix as input and the `train` method also uses the [**proxy**](https://CRAN.R-project.org/package=proxy) package to compute the distance using the Minkowski distance. The two tuning parameters is the neighborhood size (`eps`) and the Minkowski distance parameter (`p`).
- A bug was (hopefully) fixed that occurred when some type of parallel processing was used with `train`. The problem is that the `methods` package was not being loaded in the workers. While reproducible, it is unknown why this occurs and why it is only for some technologies and systems. The `methods` package is now a formal dependency and we coerce the workers to load it remotely.
- A bug was fixed where some calls were printed twice.
- For `rpart`, `C5.0` and `ksvm`, cost-sensitive versions of these models for two classes were added to `train`. The method values are `rpartCost`, `C5.0Cost` and `svmRadialWeights`.
- The prediction code for the `ksvm` models was changed. There are some cases where the class predictions and the predicted class probabilities disagree. This usually happens when the probabilities are close to 0.50 (in the two class case). A [**kernlab**](https://CRAN.R-project.org/package=kernlab) bug has been filed. In the meantime, if the `ksvm` model uses a probability model, the class probabilities are generated first and the predicted class is assigned to the probability with the largest value. Thanks to Kjell Johnson for finding that one.
- `print.train` was changed so that tune parameters that are logicals are printed well.

## Changes in version 5.16-13

- Added a few exemptions to the logic that determines whether a model call should be scrubbed.
- An error trap was created to catch issues with missing importance scores in `rfe`.

## Changes in version 5.16-03

- A function `twoClassSim` was added for benchmarking classification models.
- A bug was fixed in `predict.nullModel` related to predicted class probabilities.
- The version requirement for [**gbm**](https://CRAN.R-project.org/package=gbm) was updated.
- The function `getTrainPerf` was made visible.
- The automatic tuning grid for `sda` models from the [**sda**](https://CRAN.R-project.org/package=sda) package was changed to include `lambda`.
- When `randomForests` is used with `train` and `tuneLength == 1`, the `randomForests` default value for `mtry` is used.
- Maximum uncertainty linear discriminant analysis (`Mlda`) and factor-based linear discriminant analysis (`RFlda`) from the [**HiDimDA**](https://CRAN.R-project.org/package=HiDimDA) package were added to `train`.

## Changes in version 5.15-87

- Added the Yeo-Johnson power transformation from the [**car**](https://CRAN.R-project.org/package=car) package to the `preProcess` function.
- A `train` bug was fixed for the `rrlda` model (found by Tiago Branquinho Oliveira).
- The `extraTrees` model in the [**extraTrees**](https://CRAN.R-project.org/package=extraTrees) package was added.
- The `kknn.train` model in the [**kknn**](https://CRAN.R-project.org/package=kknn) package was added.
- A bug was fixed in `lrFuncs` where the class threshold was improperly set (thanks to David Meyer).
- A bug related to newer versions of the [**gbm**](https://CRAN.R-project.org/package=gbm) package were fixed. Another [**gbm**](https://CRAN.R-project.org/package=gbm) bug was fixed related to using non-Bernoulli distributions with two class outcomes (thanks to Zachary Mayer).
- The old funciton `getTrainPerf` was finally made visible.
- Some models are created using "do.call" and may contain the entire data set in the call object. A function to "scrub" some model call objects was added to reduce their size.
- The tuning process for `sda:::sda` models was changed to add the `lambda` parameter.

## Changes in version 5.15-60

- A bug in `predictors.earth`, discovered by Katrina Bennett, was fixed.
- A bug induced by version 5.15-052 for the bootstrap 632 rule was fixed.
- The DESCRIPTION file as of 5.15-048 should have used a version-specific lattice dependency.
- `lift` can compute gain and lift charts (and defaults to gain)
- The [**gbm**](https://CRAN.R-project.org/package=gbm) model was updated to handle 3 or more classes.
- For bagged trees using [**ipred**](https://CRAN.R-project.org/package=ipred), the code in `train` defaults to `keepX = FALSE` to save space. Pass in `keepX = TRUE` to use out-of-bag sampling for this model.
- Changes were made to support vector machines for classification models due to bugs with class probabilities in the latest version of [**kernlab**](https://CRAN.R-project.org/package=kernlab). The `prob.model` will default to the value of `classProbs` in the `trControl` function. If `prob.model` is passed in as an argument to `train`, this specification over-rides the default. In other words, to avoid generating a probability model, set either `classProbs = FALSE` or `prob.model = FALSE`.

## Changes in version 5.15-052

- Added `bayesglm` from the [**arm**](https://CRAN.R-project.org/package=arm) package.
- A few bugs were fixed in `bag`, thanks to Keith Woolner. Most notably, out-of-bag estimates are now computed when the prediction function includes a column called `pred`.
- Parallel processing was implemented in `bag` and `avNNet`, which can be turned off using an optional arguments.
- `train`, `rfe`, `sbf`, `bag` and `avNNet` were given an additional argument in their respective control files called `allowParallel` that defaults to `TRUE`. When `Code`, the code will be executed in parallel if a parallel backend (e.g. [**doMC**](https://CRAN.R-project.org/package=doMC)) is registered. When `allowParallel = FALSE`, the parallel backend is always ignored. The use case is when `rfe` or `sbf` calls `train`. If a parallel backend with P processors is being used, the combination of these functions will create P^2 processes. Since some operations benefit more from parallelization than others, the user has the ability to concentrate computing resources for specific functions.
- A new resampling function called `createTimeSlices` was contributed by Tony Cooper that generates cross-validation indices for time series data.
- A few more options were added to `trainControl`. `initialWindow`, `horizon` and `fixedWindow` are applicable for when `method = "timeslice"`. Another, `indexOut` is an optional list of resampling indices for the hold-out set. By default, these values are the unique set of data points not in the training set.
- A bug was fixed in multiclass `glmnet` models when generating class probabilities (thanks to Bradley Buchsbaum for finding it).

## Changes in version 5.15-048

- The three vignettes were removed and two things were added: a smaller vignette and a large collection of help pages.
- Minkoo Seo found a bug where `na.action` was not being properly set with train.formula().
- `parallel.resamples` was changed to properly account for missing values.
- Some testing code was removed from `probFunction` and `predictionFunction`.
- Fixed a bug in `sbf` exposed by a new version of [**plyr**](https://CRAN.R-project.org/package=plyr).
- Changed the package dependency on [**reshape**](https://CRAN.R-project.org/package=reshape) to [**reshape2**](https://CRAN.R-project.org/package=reshape2).
- To be more consistent with recent versions of [**lattice**](https://CRAN.R-project.org/package=lattice), the `parallel.resamples` function was changed to `parallelplot.resamples`.
- Since `ksvm` now allows probabilities when class weights are used, the default behavior in `train` is to set `prob.model = TRUE` unless the user explicitly sets it to `FALSE`. However, I have reported a bug in `ksvm` that gives inconsistent results with class weights, so this is not advised at this point in time.
- Bugs were fix in `predict.bagEarth` and `predict.bagFDA`.
- When using `rfeControl(saveDetails = TRUE)` or `sbfControl(saveDetails = TRUE)` an additional column is added to `object$pred` called `rowIndex`. This indicates the row from the original data that is being held-out.

## Changes in version 5.15-045

- A bug was fixed that induced `NA` values in SVM model predictions.

## Changes in version 5.15-042

- Many examples are wrapped in dontrun to speed up cran checking.
- The `scrda` methods were removed from the package (on 6/30/12, R Core sent an email that "since we haven't got fixes for long standing warnings of the rda packages since more than half a year now, we set the package to ORPHANED.")
- [**C50**](https://CRAN.R-project.org/package=C50) was added (model codes `C5.0`, `C5.0Tree` and `C5.0Rules`).
- Fixed a bug in `train` with NaiveBayes when `fL != 0` was used
- The output of `train` with `verboseIter = TRUE` was modified to show the resample label as well as logging when the worker started and stopped the task (better when using parallel processing).
- Added a long-hidden function `downSample` for class imbalances
- An `upSample` function was added for class imbalances.
- A new file, aaa.R, was added to be compiled first that tries to eliminate the dreaded 'no visible binding for global variable' false positives. Specific namespaces were used with several functions for avoid similar warnings.
- A bug was fixed with `icr.formula` that was so ridiculous, I now know that nobody has ever used that function.
- Fixed a bug when using `method = "oob"` with `train`
- Some exceptions were added to `plot.train` so that some tuning parameters are better labeled.
- `dotplot.resamples` and `bwplot.resamples` now order the models using the first metric.
- A few of the lattice plots for the `resamples` class were changed such that when only one metric is shown: the strip is not shown and the x-axis label displays the metric
- When using `trainControl(savePredictions = TRUE)` an additional column is added to `object$pred` called `rowIndex`. This indicates the row from the original data that is being held-out.
- A variable importance function for `nnet` objects was created based on Gevrey, M., Dimopoulos, I., & Lek, S. (2003). Review and comparison of methods to study the contribution of variables in artificial neural network models. ecological modelling, 160(3), 249–264.
- The `predictor` function for `glmnet` was update and a variable importance function was also added.
- Raghu Nidagal found a bug in `predict.avNNet` that was fixed.
- `sensitivity` and `specificity` were given an `na.rm` argument.
- A first attempt at fault tolerance was added to `train`. If a model fit fails, the predictions are set to `NA` and a warning is issued (eg "model fit failed for Fold04: sigma=0.00392, C=0.25"). When `verboseIter = TRUE`, the warning is also printed to the log. Resampled performance is calculated on only the non-missing estimates. This can also be done during predictions, but must be done on a model by model basis. Fault tolerance was added for [**kernlab**](https://CRAN.R-project.org/package=kernlab) models only at this time.
- `lift` was modified in two ways. First, `cuts` is no longer an argument. The function always uses cuts based on the number of unique probability estimates. Second, a new argument called `label` is available to use alternate names for the models (e.g. names that are not valid R variable names).
- A bug in `print.bag` was fixed.
- Class probabilities were not being generated for sparseLDA models.
- Bugs were fixed in the new varImp methods for PART and RIPPER
- Starting using namespaces for `ctree` and `cforest` to avoid conflicts between duplicate function names in the [**party**](https://CRAN.R-project.org/package=party) and [**partykit**](https://CRAN.R-project.org/package=partykit) package
- A set of functions for RFE and logistic regression (`lrFuncs`) was added.
- A bug in `train` with `method="glmStepAIC"` was fixed so that `direction` and other `stepAIC` arguments were honored.
- A bug was fixed in `preProcess` where the number of ICA components was not specified. (thanks to Alexander Lebedev)
- Another bug was fixed for oblique random forest methods in `train`. (thanks to Alexander Lebedev)

## Changes in version 5.15-023

- The list of models that can accept factor inputs directly was expanded to include the [**RWeka**](https://CRAN.R-project.org/package=RWeka) models, `ctree`, `cforest` and custom models.
- Added model `lda2`, which tunes by the number of functions used during prediction.
- `predict.train` allows probability predictions for custom models now (thanks to Peng Zhang)
- `confusionMatrix.train` was updated to use the default `confusionMatrix` code when `norm = "none"` and only a single hold-out was used.
- Added variable importance metrics for PART and RIPPER in the [**RWeka**](https://CRAN.R-project.org/package=RWeka) package.
- vignettes were moved from /inst/doc to /vignettes

## Changes in version 5.14-023

- The model details in `?train` was changed to be more readable
- Added two models from the [**RRF**](https://CRAN.R-project.org/package=RRF) package. `RRF` uses a penalty for each predictor based on the scaled variable importance scores from a prior random forest fit. `RRFglobal` sets a common, global penalty across all predictors.
- Added two models from the [**KRLS**](https://CRAN.R-project.org/package=KRLS) package: `krlsRadial` and `krlsPoly`. Both have kernel parameters (`sigma` and `degree`) and a common regularization parameter `lambda`. The default for `lambda` is `NA`, letting the `krls` function estimate it internally. `lambda` can also be specified via `tuneGrid`.
- `twoClassSummary` was modified to wrap the call to `pROC:::roc` in a `try` command. In cases where the hold-out data are only from one class, this produced an error. Now it generates `NA` values for the AUC when this occurs and a general warning is issued.
- The underlying workflows for `train` were modified so that missing values for performance measures would not throw an error (but will issue a warning).

## Changes in version 5.13-037

- Models `mlp`, `mlpWeightDecay`, `rbf` and `rbfDDA` were added from [**RSNNS**](https://CRAN.R-project.org/package=RSNNS).
- Functions `roc`, `rocPoint` and `aucRoc` finally met their end. The cake was a lie.
- This NEWS file was converted over to Rd format.

## Changes in version 5.13-020

- `lift` was expanded into `lift.formula` for calculating the plot points and `xyplot.lift` to create the plot.
- The package vignettes were altered to stop loading external RData files.
- A few `match.call` changes were made to pass new R CMD check tests.
- `calibration`, `calibration.formula` and `xyplot.calibration` were created to make probability calibration plots.
- Model types `xyf` and `bdk` from the [**kohonen**](https://CRAN.R-project.org/package=kohonen) package were added.
- `update.train` was added so that tuning parameters can be manually set if the automated approach to setting their values is insufficient.

## Changes in version 5.11-006

- When using `method = "pls"` in `train`, the `plsr` function used the default PLS algorithm ("kernelpls"). Now, the full orthogonal scores method is used. This results in the same model, but a more extensive set of values are calculated that enable VIP calculations (without much of a loss in computational efficient).
- A check was added to `preProcess` to ensure valid values of `method` were used.
- A new method, `kernelpls`, was added.
- `residuals` and `summary` methods were added to `train` objects that pass the final model to their respective functions.

## Changes in version 5.11-006

- Bugs were fixed that prevented hold-out predictions from being returned.

## Changes in version 5.11-003

- A bug in `roc` was found when the classes were completely separable.
- The ROC calculations for `twoClassSummary` and `filterVarImp` were changed to use the [**pROC**](https://CRAN.R-project.org/package=pROC) package. This, and other changes, have increased efficiency. For `filterVarImp` on the cell segmentation data lead to a 54-fold decrease in execution time. For the Glass data in the [**mlbench**](https://CRAN.R-project.org/package=mlbench) package, the speedup was 37-fold. Warnings were added for `roc`, `aucRoc` and `rocPoint` regarding their deprecation.
- random ferns (package [**rFerns**](https://CRAN.R-project.org/package=rFerns)) were added
- Another sparse LDA model (from the penalizedLDA) was also added

## Changes in version 5.09-002

- Fixed a bug which occurred when `plsda` models were used with class probabilities
- As of 8/15/11, the `glmnet` function was updated to return a character vector. Because of this, `train` required modification and a version requirement was put in the package description file.

## Changes in version 5.09-006

- Shea X made a suggestion and provided code to improve the speed of prediction when sequential parameters are used for `gbm` models.
- Andrew Ziem suggested an error check with `metric = "ROC"` and `classProbs = FALSE`.
- Andrew Ziem found a bug in how `train` obtained `earth` class probabilities

## Changes in version 5.08-011

- Andrew Ziem found another small bug with parallel processing and `train` (functions in the caret namespace cannot be found).
- Ben Hoffman found a bug in `pickSizeTolerance` that was fixed.
- Jiaye Yu found (and fixed) a bug in getting predictions back from `rfe`

## Changes in version 5.07-024

- Using `saveDetails = TRUE` in `sbfControl` or `rfeControl` will save the predictions on the hold-out sets (Jiaye Yu wins the prize for finding that one).
- `trainControl` now has a logical to save the hold-out predictions.

## Changes in version 5.07-005

- `type = "prob"` was added for `avNNet` prediction.
- A warning was added when a model from [**RWeka**](https://CRAN.R-project.org/package=RWeka) is used with `train` and (it appears that) [**multicore**](https://CRAN.R-project.org/package=multicore) is being used for parallel processing. The session will crash, so don't do that.
- A bug was fixed where the extrapolation limits were being applied in `predict.train` but not in `extractPrediction`. Thanks to Antoine Stevens for finding this.
- Modifications were made to some of the workflow code to expose internal functions. When parallel processing was used with [**doMPI**](https://CRAN.R-project.org/package=doMPI) or [**doSMP**](https://CRAN.R-project.org/package=doSMP), [**foreach**](https://CRAN.R-project.org/package=foreach) did not find some [**caret**](https://CRAN.R-project.org/package=caret) internals (but [**doMC**](https://CRAN.R-project.org/package=doMC) did).

## Changes in version 5.07-001

- changed calls to `predict.mvr` since the [**pls**](https://CRAN.R-project.org/package=pls) package now has a namespace.

## Changes in version 5.06-002

- a beta version of custom models with `train` is included. The "caretTrain" vignette was updated with a new section that defines how to make custom models.

## Changes in version 5.05-004

- laying some of the groundwork for custom models
- updates to get away from deprecated (mean and sd on data frames)
- The pre-processing in `train` bug of the last version was not entirely squashed. Now it is.

## Changes in version 5.04-007

- `panel.lift` was moved out of the examples in `?lift` and into the package along with another function, `panel.lift2`.
- `lift` now uses `panel.lift2` by default
- Added robust regularized linear discriminant analysis from the [**rrlda**](https://CRAN.R-project.org/package=rrlda) package
- Added `evtree` from [**evtree**](https://CRAN.R-project.org/package=evtree)
- A weird bug was fixed that occurred when some models were run with sequential parameters that were fixed to single values (thanks to Antoine Stevens for finding this issue). item Another bug was fixed where pre-processing with `train` could fail

## Changes in version 5.03-003

- pre-processing in `train` did not occur for the final model fit

## Changes in version 5.02-011

- A function, `lift`, was added to create lattice objects for lift plots.
- Several models were added from the [**obliqueRF**](https://CRAN.R-project.org/package=obliqueRF) package: 'ORFridge' (linear combinations created using L2 regularization), 'ORFpls' (using partial least squares), 'ORFsvm' (linear support vector machines), and 'ORFlog' (using logistic regression). As of now, the package only support classification.
- Added regression models `simpls` and `widekernelpls`. These are new models since both `train` and `plsr` have an argument called `method`, so the computational algorithm could not be passed through using the three dots.
- Model `rpart` was added that uses `cp` as the tuning parameter. To make the model codes more consistent, `rpart` and `ctree` correspond to the nominal tuning parameters (`cp` and `mincriterion`, respectively) and `rpart2` and `ctree2` are the alternate versions using `maxdepth`.
- The text for `ctree`'s tuning parameter was changed to '1 - P-Value Threshold'
- The argument `controls` was not being properly passed through in models `ctree` and `ctree2`.

## Changes in version 5.01-001

- `controls` was not being set properly for `cforest` models in `train`
- The print methods for `train`, `rfe` and `sbf` did not recognize LOOCV
- `avNNet` sometimes failed with categorical outcomes with `bag = FALSE`
- A bug in `preProcess` was fixed that was triggered by matrices without dimnames (found by Allan Engelhardt)
- bagged MARS models with factor outcomes now work
- `cforest` was using the argument `control` instead of `controls`
- A few bugs for class probabilities were fixed for `slda`, `hdda`, `glmStepAIC`, `nodeHarvest`, `avNNet` and `sda`
- When looping over models and resamples, the [**foreach**](https://CRAN.R-project.org/package=foreach) package is now being used. Now, when using parallel processing, the [**caret**](https://CRAN.R-project.org/package=caret) code stays the same and parallelism is invoked using one of the "do" packages (eg. [**doMC**](https://CRAN.R-project.org/package=doMC), [**doMPI**](https://CRAN.R-project.org/package=doMPI), etc). This affects `train`, `rfe` and `sbf`. Their respective man pages have been revised to illustrate this change.
- The order of the results produced by `defaultSummary` were changed so that the ROC AUC is first
- A few man and C files were updated to eliminate R CMD check warnings
- Now that we are using foreach, the verbose option in `trainControl`, `rfeControl` and `sbfControl` are now defaulted to `FALSE`
- `rfe` now returns the variable ranks in a single data frame (previously there were data frames in lists of lists) for each of use. This will will break code from previous versions. The built-in RFE functions were also modified
- confusionMatrix methods for `rfe` and `sbf` were added
- NULL values of 'method' in `preProcess` are no longer allowed
- a model for ridge regression was added (`method = 'ridge'`) based on `enet`.

## Changes in version 4.98

- A bug was fixed in a few of the bagging aggregation functions (found by Harlan Harris).
- Fixed a bug spotted by Richard Marchese Robinson in createFolds when the outcome was numeric. The issue is that `createFolds` is trying to randomize `n/4` numeric samples to `k` folds. With less than 40 samples, it could not always do this and would generate less than `k` folds in some cases. The change will adjust the number of groups based on `n` and `k`. For small samples sizes, it will not use stratification. For larger data sets, it will at most group the data into quartiles.
- A function `confusionMatrix.train` was added to get an average confusion matrices across resampled hold-outs when using the `train` function for classification.
- Added another model, `avNNet`, that fits several neural networks via the [**nnet**](https://CRAN.R-project.org/package=nnet) package using different seeds, then averages the predictions of the networks. There is an additional bagging option.
- The default value of the 'var' argument of `bag` was changed.
- As requested, most options can be passed from `train` to `preProcess`. The `trainControl` function was re-factored and several options (e.g. `k`, `thresh`) were combined into a single list option called `preProcOptions`. The default is consistent with the original configuration: `preProcOptions = list(thresh = 0.95, ICAcomp = 3, k = 5)`
- nother option was added to `preProcess`. The `pcaComp` option can be used to set exactly how many components are used (as opposed to just a threshold). It defaults to `NULL` so that the threshold method is still used by default, but a non-null value of `pcaComp` over-rides `thresh`.
- When created within `train`, the call for `preProcess` is now modified to be a text string ("scrubed") because the call could be very large.
- Removed two deprecated functions: `applyProcessing` and `processData`.
- A new version of the cell segmentation data was saved and the original version was moved to the package website (see `segmentationData` for location). First, several discrete versions of some of the predictors (with the suffix `"Status"`) were removed. Second, there are several skewed predictors with minimum values of zero (that would benefit from some transformation, such as the log). A constant value of 1 was added to these fields: `AvgIntenCh2`, `FiberAlign2Ch3`, `FiberAlign2Ch4`, `SpotFiberCountCh4` and `TotalIntenCh2`.

## Changes in version 4.92

- Some tweaks were made to `plot.train` in a effort to get the group key to look less horrid.
- `train`, `rfe` and `sbf` are now able to estimate the time that these models take to predict new samples. Their respective control objects have a new option, `timingSamps`, that indicates how many of the training set samples should be used for prediction (the default of zero means do not estimate the prediction time).
- `xyplot.resamples` was modified. A new argument, `what`, has values: `"scatter"` plots the resampled performance values for two models; `"BlandAltman"` plots the difference between two models by the average (aka a MA plot) for two models; `"tTime"`, `"mTime"`, `"pTime"` plot the total model building and tuning; time (`"t"`) or the final model building time (`"m"`) or the time to produce predictions (`"p"`) against a confidence interval for the average performance. 2+ models can be used.
- Three new model types were added to `train` using `regsubsets` in the [**leaps**](https://CRAN.R-project.org/package=leaps) package: `"leapForward"`, `"leapBackward"` and `"leapSeq"`. The tuning parameter, `nvmax`, is the maximum number of terms in the subset.
- The seed was accidentally set when `preProcess` used ICA (spotted by Allan Engelhardt)
- `preProcess` was always being called (even to do nothing) (found by Guozhu Wen)

## Changes in version 4.91

- Added a few new models associated with the [**bst**](https://CRAN.R-project.org/package=bst) package: bstTree, bstLs and bstSm.
- A model denoted as `"M5"` that combines M5P and M5Rules from the [**RWeka**](https://CRAN.R-project.org/package=RWeka) package. This new model uses either of these functions depending on the tuning parameter `"rules"`.

## Changes in version 4.90

- Fixed a bug with `train` and `method = "penalized"`. Thanks to Fedor for finding it.

## Changes in version 4.89

- A new tuning parameter was added for `M5Rules` controlling smoothing.
- The Laplace correction value for Naive Bayes was also added as a tuning parameter.
- `varImp.RandomForest` was updated to work. It now requires a recent version of the [**party**](https://CRAN.R-project.org/package=party) package.

## Changes in version 4.88

- A variable importance method was created for [**Cubist**](https://CRAN.R-project.org/package=Cubist) models.

## Changes in version 4.87

- Altered the earth/MARS/FDA labels to be more exact.
- Added cubist models from the [**Cubist**](https://CRAN.R-project.org/package=Cubist) package.
- A new option to `trainControl` was added to allow users to constrain the possible predicted values of the model to the range seen in the training set or a user-defined range. One-sided ranges are also allowed.

## Changes in version 4.85

- Two typos fixed in `print.rfe` and `print.sbf` (thanks to Jan Lammertyn)

## Changes in version 4.83

- `dummyVars` failed with formulas using `"."` (`all.vars` does not handle this well)
- `tree2` was failing for some classification models
- When SVM classification models are used with `class.weights`, the options `prob.model` is automatically set to `FALSE` (otherwise, it is always set to `TRUE`). A warning is issued that the model will not be able to create class probabilities.
- Also for SVM classification models, there are cases when the probability model generates negative class probabilities. In these cases, we assign a probability of zero then coerce the probabilities to sum to one.
- Several typos in the help pages were fixed (thanks to Andrew Ziem).
- Added a new model, `svmRadialCost`, that fits the SVM model and estimates the `sigma` parameter for each resample (to properly capture the uncertainty).
- `preProcess` has a new method called `"range"` that scales the predictors to [0, 1] (which is approximate for new samples if the training set ranges is narrow in comparison).
- A check was added to `train` to make sure that, when the user passes a data frame to `tuneGrid`, the names are correct and complete.
- `print.train` prints the number of classes and levels for classification models.

## Changes in version 4.78

- Added a few bagging modules. See ?bag.
- Added basic timings of the entire call to `train`, `rfe` and `sbf` as well as the fit time of the final model. These are stored in an element called "times".
- The data files were updated to use better compression, which added a higher R version dependency.
- `plot.train` was pretty much re-written to more effectively use trellis theme defaults and to allow arguments (e.g. axis labels, keys, etc) to be passed in to over-ride the defaults.
- Bug fix for lda bagging function
- Bug fix for `print.train` when `preProc` is `NULL`
- `predict.BoxCoxTrans` would go all klablooey if there were missing values
- `varImp.rpart` was failing with some models (thanks to Maria Delgado)

## Changes in version 4.77

- A new class was added or estimating and applying the Box-Cox transformation to data called BoxCoxTrans. This is also included as an option to transform predictor variables. Although the Box-Tidwell transformation was invented for this purpose, the Box-Cox transformation is more straightforward, less prone to numerical issues and just as effective. This method was also added to `preProcess`.
- Fixed mis-labelled x axis in `plot.train` when a transformation is applied for models with three tuning parameters.
- When plotting a `train` object with `method == "gbm"` and multiple values of the shrinkage parameter, the ordering of panels was improved.
- Fixed bugs for regression prediction using `partDSA` and `qrf`.
- Another bug, reported by Jan Lammertyn, related to `extractPrediciton` with a single predictor was also fixed.

## Changes in version 4.76

- Fixed a bug where linear SVM models were not working for classification

## Changes in version 4.75

- `'gcvEearth'` which is the basic MARS model. The pruning procedure is the nominal one based on GCV; only the degree is tuned by `train`.
- `'qrnn'` for quantile regression neural networks from the [**qrnn**](https://CRAN.R-project.org/package=qrnn) package.
- `'Boruta'` for random forests models with feature selection via the [**Boruta**](https://CRAN.R-project.org/package=Boruta) package.

## Changes in version 4.74

- Some changes to `print.train`: the call is not automatically printed (but can be when `print.train` is explicitly invoked); the "Selected" column is also not automatically printed (but can be); non-table text now respects `options("width")`; only significant digits are now printed when tuning parameters are kept at a constant value

## Changes in version 4.73

- Bug fixes to `preProcess` related to complete.cases and a single predictor.
- For knn models (knn3 and knnreg), added automatic conversion of data frames to matrices

## Changes in version 4.72

- A new function for `rfe` with [**gam**](https://CRAN.R-project.org/package=gam) was added.
- "Down-sampling" was implemented with `bag` so that, for classification models, each class has the same number of classes as the smallest class.
- Added a new class, `dummyVars`, that creates an entire set of binary dummy variables (instead of the reduced, full rank set). The initial code was suggested by Gabor Grothendieck on R-Help. The predict method is used to create dummy variables for any data set.
- Added `R2` and `RMSE` functions for evaluating regression models
- `varImp.gam` failed to recognize objects from [**mgcv**](https://CRAN.R-project.org/package=mgcv)
- a small fix to test a logical vector `filterVarImp`
- When `diff.resamples` calculated the number of comparisons, the `"models"` argument was ignored.
- `predict.bag` was ignoring `type = "prob"`
- Minor updates to conform to R 2.13.0

## Changes in version 4.70

- Added a warning to `train` when class levels are not valid R variable names.
- Fixed a bug in the variable importance function for `multinom` objects.
- Added p-value adjustments to `summary.diff.resamples`. Confidence intervals in `dotplot.diff.resamples` are adjusted accordingly if the Bonferroni is used.
- For `dotplot.resamples`, no point was plotted when the upper and/or lower interval values were NaN. Now, the point is plotted but without the interval bars.
- Updated `print.rfe` to correctly describe new resampling methods.

## Changes in version 4.69

- Fixed a bug in `predict.rfe` where an error was thrown even though the required predictors were in `newdata`.
- Changed `preProcess` so that centering and scaling are both automatic when PCA or ICA are requested.

## Changes in version 4.68

- Added two functions, `checkResamples` and `checkConditionalX` that identify predictor data with degenerate distributions when conditioned on a factor.
- Added a high content screening data set (`segmentedData`) from Hill et al. Impact of image segmentation on high-content screening data quality for SK-BR-3 cells. BMC bioinformatics (2007) vol. 8 (1) pp. 340.
- Fixed bugs in how `sbf` objects were printed (when using repeated CV) and classification models with [**earth**](https://CRAN.R-project.org/package=earth) and `classProbs = TRUE`.

## Changes in version 4.67

- Added `predict.rfe`
- Added imputation using bagged regression trees to `preProcess`.
- Fixed bug in `varImp.rfe` that caused incorrect results (thanks to Lawrence Mosley for the find).

## Changes in version 4.65

- Fixed a bug where `train` would not allow knn imputation.
- `filterVarImp` and `roc` now check for missing values and use complete data for each predictor (instead of case- wise deletion across all predictors).

## Changes in version 4.64

- Fixed bug introduced in the last version with `createDataPartition(... list = FALSE)`.
- Fixed a bug predicting class probabilities when using [**earth**](https://CRAN.R-project.org/package=earth)/glm models
- Fixed a bug that occurred when `train` was used with `ctree` or `tree2` methods.
- Fixed bugs in `rfe` and `sbf` when running in parallel; not all the resampling results were saved

## Changes in version 4.63

- A p-value from McNemar's test was added to `confusionMatrix`.
- Updated `print.train` so that constant parameters are not shown in the table (but a note is written below the table instead). Also, the output was changed slightly to be more easily read (I hope)
- Adapted `varImp.gam` to work with either [**mgcv**](https://CRAN.R-project.org/package=mgcv) or [**gam**](https://CRAN.R-project.org/package=gam) packages.
- Expanded the tuning parameters for `lvq`.
- Some of the examples in the Model Building vignette were changed
- Added bootstrap 632 rule and repeated cross-validation to `trainControl`.
- A new function, `createMultiFolds`, is used to generate indices for repeated CV.
- The various resampling functions now have \*named\* lists as output (with prefixes "Fold" for cv and repeated cv and "Resample" otherwise)
- Pre-processing has been added to `train` with the `preProcess` argument. This has been tested when caret function are used with `rfe` and `sbf` (via `caretFuncs` and `caretSBF`, respectively).
- When `preProcess(method = "spatialSign")`, centering and scaling is done automatically too. Also, a bug was fixed that stopped the transformation from being executed.
- knn imputation was added to `preProcess`. The [**RANN**](https://CRAN.R-project.org/package=RANN) package is used to find the neighbors (the knn impute function in the impute library was consistently generating segmentation faults, so we wrote our own).
- Changed the behavior of `preProcess` in situations where scaling is requested but there is no variation in the predictor. Previously, the method would fail. Now a warning is issued and the value of the standard deviation is coerced to be one (so that scaling has no effect).

## Changes in version 4.62

- Added `gam` from [**mgcv**](https://CRAN.R-project.org/package=mgcv) (with smoothing splines and feature selection) and `gam` from [**gam**](https://CRAN.R-project.org/package=gam) (with basic splines and loess) smoothers. For these models, a formula is derived from the data where "near zero variance" predictors (see `nearZerVar`) are excluded and predictors with less than 10 distinct values are entered as linear (i.e. unsmoothed) terms.

## Changes in version 4.61

- Changed [**earth**](https://CRAN.R-project.org/package=earth) fit for classification models to use the `glm` argument with a binomial family.
- Added `varImp.multinom`, which is based on the absolute values of the model coefficients

## Changes in version 4.60

- The feature selection vignette was updated slightly (again).

## Changes in version 4.59

- Updated `rfe` and `sbf` to include class probabilities in performance calculations.
- Also, the names of the resampling indices were harmonized across `train`, `rfe` and `sbf`.
- The feature selection vignette was updated slightly.

## Changes in version 4.58

- Added the ability to include class probabilities in performance calculations. See `trainControl` and `twoClassSummary`.
- Updated and restructured the main vignette.

## Changes in version 4.57

- Internal changes related to how predictions from models are stored and summarized. With the exception of loo, the model performance values are calculated by the workers instead of the main program. This should reduce i/o and lay some groundwork for upcoming changes.
- The default grid for [**relaxo**](https://CRAN.R-project.org/package=relaxo) models were changed based on and initial model fit.
- [**partDSA**](https://CRAN.R-project.org/package=partDSA) model predictions were modified; there were cases where the user might request X partitions, but the model only produced Y < X. In these cases, the partitions for missing models were replaced with the largest model that was fit.
- The function `modelLookup` was put in the namespace and a man file was added.
- The names of the resample indices are automatically reset, even if the user specified them.

## Changes in version 4.56

- Fixed a bug generated a few versions ago where `varImp` for `plsda` and `fda` objects crashed.

## Changes in version 4.55

- When computing the scale parameter for RBF kernels, the option to automatically scale the data was changed to `TRUE`

## Changes in version 4.54

- Added `logic.bagging` in **logicFT** with `method = "logicBag"`

## Changes in version 4.53

- Fixed a bug in `varImp.train` related to nearest shrunken centroid models.
- Added logic regression and logic forests

## Changes in version 4.51

- Added an option to `splom.resamples` so that the variables in the scatter plots are models or metrics.

## Changes in version 4.50

- Added `dotplot.resamples` plus acknowledgements to Hothorn et al. (2005) and Eugster et al. (2008)

## Changes in version 4.49

- Enhanced the `tuneGrid` option to allow a function to be passed in.

## Changes in version 4.48

- Added a `prcomp` method for the `resamples` class

## Changes in version 4.47

- Extended `resamples` to work with `rfe` and `sbf`

## Changes in version 4.46

- Cleaned up some of the man files for the resamples class and added `parallel.resamples`.
- Fixed a bug in `diff.resamples` where `...` were not being passed to the test statistic function.
- Added more log messages in `train` when running verbose.
- Added the German credit data set.

## Changes in version 4.45

- Added a general framework for bagging models via the `bag` function. Also, model type `"hdda"` from the [**HDclassif**](https://CRAN.R-project.org/package=HDclassif) package was added.

## Changes in version 4.44

- Added [**neuralnet**](https://CRAN.R-project.org/package=neuralnet), `quantregForest` and `rda` (from [**rda**](https://CRAN.R-project.org/package=rda)) to `train`. Since there is a naming conflict with `rda` from [**mda**](https://CRAN.R-project.org/package=mda), the [**rda**](https://CRAN.R-project.org/package=rda) model was given a method value of `"scrda"`.

## Changes in version 4.43

- Tthe resampling estimate of the standard deviation given by `train` since v 4.39 was wrong
- A new field was added to `varImp.mvr` called `"estimate"`. In cases where the mvr model had multiple estimates of performance (e.g. training set, CV, etc) the user can now select which estimate they want to be used in the importance calculation (thanks to Sophie Bréand for finding this)

## Changes in version 4.42

- Added `predict.sbf` and modified the structure of the `sbf` helper functions. The `"score"` function only computes the metric used to filter and the filter function does the actual filtering. This was changed so that FDR corrections or other operations that use all of the p-values can be computed.
- Also, the formatting of p-values in `print.confusionMatrix` was changed
- An argument was added to `maxDissim` so that the variable name is returned instead of the index.
- Independent component analysis was added to the list of pre-processing operations and a new model ("icr") was added to fit a pcr-like model with the ICA components.

## Changes in version 4.40

- Added `hda` and cleaned up the [**caret**](https://CRAN.R-project.org/package=caret) training vignette

## Changes in version 4.39

- Added several classes for examining the resampling results. There are methods for estimating pair-wise differences and lattice functions for visualization. The training vignette has a new section describing the new features.

## Changes in version 4.38

- Added [**partDSA**](https://CRAN.R-project.org/package=partDSA) and `stepAIC` for linear models and generalized linear models

## Changes in version 4.37

- Fixed a new bug in how resampling results are exported

## Changes in version 4.36

- Added penalized linear models from the [**foba**](https://CRAN.R-project.org/package=foba) package

## Changes in version 4.35

- Added `rocc` classification and fixed a typo.

## Changes in version 4.34

- Added two new data sets: `dhfr` and `cars`

## Changes in version 4.33

- Added GAMens (ensembles using gams)
- Fixed a bug in `roc` that, for some data cases, would reverse the "positive" class and report sensitivity as specificity and vice-versa.

## Changes in version 4.32

- Added a parallel random forest method in `train` using the [**foreach**](https://CRAN.R-project.org/package=foreach) package.
- Also added penalized logistic regression using the `plr` function in the [**stepPlr**](https://CRAN.R-project.org/package=stepPlr) package.

## Changes in version 4.31

- Added a new feature selection function, `sbf` (for selection by filter).
- Fixed bug in `rfe` that did not affect the results, but did produce a warning.
- A new model function, `nullModel`, was added. This model fits either the mean only model for regression or the majority class model for classification.
- Also, ldaFuncs had a bug fixed.
- Minor changes to Rd files

## Changes in version 4.30

- For whatever reason, there is now a function in the [**spls**](https://CRAN.R-project.org/package=spls) package by the name of splsda that does the same thing. A few functions and a man page were changed to ensure backwards compatibility.

## Changes in version 4.29

- Added stepwise variable selection for `lda` and `qda` using the `stepclass` function in [**klaR**](https://CRAN.R-project.org/package=klaR)

## Changes in version 4.28

- Added robust linear and quadratic discriminant analysis functions from [**rrcov**](https://CRAN.R-project.org/package=rrcov).
- Also added another column to the output of `extractProb` and `extractPrediction` that saves the name of the model object so that you can have multiple models of the same type and tell which predictions came from which model.
- Changes were made to `plotClassProbs`: new parameters were added and densityplots can now be produced.

## Changes in version 4.27

- Added [**nodeHarvest**](https://CRAN.R-project.org/package=nodeHarvest)

## Changes in version 4.26

- Fixed a bug in `caretFunc` that led to NaN variable rankings, so that the first k terms were always selected.

## Changes in version 4.25

- Added parallel processing functionality for `rfe`

## Changes in version 4.24

- Added the ability to use custom metrics with `rfe`

## Changes in version 4.22

- Many Rd changes to work with updated parser.

## Changes in version 4.21

- Re-saved data in more compressed format

## Changes in version 4.20

- Added `pcr` as a method

## Changes in version 4.19

- Weights argument was added to `train` for models that accept weights
- Also, a bug was fixed for lasso regression (wrong lambda specification) and other for prediction in naive Bayes models with a single predictor.

## Changes in version 4.18

- Fixed bug in new `nearZeroVar` and updated `format.earth` so that it does not automatically print the formula

## Changes in version 4.17

- Added a new version of `nearZeroVar` from Allan Engelhardt that is much faster

## Changes in version 4.16

- Fixed bugs in `extractProb` (for glmnet) and `filterVarImp`.
- For glmnet, the user can now pass in their own value of family to `train` (otherwise `train` will set it depending on the mode of the outcome). However, glmnet doesn't have much support for families at this time, so you can't change links or try other distributions.

## Changes in version 4.15

- Fixed bug in `createFolds` when the smallest y value is more than 25 of the data

## Changes in version 4.14

- Fixed bug in `print.train`

## Changes in version 4.13

- Added vbmp from [**vbmp**](https://CRAN.R-project.org/package=vbmp) package

## Changes in version 4.12

- Added additional error check to `confusionMatrix`
- Fixed an absurd typo in `print.confusionMatrix`

## Changes in version 4.11

- Added: linear kernels for svm, rvm and Gaussian processes; `rlm` from [**MASS**](https://CRAN.R-project.org/package=MASS); a knn regression model, knnreg
- A set of functions (class "`classDist`") to computes the class centroids and covariance matrix for a training set for determining Mahalanobis distances of samples to each class centroid was added
- a set of functions (`rfe`) for doing recursive feature selection (aka backwards selection). A new vignette was added for more details

## Changes in version 4.10

- Added `OneR` and `PART` from [**RWeka**](https://CRAN.R-project.org/package=RWeka)

## Changes in version 4.09

- Fixed error in documentation for `confusionMatrix`. The old doc had `"Detection Prevalence = A/(A+B)"` and the new one has `"Detection Prevalence =(A+B)(A+B+C+D)"`. The underlying code was correct.
- Added `lars` (`fraction` and `step` as parameters)

## Changes in version 4.08

- Updated `train` and `bagEarth` to allow `earth` for classification models

## Changes in version 4.07

- Added [**glmnet**](https://CRAN.R-project.org/package=glmnet) models

## Changes in version 4.06

- Added code for sparse PLS classification.
- Fix a bug in prediction for `caTools::LogitBoost`

## Changes in version 4.05

- Updated again for more stringent R CMD check tests in R-devel 2.9

## Changes in version 4.04

- Updated for more stringent R CMD check tests in R-devel 2.9

## Changes in version 4.03

- Significant internal changes were made to how the models are fit. Now, the function used to compute the models is passed in as a parameter (defaulting to `lapply`). In this way, users can use their own parallel processing software without new versions of [**caret**](https://CRAN.R-project.org/package=caret). Examples are given in `train`.
- Also, fixed a bug where the MSE (instead of RMSE) was reported for random forest OOB resampling
- There are more examples in `train`.
- Changes to `confusionMatrix`, `sensitivity`, `specificity` and the predictive value functions: each was made more generic with default and `table` methods; `confusionMatrix` "extractor" functions for matrices and tables were added; the pos/neg predicted value computations were changed to incorporate prevalence; prevalence was added as an option to several functions; detection rate and prevalence statistics were added to `confusionMatrix`; and the examples were expanded in the help files.
- This version of caret will break compatibility with **caretLSF** and **caretNWS**. However, these packages will not be needed now and will be deprecated.

## Changes in version 3.51

- Updated the man files and manuals.

## Changes in version 3.50

- Added `qda`, `mda` and `pda`.

## Changes in version 3.49

- Fixed bug in `resampleHist`. Also added a check in the `train` functions that error trapped with `glm` models and > 2 classes

## Changes in version 3.48

- Added `glm`s. Also, added `varImp.bagEarth` to the namespace.

## Changes in version 3.47

- Added `sda` from the [**sda**](https://CRAN.R-project.org/package=sda) package. There was a naming conflict between `sda::sda` and `sparseLDA:::sda`. The method value for `sparseLDA` was changed from "sda" to "sparseLDA".

## Changes in version 3.46

- Added `spls` from the [**spls**](https://CRAN.R-project.org/package=spls) package

## Changes in version 3.45

- Added caching of [**RWeka**](https://CRAN.R-project.org/package=RWeka) objects to that they can be saved to the file system and used in other sessions. (changes per Kurt Hornik on 2008-10-05)

## Changes in version 3.44

- Added `sda` from the [**sparseLDA**](https://CRAN.R-project.org/package=sparseLDA) package (not on CRAN).
- Also, a bug was fixed where the ellipses were not passed into a few of the newer models (such as `penalized` and `ppr`)

## Changes in version 3.43

- Added the penalized model from the [**penalized**](https://CRAN.R-project.org/package=penalized) package. In [**caret**](https://CRAN.R-project.org/package=caret), it is regression only although the package allows for classification via glm models. However, it does not allow the user to pass the classes in (just an indicator matrix). Because of this, it doesn't really work with the rest of the classification tools in the package.

## Changes in version 3.42

- Added a little more formatting to `print.train`

## Changes in version 3.41

- For `gbm`, let the user over-ride the default value of the `distribution` argument (brought us by Peter Tait via RHelp).

## Changes in version 3.40

- Changed `predict.preProcess` so that it doesn't crash if `newdata` does not have all of the variables used to originally pre-process \*unless\* PCA processing was requested.

## Changes in version 3.39

- Fixed bug in `varImp.rpart` when the model had only primary splits.
- Minor changes to the Affy normalization code
- Changed typo in `predictors` man page

## Changes in version 3.38

- Added a new class called `predictors` that returns the names of the predictors that were used in the final model.
- Also added `ppr` from the `stats` package.
- Minor update to the project web page to deal with IE issues

## Changes in version 3.37

- Added the ability of `train` to use custom made performance functions so that the tuning parameters can be chosen on the basis of things other than RMSE/R-squared and Accuracy/Kappa.
- A new argument was added to `trainControl` called "summaryFunction" that is used to specify the function used to compute performance metrics. The default function preserves the functionality prior to this new version
- a new argument to `train` is "maximize" which is a logical for whether the performance measure specified in the "metric" argument to `train` should be maximized or minimized.
- The selection function specified in `trainControl` carries the maximize argument with it so that customized performance metrics can be used.
- A bug was fixed in `confusionMatrix` (thanks to Gabor Grothendieck)
- Another bug was fixed related to predictions from least square SVMs

## Changes in version 3.36

- Added `superpc` from the [**superpc**](https://CRAN.R-project.org/package=superpc) package. One note: the `data` argument that is passed to `superpc` is saved in the object that results from `superpc.train`. This is used later in the prediction function.

## Changes in version 3.35

- Added `slda` from [**ipred**](https://CRAN.R-project.org/package=ipred).

## Changes in version 3.34

- Fixed a few bugs related to the lattice plots from version 3.33.
- Also added the ripper (aka `JRip`) and logistic model trees from [**RWeka**](https://CRAN.R-project.org/package=RWeka)

## Changes in version 3.33

- Added `xyplot.train`, `densityplot.train`, `histogram.train` and `stripplot.train`. These are all functions to plot the resampling points. There is some overlap between these functions, `plot.train` and `resampleHist`. `plot.train` gives the average metrics only while these plot all of the resampled performance metrics. `resampleHist` could plot all of the points, but only for the final optimal set of predictors.
- To use these functions, there is a new argument in `trainControl` called `returnResamp` which should have values "none", "final" and "all". The default is "final" to be consistent with previous versions, but "all" should be specified to use these new functions to their fullest.

## Changes in version 3.32

- The functions `predict.train` and `predict.list` were added to use as alternatives to the `extractPrediction` and `extractProbs` functions.
- Added C4.5 (aka `J48`) and rules-based models (M5 prime) from [**RWeka**](https://CRAN.R-project.org/package=RWeka).
- Also added `logitBoost` from the [**caTools**](https://CRAN.R-project.org/package=caTools) package. This package doesn't have a namespace and [**RWeka**](https://CRAN.R-project.org/package=RWeka) has a function with the same name. It was suggested to use the "::" prefix to differentiate them (but we'll see how this works).

