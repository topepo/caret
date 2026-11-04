# var_seq builds a default grid of predictor counts

    Code
      vs <- caret:::var_seq(2, classification = FALSE, len = 3)
    Output
      note: only 1 unique complexity parameters in default grid. Truncating the grid to 1 .
      

# check_dims requires x rows to match the outcome length

    Code
      caret:::check_dims(m, 1:3)
    Condition
      Error in `caret:::check_dims()`:
      ! nrow(x) == n is not TRUE

---

    Code
      caret:::check_dims(matrix(1:3, nrow = 1), 1)
    Condition
      Error in `caret:::check_dims()`:
      ! nrow(x) > 1 is not TRUE

# printCall prints the call under a 'Call:' header

    Code
      caret:::printCall(quote(train(y ~ x, data = dat)))
    Output
      
      Call:
      train(y ~ x, data = dat)
      

# fail_warning warns about a failed model fit

    Code
      caret:::fail_warning(list(method = "knn"), "boom", iter = 1, verb = FALSE)
    Condition
      Warning:
      model fit failed for 1: =knn boom

# check_na_conflict warns when imputation clashes with na.action

    Code
      caret:::check_na_conflict(quote(train(y ~ x, na.action = na.omit, preProcess = "knnImpute")))
    Condition
      Warning:
      `preProcess` includes an imputation method but missing data will be eliminated by the formula method using `na.action=na.omit`. Consider using `na.actin=na.pass` instead.

# requireNamespaceQuietStop asks for a missing package by name

    Code
      caret:::requireNamespaceQuietStop("notARealPackage")
    Condition
      Error:
      ! package notARealPackage is required

# parse_sampling rejects anything it cannot use

    Code
      caret:::parse_sampling(1:3)
    Condition
      Error:
      ! The sampling argument should be either a string, function, or list. See http://topepo.github.io/caret/model-training-and-tuning.html

---

    Code
      caret:::parse_sampling("not_a_scheme")
    Condition
      Error:
      ! That sampling scheme is not in caret's built-in library

# check_samp_func insists on arguments x and y

    Code
      caret:::check_samp_func(function(x) NULL)
    Condition
      Error:
      ! the 'sampling' function should have arguments 'x' and 'y'

---

    Code
      caret:::check_samp_func(function(a, b) NULL)
    Condition
      Error:
      ! the 'sampling' function should have arguments 'x' and 'y'

# check_samp_list insists on the three elements it needs

    Code
      caret:::check_samp_list(good[c("name", "func")])
    Condition
      Error:
      ! the 'sampling' list should have elements first, func, name

---

    Code
      caret:::check_samp_list(wrong)
    Condition
      Error:
      ! the 'sampling' list should have elements first, func, name

---

    Code
      caret:::check_samp_list(bad_first)
    Condition
      Error:
      ! The element 'first' should be a logical

# fail_warning reports the settings that failed

    model fit failed for Fold1: shift=1, scale=2 Error in try(stop("first problem"), silent = TRUE) : first problem
    model fit failed for Fold1: shift=1, scale=2 Error in try(stop("second problem"), silent = TRUE) : second problem
    

---

    Code
      wrn <- caret:::fail_warning(settings, "boom", where = "predictions", iter = "Fold2",
        verb = TRUE)
    Output
      predictions failed for Fold2: shift=1, scale=2 boom 
    Condition
      Warning:
      predictions failed for Fold2: shift=1, scale=2 boom

# evalSummaryFunction validates its arguments

    Code
      caret:::evalSummaryFunction(rnorm(20), perf = 1:20, ctrl = ctrl, lev = NULL,
      metric = "RMSE", method = "lm")
    Condition
      Error:
      ! `perf` should be a data frame

---

    Code
      caret:::evalSummaryFunction(factor(rep(c("a", "b"), 10)), ctrl = ctrl, lev = c(
        "a", "b"), metric = "ROC", method = "glm")
    Condition
      Error in `caret:::evalSummaryFunction()`:
      ! train()'s use of ROC codes requires class probabilities. See the classProbs option of trainControl()

# get_resample_perf needs the resamples to have been saved

    Code
      caret:::get_resample_perf(obj)
    Condition
      Error:
      ! no resampled results were saved; use a `returnResamp` other than 'none' in trainControl()

---

    Code
      caret:::get_resample_perf(obj)
    Condition
      Error:
      ! no resampled results were saved; use a `returnResamp` other than 'none' in trainControl()

---

    Code
      caret:::get_resample_perf(obj)
    Condition
      Error:
      ! no resampled results were saved; use a `returnResamp` other than 'none' in trainControl()

# parallel_check warns about multicore with the wrong package

    Models using rJava will not work with parallel processing with multicore/doMC

# check_dims and get_range read a survival outcome by row

    Code
      caret:::check_dims(x[1:3, ], y)
    Condition
      Error in `caret:::check_dims()`:
      ! nrow(x) == n is not TRUE

