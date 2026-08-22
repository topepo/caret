# rfe runs and its methods behave (default interface)

    Code
      print(rf)
    Output
      
      Recursive feature selection
      
      Outer resampling method: Cross-Validated (3 fold) 
      
      Resampling performance over subset size:
      
       Variables Accuracy  Kappa AccuracySD KappaSD Selected
               2   0.7133 0.3786    0.07024 0.14768         
               4   0.7533 0.4720    0.06429 0.13298         
               6   0.8467 0.6749    0.04619 0.09194        *
               8   0.8467 0.6767    0.02309 0.04100         
      
      The top 5 variables (out of 6):
         TwoFactor2, TwoFactor1, Linear02, Linear03, Linear04
      

# rfeIter validates its arguments

    Code
      rfeIter(unnamed, y, testX = x, testY = y, sizes = 2)
    Condition
      Error in `rfeIter()`:
      ! x must have column names

---

    Code
      rfeIter(x, y, testX = NULL, testY = y, sizes = 2)
    Condition
      Error in `rfeIter()`:
      ! a test set must be specified

---

    Code
      rfeIter(x, y, testX = x, testY = y, sizes = NULL)
    Condition
      Error in `rfeIter()`:
      ! please specify the number of features

# rfe refuses a case-weight role

    Code
      rfe(rec, data = reg, sizes = c(1, 2), rfeControl = rfeControl(functions = lmFuncs,
        method = "cv", number = 3))
    Condition
      Error:
      ! `rfe` does not allow for weights.

# the rfe resampling plots need saved resamples

    explicit 'data' specification ignored

# the rfe resampling plots refuse leave-one-out results

    Code
      xyplot(rf)
    Condition
      Error in `xyplot.rfe()`:
      ! Resampling plots cannot be done with leave-out-out CV or out-of-bag resampling

---

    Code
      stripplot(rf)
    Condition
      Error in `stripplot.rfe()`:
      ! Resampling plots cannot be done with leave-out-out CV or out-of-bag resampling

---

    Code
      densityplot(rf)
    Condition
      Error in `densityplot.rfe()`:
      ! Resampling plots cannot be done with leave-out-out CV or out-of-bag resampling

---

    Code
      histogram(rf)
    Condition
      Error in `histogram.rfe()`:
      ! Resampling plots cannot be done with leave-out-out CV or out-of-bag resampling

# rfe checks the seeds it is given

    Code
      rfe(reg[, 1:3], reg$y, sizes = c(1, 2), rfeControl = rfeControl(functions = lmFuncs,
        method = "cv", index = folds, seeds = good[1:2]))
    Condition
      Error in `rfe.default()`:
      ! Bad seeds: the seed object should be a list of length 4 with 3 integer vectors of size 3 and the last list element having a single integer

---

    Code
      rfe(reg[, 1:3], reg$y, sizes = c(1, 2), rfeControl = rfeControl(functions = lmFuncs,
        method = "cv", index = folds, seeds = c(lapply(1:3, function(i) 1L), list(1L))))
    Condition
      Error in `rfe.default()`:
      ! Bad seeds: the seed object should be a list of length 4 with 3 integer vectors of size 3 and the last list element having a single integer

# rfe reports its progress

    Code
      fit <- rfe(reg[, 1:3], reg$y, sizes = c(1, 2), rfeControl = rfeControl(
        functions = lmFuncs, method = "cv", number = 2, verbose = TRUE))
    Output
      +(rfe) fit Fold1 size: 3 
      -(rfe) fit Fold1 size: 3 
      +(rfe) imp Fold1 
      -(rfe) imp Fold1 
      +(rfe) fit Fold1 size: 2 
      -(rfe) fit Fold1 size: 2 
      +(rfe) fit Fold1 size: 1 
      -(rfe) fit Fold1 size: 1 
      +(rfe) fit Fold2 size: 3 
      -(rfe) fit Fold2 size: 3 
      +(rfe) imp Fold2 
      -(rfe) imp Fold2 
      +(rfe) fit Fold2 size: 2 
      -(rfe) fit Fold2 size: 2 
      +(rfe) fit Fold2 size: 1 
      -(rfe) fit Fold2 size: 1 

# rfe reports its progress for a recipe

    Code
      fit <- rfe(rec, data = reg, sizes = c(1, 2), rfeControl = rfeControl(functions = lmFuncs,
        method = "cv", number = 2, verbose = TRUE))
    Output
      Preparing recipe
      +(rfe) Fold1 recipe 
      -(rfe) Fold1 recipe 
      +(rfe) fit Fold1 size: 3 
      -(rfe) fit Fold1 size: 3 
      +(rfe) imp Fold1 
      -(rfe) imp Fold1 
      +(rfe) fit Fold1 size: 2 
      -(rfe) fit Fold1 size: 2 
      +(rfe) fit Fold1 size: 1 
      -(rfe) fit Fold1 size: 1 
      +(rfe) Fold2 recipe 
      -(rfe) Fold2 recipe 
      +(rfe) fit Fold2 size: 3 
      -(rfe) fit Fold2 size: 3 
      +(rfe) imp Fold2 
      -(rfe) imp Fold2 
      +(rfe) fit Fold2 size: 2 
      -(rfe) fit Fold2 size: 2 
      +(rfe) fit Fold2 size: 1 
      -(rfe) fit Fold2 size: 1 

# predict.rfe needs the variables it selected

    Code
      predict(fit, without)
    Condition
      Error in `predict.rfe()`:
      ! missing columns from newdata: x1

