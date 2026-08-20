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

