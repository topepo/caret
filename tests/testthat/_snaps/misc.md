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

