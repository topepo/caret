# get_vector passes vectors through and unwraps one-column frames

    Code
      caret:::get_vector(data.frame(a = 1:3, b = 4:6))
    Condition
      Error in `caret:::get_vector()`:
      ! Only one column should be available

# preproc_dots warns only about leftover preProc arguments

    Code
      caret:::preproc_dots(preProcOptions = list(k = 5))
    Condition
      Warning:
      When using a recipe with `train`, `preProcOptions` will be ignored.

# train() only allows one case-weight column

    Code
      train(rec, data = reg, method = "lm", trControl = trainControl(method = "cv",
        number = 3))
    Condition
      Error in `train.recipe()`:
      ! Ony one column can be used as a case weight.

# train() rejects a recipe with several outcomes

    Code
      train(rec, data = reg, method = "lm", trControl = trainControl(method = "cv",
        number = 3, sampling = "down"))
    Condition
      Error in `train.recipe()`:
      ! `train` doesn't support multivariate outcomes

# train() reports a recipe model that fails in a resample

    model fit failed for Fold3: shift=1, scale=1 Error : fit failed on purpose
    

# train() reports a recipe model whose predictions fail

    Code
      train(rec, data = reg, method = bad_predict, tuneLength = 2, trControl = trainControl(
        method = "cv", number = 3))
    Condition
      Warning:
      predictions failed for Fold1: shift=1, scale=1 Error : predict failed on purpose
      Warning:
      predictions failed for Fold1: shift=2, scale=1 Error : predict failed on purpose
      Warning:
      predictions failed for Fold2: shift=1, scale=1 Error : predict failed on purpose
      Warning:
      predictions failed for Fold2: shift=2, scale=1 Error : predict failed on purpose
      Warning:
      predictions failed for Fold3: shift=1, scale=1 Error : predict failed on purpose
      Warning:
      predictions failed for Fold3: shift=2, scale=1 Error : predict failed on purpose
      Warning in `train_rec()`:
      There were missing values in resampled performance measures.
    Output
      Something is wrong; all the RMSE metric values are missing:
            RMSE        Rsquared        MAE     
       Min.   : NA   Min.   : NA   Min.   : NA  
       1st Qu.: NA   1st Qu.: NA   1st Qu.: NA  
       Median : NA   Median : NA   Median : NA  
       Mean   :NaN   Mean   :NaN   Mean   :NaN  
       3rd Qu.: NA   3rd Qu.: NA   3rd Qu.: NA  
       Max.   : NA   Max.   : NA   Max.   : NA  
       NAs:2 NAs:2 NAs:2 
    Condition
      Error:
      ! Stopping

# train() prints its progress for a recipe

    Code
      fit <- train(rec, data = reg, method = tolerant, tuneLength = 2, trControl = trainControl(
        method = "cv", number = 2, verboseIter = TRUE))
    Output
      Preparing recipe
      + Fold1: shift=1, scale=1 
      - Fold1: shift=1, scale=1 
      + Fold1: shift=2, scale=1 
      - Fold1: shift=2, scale=1 
      + Fold2: shift=1, scale=1 
      - Fold2: shift=1, scale=1 
      + Fold2: shift=2, scale=1 
      - Fold2: shift=2, scale=1 
    Condition
      Warning in `train_rec()`:
      There were missing values in resampled performance measures.
    Output
      Aggregating results
      Selecting tuning parameters
      Fitting shift = 1, scale = 1 on full training set

# the recipe race reports a model that fails everywhere

    Code
      train(rec, data = reg, method = always, tuneLength = 2, trControl = trainControl(
        method = "adaptive_cv", number = 4, adaptive = list(min = 2, alpha = 0.05,
          method = "gls", complete = TRUE)))
    Condition
      Warning:
      model fit failed for Fold1.Rep1: shift=1, scale=1 Error : fit failed on purpose
      Warning:
      model fit failed for Fold1.Rep1: shift=2, scale=1 Error : fit failed on purpose
      Warning in `train_adapt_rec()`:
      There were missing values in resampled performance measures.
      Error:
      ! Something is wrong; all the RMSE metric values are missing in the resamples the race starts from, so the candidate models cannot be compared.

# the recipe race carries on when a model fails in one resample

    Code
      fit <- train(rec, data = reg, method = sometimes, tuneLength = 2, trControl = trainControl(
        method = "adaptive_cv", number = 5, adaptive = list(min = 3, alpha = 0.05,
          method = "gls", complete = TRUE)))
    Condition
      Warning:
      model fit failed for Fold2.Rep1: shift=1, scale=1 Error : fit failed on purpose
      Warning:
      model fit failed for Fold2.Rep1: shift=2, scale=1 Error : fit failed on purpose
      Warning in `train_adapt_rec()`:
      There were missing values in resampled performance measures.
      Warning in `train_adapt_rec()`:
      There were missing values in resampled performance measures.

# the recipe race reports its progress

    Code
      fit <- suppressWarnings(train(rec, data = cls, method = "knn", tuneGrid = data.frame(
        k = c(1, 9, 17)), trControl = trainControl(method = "adaptive_cv", number = 5,
        verboseIter = TRUE, adaptive = list(min = 3, alpha = 0.05, method = "gls",
          complete = TRUE))))
    Output
      Preparing recipe
      + Fold1.Rep1: k= 1 
      - Fold1.Rep1: k= 1 
      + Fold1.Rep1: k= 9 
      - Fold1.Rep1: k= 9 
      + Fold1.Rep1: k=17 
      - Fold1.Rep1: k=17 
      + Fold2.Rep1: k= 1 
      - Fold2.Rep1: k= 1 
      + Fold2.Rep1: k= 9 
      - Fold2.Rep1: k= 9 
      + Fold2.Rep1: k=17 
      - Fold2.Rep1: k=17 
      + Fold3.Rep1: k= 1 
      - Fold3.Rep1: k= 1 
      + Fold3.Rep1: k= 9 
      - Fold3.Rep1: k= 9 
      + Fold3.Rep1: k=17 
      - Fold3.Rep1: k=17 
      o no models eliminated; 3 remain
      + Fold4.Rep1: k= 1 
      - Fold4.Rep1: k= 1 
      + Fold4.Rep1: k= 9 
      - Fold4.Rep1: k= 9 
      + Fold4.Rep1: k=17 
      - Fold4.Rep1: k=17 
      o 1 model of 3 was eliminated due to linear dependencies
      o no models eliminated; 3 remain
      + Fold5.Rep1: k= 1 
      - Fold5.Rep1: k= 1 
      + Fold5.Rep1: k=17 
      - Fold5.Rep1: k=17 
      o 1 eliminated;1 remains
      Aggregating results
      Selecting tuning parameters
      Fitting k = 1 on full training set

# leave-one-out resampling reports a failing recipe model

    Code
      train(rec, data = small, method = always, tuneLength = 2, trControl = trainControl(
        method = "LOOCV"))
    Condition
      Warning:
      model fit failed for Fold01: shift=1, scale=1 Error : fit failed on purpose
      Warning:
      model fit failed for Fold01: shift=2, scale=1 Error : fit failed on purpose
      Warning:
      model fit failed for Fold02: shift=1, scale=1 Error : fit failed on purpose
      Warning:
      model fit failed for Fold02: shift=2, scale=1 Error : fit failed on purpose
      Warning:
      model fit failed for Fold03: shift=1, scale=1 Error : fit failed on purpose
      Warning:
      model fit failed for Fold03: shift=2, scale=1 Error : fit failed on purpose
      Warning:
      model fit failed for Fold04: shift=1, scale=1 Error : fit failed on purpose
      Warning:
      model fit failed for Fold04: shift=2, scale=1 Error : fit failed on purpose
      Warning:
      model fit failed for Fold05: shift=1, scale=1 Error : fit failed on purpose
      Warning:
      model fit failed for Fold05: shift=2, scale=1 Error : fit failed on purpose
      Warning:
      model fit failed for Fold06: shift=1, scale=1 Error : fit failed on purpose
      Warning:
      model fit failed for Fold06: shift=2, scale=1 Error : fit failed on purpose
      Warning:
      model fit failed for Fold07: shift=1, scale=1 Error : fit failed on purpose
      Warning:
      model fit failed for Fold07: shift=2, scale=1 Error : fit failed on purpose
      Warning:
      model fit failed for Fold08: shift=1, scale=1 Error : fit failed on purpose
      Warning:
      model fit failed for Fold08: shift=2, scale=1 Error : fit failed on purpose
      Warning:
      model fit failed for Fold09: shift=1, scale=1 Error : fit failed on purpose
      Warning:
      model fit failed for Fold09: shift=2, scale=1 Error : fit failed on purpose
      Warning:
      model fit failed for Fold10: shift=1, scale=1 Error : fit failed on purpose
      Warning:
      model fit failed for Fold10: shift=2, scale=1 Error : fit failed on purpose
      Warning:
      model fit failed for Fold11: shift=1, scale=1 Error : fit failed on purpose
      Warning:
      model fit failed for Fold11: shift=2, scale=1 Error : fit failed on purpose
      Warning:
      model fit failed for Fold12: shift=1, scale=1 Error : fit failed on purpose
      Warning:
      model fit failed for Fold12: shift=2, scale=1 Error : fit failed on purpose
    Output
      Something is wrong; all the RMSE metric values are missing:
         RMSE         Rsquared         MAE         
       Mode:logical   Mode:logical   Mode:logical  
       NAs:2 NAs:2 NAs:2 
    Condition
      Error:
      ! Stopping

