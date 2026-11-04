# train warns when the metric is unavailable for out-of-bag fits

    The metric "MAE" was not in the result set. RMSE will be used instead.

# train reports its progress and can trim the final model

    Code
      fit <- train(y ~ ., data = reg, method = "rpart2", tuneGrid = data.frame(
        maxdepth = c(1, 2)), trControl = trainControl(method = "cv", number = 2,
        verboseIter = TRUE, trim = TRUE))
    Output
      + Fold1: maxdepth=2 
      - Fold1: maxdepth=2 
      + Fold2: maxdepth=2 
      - Fold2: maxdepth=2 
      Aggregating results
      Selecting tuning parameters
      Fitting maxdepth = 1 on full training set
      Final model footprint reduced by 0.9 Mb or 98% 

# train accepts user-supplied seeds and checks their shape

    Code
      train(Class ~ ., data = cls, method = "knn", tuneGrid = data.frame(k = c(3, 5)),
      trControl = do.call(trainControl, c(ctrl_args, list(seeds = too_short))))
    Condition
      Error:
      ! Bad seeds: the seed object should be a list of length 4 with 3 integer vectors of size 2 and the last list element having at least a single integer

---

    Code
      train(Class ~ ., data = cls, method = "knn", tuneGrid = data.frame(k = c(3, 5)),
      trControl = do.call(trainControl, c(ctrl_args, list(seeds = has_na))))
    Condition
      Error:
      ! At least one seed is missing (NA)

# train warns about resamples whose fit failed

    model fit failed for Fold1: shift=1, scale=1 Error : fit failed on purpose
    

# train stops when every resample fails

    Code
      train(reg[, 1:3], reg$y, method = always, tuneLength = 2, trControl = trainControl(
        method = "cv", number = 3))
    Condition
      Warning:
      model fit failed for Fold1: shift=1, scale=1 Error : fit failed on purpose
      Warning:
      model fit failed for Fold1: shift=2, scale=1 Error : fit failed on purpose
      Warning:
      model fit failed for Fold2: shift=1, scale=1 Error : fit failed on purpose
      Warning:
      model fit failed for Fold2: shift=2, scale=1 Error : fit failed on purpose
      Warning:
      model fit failed for Fold3: shift=1, scale=1 Error : fit failed on purpose
      Warning:
      model fit failed for Fold3: shift=2, scale=1 Error : fit failed on purpose
      Warning in `nominalTrainWorkflow()`:
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

# train warns when predictions fail in a resample

    Code
      train(reg[, 1:3], reg$y, method = bad_predict, tuneLength = 2, trControl = trainControl(
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
      Warning in `nominalTrainWorkflow()`:
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

# train passes the workflow debug flag through a tolerant fit

    Code
      fit <- train(reg[, 1:3], reg$y, method = tolerant, tuneLength = 2, trControl = trainControl(
        method = "cv", number = 2), testing = TRUE)
    Output
      pre-model
      $fit
      $lev
      [1] NA
      
      $n
      [1] 15
      
      $xNames
      [1] "x1" "x2" "x3"
      
      $problemType
      [1] "Regression"
      
      $tuneValue
        shift scale
      1     1     1
      
      $obsLevels
      [1] NA
      
      $param
      list()
      
      attr(,"class")
      [1] "custom_fit"
      
      $preProc
      NULL
      
      [1] 1 1 1 1 1 1
            RMSE Rsquared       MAE shift scale Resample
      1 1.140647       NA 0.8899587     1     1    Fold1
      pre-model
      $fit
      $lev
      [1] NA
      
      $n
      [1] 15
      
      $xNames
      [1] "x1" "x2" "x3"
      
      $problemType
      [1] "Regression"
      
      $tuneValue
        shift scale
      2     2     1
      
      $obsLevels
      [1] NA
      
      $param
      list()
      
      attr(,"class")
      [1] "custom_fit"
      
      $preProc
      NULL
      
      [1] 2 2 2 2 2 2
            RMSE Rsquared      MAE shift scale Resample
      2 1.950278       NA 1.751255     2     1    Fold1
      pre-model
      $fit
      $lev
      [1] NA
      
      $n
      [1] 15
      
      $xNames
      [1] "x1" "x2" "x3"
      
      $problemType
      [1] "Regression"
      
      $tuneValue
        shift scale
      1     1     1
      
      $obsLevels
      [1] NA
      
      $param
      list()
      
      attr(,"class")
      [1] "custom_fit"
      
      $preProc
      NULL
      
      [1] 1 1 1 1 1 1
            RMSE Rsquared      MAE shift scale Resample
      1 1.201969       NA 1.003703     1     1    Fold2
      pre-model
      $fit
      $lev
      [1] NA
      
      $n
      [1] 15
      
      $xNames
      [1] "x1" "x2" "x3"
      
      $problemType
      [1] "Regression"
      
      $tuneValue
        shift scale
      2     2     1
      
      $obsLevels
      [1] NA
      
      $param
      list()
      
      attr(,"class")
      [1] "custom_fit"
      
      $preProc
      NULL
      
      [1] 2 2 2 2 2 2
            RMSE Rsquared      MAE shift scale Resample
      2 2.032406       NA 1.842973     2     1    Fold2
    Condition
      Warning in `nominalTrainWorkflow()`:
      There were missing values in resampled performance measures.

# residuals.train uses the final model or falls back to predicting

    Code
      residuals(no_data)
    Condition
      Error:
      ! The training data must be saved to produce residuals

# residuals.train refuses classification models

    Code
      residuals(fit)
    Condition
      Error:
      ! train() only produces residuals on numeric outcomes

# train.formula stops when na.action removes every row

    Code
      train(y ~ ., data = reg, method = "lm", na.action = na.omit, trControl = trainControl(
        method = "cv", number = 3))
    Condition
      Error:
      ! Every row has at least one missing value were found

