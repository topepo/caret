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

# train rejects a recipe whose outcome the model cannot handle

    Code
      train(rec, data = reg, method = "lda")
    Condition
      Error:
      ! wrong model type for regression

# train wants a character matrix for the string kernels

    Code
      train(rec, data = reg, method = "svmSpectrumString")
    Condition
      Error:
      ! 'x_dat' should be a character matrix with a single column for string kernel methods

# train warns about a two-valued numeric outcome from a recipe

    You are trying to do regression and your outcome only has two possible values Are you trying to do classification? If so, use a 2 level factor as your outcome column.

# train refuses sampling for a recipe with a numeric outcome

    Code
      train(rec, data = reg, method = "lm", trControl = trainControl(method = "cv",
        number = 3, sampling = "down"))
    Condition
      Error:
      ! sampling methods are only implemented for classification problems

# train rejects a recipe outcome level with no data

    Code
      train(rec, data = cls, method = "lda")
    Condition
      Error:
      ! One or more factor levels in the outcome has no data: 'empty'

# train needs valid level names for recipe class probabilities

    Code
      train(rec, data = cls, method = "lda", trControl = trainControl(method = "cv",
        number = 3, classProbs = TRUE))
    Condition
      Error:
      ! At least one of the class levels is not a valid R variable name; This will cause errors when class probabilities are generated because the variables names will be converted to  one, X2.two, three . Please use factor levels that can be used as valid R variable names  (see ?make.names for help).

# train checks the metric against the recipe's outcome type

    Code
      train(cls_rec, data = cls, method = "lda", metric = "RMSE")
    Condition
      Error:
      ! Metric RMSE not applicable for classification models

---

    Code
      train(reg_rec, data = reg, method = "lm", metric = "Kappa")
    Condition
      Error:
      ! Metric Kappa not applicable for regression models

---

    Code
      train(cls_rec, data = cls, method = "lda", metric = "ROC")
    Condition
      Error:
      ! Class probabilities are needed to score models using the area under the ROC curve. Set `classProbs = TRUE` in the trainControl() function.

# train drops recipe class probabilities it cannot produce

    Class probabilities were requested for a model that does not implement them

# train drops class probabilities for a recipe regression outcome

    cannnot compute class probabilities for regression

# train validates the recipe fit's other options

    Code
      train(rec, data = reg, method = "lm", trControl = trainControl(method = "cv",
        savePredictions = "some"))
    Condition
      Error:
      ! `savePredictions` should be either logical or "all", "final" or "none"

---

    Code
      train(rec, data = reg, method = "knn", tuneGrid = data.frame(k = 5), trControl = trainControl(
        method = "adaptive_cv", number = 4))
    Condition
      Error in `trainControl()`:
      ! adaptive$min should be less than 4

# train checks a recipe fit's tuning grid

    Code
      train(rec, data = cls, method = "knn", tuneGrid = data.frame(bogus = 5))
    Condition
      Error:
      ! The tuning parameter grid should have columns k

---

    Code
      train(rec, data = cls, method = "knn", tuneGrid = data.frame(k = 5, bogus = 1))
    Condition
      Error:
      ! The tuning parameter grid should have columns k

# train checks what a recipe model's loop function returns

    Code
      train(rec, data = reg, method = bad_loop, tuneLength = 2)
    Condition
      Error:
      ! The 'loop' function should produce a list with elements 'loop' and 'submodels'

# the recipe race fills in sub-model predictions when a fit fails

    Code
      fit <- train(rec, data = dat, method = failing, tuneLength = 3, trControl = trainControl(
        method = "adaptive_cv", number = 5, classProbs = TRUE, savePredictions = "all",
        adaptive = list(min = 3, alpha = 0.05, method = "gls", complete = TRUE)))
    Condition
      Warning:
      model fit failed for Fold2.Rep1: shift=1, scale=1 Error : fit failed on purpose
      Warning in `train_adapt_rec()`:
      There were missing values in resampled performance measures.
      Warning in `train_adapt_rec()`:
      There were missing values in resampled performance measures.

# the recipe race fills in sub-models when prediction fails

    Code
      fit <- train(rec, data = dat, method = bad_pred, tuneLength = 3, trControl = trainControl(
        method = "adaptive_cv", number = 5, adaptive = list(min = 3, alpha = 0.05,
          method = "gls", complete = TRUE)))
    Condition
      Warning:
      predictions failed for Fold2.Rep1: shift=1, scale=1 Error : predict failed on purpose
      Warning in `train_adapt_rec()`:
      There were missing values in resampled performance measures.
      Warning in `train_adapt_rec()`:
      There were missing values in resampled performance measures.

# the recipe workflows report a sub-model fit that fails

    Code
      fit <- train(rec, data = dat, method = failing, tuneLength = 3, trControl = trainControl(
        method = "cv", number = 5, classProbs = TRUE, savePredictions = "all"))
    Condition
      Warning:
      model fit failed for Fold5: shift=1, scale=1 Error : fit failed on purpose
      Warning in `train_rec()`:
      There were missing values in resampled performance measures.

