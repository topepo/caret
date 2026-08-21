# filter_on_corr errors with a single model

    Code
      caret:::filter_on_corr(one, "RMSE", cutoff = 0.9)
    Condition
      Error in `matrix()`:
      ! non-numeric matrix extent

# adaptive resampling reports a model that fails in every resample

    Code
      train(reg[, 1:3], reg$y, method = always, tuneLength = 2, trControl = trainControl(
        method = "adaptive_cv", number = 4, adaptive = list(min = 2, alpha = 0.05,
          method = "gls", complete = TRUE)))
    Condition
      Warning:
      model fit failed for Fold1.Rep1: shift=1, scale=1 Error : fit failed on purpose
      Warning:
      model fit failed for Fold1.Rep1: shift=2, scale=1 Error : fit failed on purpose
      Warning in `adaptiveWorkflow()`:
      There were missing values in resampled performance measures.
      Error:
      ! Something is wrong; all the RMSE metric values are missing in the resamples the race starts from, so the candidate models cannot be compared.

# adaptive resampling passes the workflow debug flag through

    Code
      fit <- suppressWarnings(train(reg[, 1:3], reg$y, method = tolerant, tuneLength = 2,
      trControl = trainControl(method = "adaptive_cv", number = 4, adaptive = list(
        min = 2, alpha = 0.05, method = "gls", complete = TRUE)), testing = TRUE))
    Output
      pre-model
      [1] 1 1 1 1 1 1
      pre-model
      [1] 2 2 2 2 2 2
      [1] 1 1 1 1 1 1
      [1] 2 2 2 2 2 2
      pre-model
      [1] 1 1 1 1 1 1
      pre-model
      [1] 1 1 1 1 1 1

# skunked drops the models that never win

    Code
      out <- caret:::skunked(scores)
    Output
      o 1 model was skunked

# adaptive resampling reports its progress

    Code
      fit <- suppressWarnings(train(Species ~ ., data = cls, method = "knn",
      tuneGrid = data.frame(k = c(1, 9, 17)), trControl = trainControl(method = "adaptive_cv",
        number = 5, verboseIter = TRUE, adaptive = list(min = 3, alpha = 0.05,
          method = "gls", complete = TRUE))))
    Output
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
      o 1 eliminated;2 remain
      + Fold5.Rep1: k=1 
      - Fold5.Rep1: k=1 
      + Fold5.Rep1: k=9 
      - Fold5.Rep1: k=9 
      o no models eliminated; 2 remain
      Aggregating results
      Selecting tuning parameters
      Fitting k = 1 on full training set

# adaptive resampling carries on when a model fails in one resample

    Code
      fit <- train(reg[, 1:3], reg$y, method = sometimes, tuneLength = 2, trControl = trainControl(
        method = "adaptive_cv", number = 5, adaptive = list(min = 3, alpha = 0.05,
          method = "gls", complete = TRUE)))
    Condition
      Warning in `adaptiveWorkflow()`:
      There were missing values in resampled performance measures.
      Warning:
      model fit failed for Fold3.Rep1: shift=1, scale=1 Error : fit failed on purpose
      Warning:
      model fit failed for Fold3.Rep1: shift=2, scale=1 Error : fit failed on purpose
      Warning in `adaptiveWorkflow()`:
      There were missing values in resampled performance measures.

# the diversity filters can report what they dropped

    Code
      keep_corr <- caret:::filter_on_corr(adapt_results, "RMSE", cutoff = 0.9,
        verbose = TRUE)
    Output
      o 1 model was eliminated due to linear dependencies

---

    Code
      keep_diff <- caret:::filter_on_diff(adapt_results, "RMSE", cutoff = 0.1,
        maximize = FALSE, verbose = TRUE)
    Output
      o 1 model of 4 was eliminated due to linear dependencies

# the race fills in sub-model predictions when a fit fails

    Code
      fit <- train(dat[, 1:3], dat$y, method = failing, tuneLength = 3, trControl = trainControl(
        method = "adaptive_cv", number = 5, classProbs = TRUE, savePredictions = "all",
        adaptive = list(min = 3, alpha = 0.05, method = "gls", complete = TRUE)))
    Condition
      Warning:
      model fit failed for Fold3.Rep1: shift=1, scale=1 Error : fit failed on purpose
      Warning in `adaptiveWorkflow()`:
      There were missing values in resampled performance measures.

# the race fills in sub-model predictions when prediction fails

    Code
      fit <- train(dat[, 1:3], dat$y, method = bad_pred, tuneLength = 3, trControl = trainControl(
        method = "adaptive_cv", number = 5, adaptive = list(min = 3, alpha = 0.05,
          method = "gls", complete = TRUE)))
    Condition
      Warning:
      predictions failed for Fold3.Rep1: shift=1, scale=1 Error : predict failed on purpose
      Warning in `adaptiveWorkflow()`:
      There were missing values in resampled performance measures.

