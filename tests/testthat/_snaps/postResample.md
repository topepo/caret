# postResample handles degenerate inputs

    Code
      postResample(c(1, 2, 3), matrix(1:4, 2))
    Condition
      Error in `postResample()`:
      ! `pred` (length 3) and `obs` (length 4) must be the same length.

# twoClassSummary validates its levels and tolerates flat ROC input

    Code
      twoClassSummary(d, lev = c("A", "B"))
    Condition
      Error in `twoClassSummary()`:
      ! levels of observed and predicted data do not match

# mnLogLoss validates its inputs

    Code
      mnLogLoss(d, lev = NULL)
    Condition
      Error in `mnLogLoss()`:
      ! 'lev' cannot be NULL

---

    Code
      mnLogLoss(d, lev = c("A", "C"))
    Condition
      Error in `mnLogLoss()`:
      ! 'data' should have columns consistent with 'lev'

---

    Code
      mnLogLoss(d2, lev = c("A", "B"))
    Condition
      Error in `mnLogLoss()`:
      ! 'data$obs' should have levels consistent with 'lev'

# multiClassSummary validates levels and works without probabilities

    Code
      multiClassSummary(d, lev = c("A", "B"))
    Condition
      Error in `multiClassSummary()`:
      ! levels of observed and predicted data do not match

# train() gives NA performance for failed models

    Code
      fit <- train(Class ~ TwoFactor1 + TwoFactor2, data = dat, method = fail_glm,
      metric = "ROC", tuneGrid = data.frame(fail = 0:1), trControl = ctrl)
    Condition
      Warning:
      model fit failed for Fold1: fail=1 Error in method$fit(x = x, y = y, wts = wts, param = tuneValue, lev = obsLevels,  : 
        planned failure
      Warning:
      model fit failed for Fold2: fail=1 Error in method$fit(x = x, y = y, wts = wts, param = tuneValue, lev = obsLevels,  : 
        planned failure
      Warning:
      model fit failed for Fold3: fail=1 Error in method$fit(x = x, y = y, wts = wts, param = tuneValue, lev = obsLevels,  : 
        planned failure
      Warning in `nominalTrainWorkflow()`:
      There were missing values in resampled performance measures.
      Warning in `train.default()`:
      missing values found in aggregated results

