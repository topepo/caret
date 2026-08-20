# bad class levels

    Code
      foo(dat)
    Condition
      Error:
      ! At least one of the class levels is not a valid R variable name; This will cause errors when class probabilities are generated because the variables names will be converted to  X0, X1 . Please use factor levels that can be used as valid R variable names  (see ?make.names for help).

# no class probs with ROC

    Code
      foo(dat)
    Condition
      Error:
      ! Class probabilities are needed to score models using the area under the ROC curve. Set `classProbs = TRUE` in the trainControl() function.

# numeric y and classification

    You are trying to do regression and your outcome only has two possible values Are you trying to do classification? If so, use a 2 level factor as your outcome column.

# 3+ classes and twoClassSummary

    Code
      foo()
    Condition
      Error in `ctrl$summaryFunction()`:
      ! Your outcome has 7 levels. The twoClassSummary() function isn't appropriate.

# train needs column names and a usable outcome

    Code
      train(x, y, method = "lm")
    Condition
      Error:
      ! Please use column names for `x`

---

    Code
      train(x, letters[1:10], method = "lm")
    Condition
      Error:
      ! wrong model type for classification

# train checks a custom method list for its required parts

    Code
      train(dat[, 1:3], dat$y, method = incomplete)
    Condition
      Error:
      ! some required components are missing: predict, prob

# train rejects an unknown model name

    Code
      train(dat[, 1:3], dat$y, method = "nosuchmodel99")
    Condition
      Error:
      ! Model nosuchmodel99 is not in caret's built-in library

# train rejects a model that cannot handle the outcome type

    Code
      train(dat[, 1:3], dat$y, method = "lda")
    Condition
      Error:
      ! wrong model type for regression

# train rejects unknown preProcess methods

    Code
      train(dat[, 1:3], dat$y, method = "lm", preProcess = "bogus")
    Condition
      Warning:
      model fit failed for Resample01: intercept=TRUE Error in pre_process_options(method, column_types) : 
        These pre-processing methods are unknown: 'bogus'
      Warning:
      model fit failed for Resample02: intercept=TRUE Error in pre_process_options(method, column_types) : 
        These pre-processing methods are unknown: 'bogus'
      Warning:
      model fit failed for Resample03: intercept=TRUE Error in pre_process_options(method, column_types) : 
        These pre-processing methods are unknown: 'bogus'
      Warning:
      model fit failed for Resample04: intercept=TRUE Error in pre_process_options(method, column_types) : 
        These pre-processing methods are unknown: 'bogus'
      Warning:
      model fit failed for Resample05: intercept=TRUE Error in pre_process_options(method, column_types) : 
        These pre-processing methods are unknown: 'bogus'
      Warning:
      model fit failed for Resample06: intercept=TRUE Error in pre_process_options(method, column_types) : 
        These pre-processing methods are unknown: 'bogus'
      Warning:
      model fit failed for Resample07: intercept=TRUE Error in pre_process_options(method, column_types) : 
        These pre-processing methods are unknown: 'bogus'
      Warning:
      model fit failed for Resample08: intercept=TRUE Error in pre_process_options(method, column_types) : 
        These pre-processing methods are unknown: 'bogus'
      Warning:
      model fit failed for Resample09: intercept=TRUE Error in pre_process_options(method, column_types) : 
        These pre-processing methods are unknown: 'bogus'
      Warning:
      model fit failed for Resample10: intercept=TRUE Error in pre_process_options(method, column_types) : 
        These pre-processing methods are unknown: 'bogus'
      Warning:
      model fit failed for Resample11: intercept=TRUE Error in pre_process_options(method, column_types) : 
        These pre-processing methods are unknown: 'bogus'
      Warning:
      model fit failed for Resample12: intercept=TRUE Error in pre_process_options(method, column_types) : 
        These pre-processing methods are unknown: 'bogus'
      Warning:
      model fit failed for Resample13: intercept=TRUE Error in pre_process_options(method, column_types) : 
        These pre-processing methods are unknown: 'bogus'
      Warning:
      model fit failed for Resample14: intercept=TRUE Error in pre_process_options(method, column_types) : 
        These pre-processing methods are unknown: 'bogus'
      Warning:
      model fit failed for Resample15: intercept=TRUE Error in pre_process_options(method, column_types) : 
        These pre-processing methods are unknown: 'bogus'
      Warning:
      model fit failed for Resample16: intercept=TRUE Error in pre_process_options(method, column_types) : 
        These pre-processing methods are unknown: 'bogus'
      Warning:
      model fit failed for Resample17: intercept=TRUE Error in pre_process_options(method, column_types) : 
        These pre-processing methods are unknown: 'bogus'
      Warning:
      model fit failed for Resample18: intercept=TRUE Error in pre_process_options(method, column_types) : 
        These pre-processing methods are unknown: 'bogus'
      Warning:
      model fit failed for Resample19: intercept=TRUE Error in pre_process_options(method, column_types) : 
        These pre-processing methods are unknown: 'bogus'
      Warning:
      model fit failed for Resample20: intercept=TRUE Error in pre_process_options(method, column_types) : 
        These pre-processing methods are unknown: 'bogus'
      Warning:
      model fit failed for Resample21: intercept=TRUE Error in pre_process_options(method, column_types) : 
        These pre-processing methods are unknown: 'bogus'
      Warning:
      model fit failed for Resample22: intercept=TRUE Error in pre_process_options(method, column_types) : 
        These pre-processing methods are unknown: 'bogus'
      Warning:
      model fit failed for Resample23: intercept=TRUE Error in pre_process_options(method, column_types) : 
        These pre-processing methods are unknown: 'bogus'
      Warning:
      model fit failed for Resample24: intercept=TRUE Error in pre_process_options(method, column_types) : 
        These pre-processing methods are unknown: 'bogus'
      Warning:
      model fit failed for Resample25: intercept=TRUE Error in pre_process_options(method, column_types) : 
        These pre-processing methods are unknown: 'bogus'
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
       NAs:1 NAs:1 NAs:1 
    Condition
      Error:
      ! Stopping

# train rejects an outcome level with no data

    Code
      train(dat[, 1:4], y, method = "lda")
    Condition
      Error:
      ! One or more factor levels in the outcome has no data: 'empty'

# train rejects metrics that do not match the outcome

    Code
      train(cls[, 1:4], cls$Species, method = "lda", metric = "RMSE")
    Condition
      Error:
      ! Metric RMSE not applicable for classification models

---

    Code
      train(reg[, 1:3], reg$y, method = "lm", metric = "Accuracy")
    Condition
      Error:
      ! Metric Accuracy not applicable for regression models

# train refuses sampling for a regression outcome

    Code
      train(reg[, 1:3], reg$y, method = "lm", trControl = trainControl(sampling = "down"))
    Condition
      Error:
      ! sampling methods are only implemented for classification problems

# train refuses out-of-bag estimates for models without them

    Code
      train(reg[, 1:3], reg$y, method = "lm", trControl = trainControl(method = "oob"))
    Condition
      Error:
      ! Out of bag estimates are not implemented for this model

# train warns about a two-valued numeric outcome

    You are trying to do regression and your outcome only has two possible values Are you trying to do classification? If so, use a 2 level factor as your outcome column.

# train drops class probabilities it cannot produce

    Class probabilities were requested for a model that does not implement them

# train drops class probabilities for a regression outcome

    cannnot compute class probabilities for regression

