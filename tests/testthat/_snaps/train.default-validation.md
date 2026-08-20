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
      Error:
      ! pre-processing methods are limited to: BoxCox, YeoJohnson, expoTrans, invHyperbolicSine, center, scale, range, knnImpute, bagImpute, medianImpute, pca, ica, spatialSign, ignore, keep, remove, zv, nzv, conditionalX, corr

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

# train needs a numeric or factor outcome

    Code
      train(reg[, 1:3], rep(c(TRUE, FALSE), 10), method = "lda")
    Condition
      Error:
      ! Please make sure that the outcome column is a factor or numeric. The class(es) of the column: 'logical'

---

    Code
      train(reg[, 1:3], as.Date("2020-01-01") + 1:20, method = "lda")
    Condition
      Error:
      ! Please make sure that the outcome column is a factor or numeric. The class(es) of the column: 'Date'

# train wants a character matrix for the string kernels

    Code
      train(strings, y, method = "svmSpectrumString")
    Condition
      Error:
      ! Please use column names for `x`

---

    Code
      train(numeric_x, y, method = "svmSpectrumString")
    Condition
      Error:
      ! 'x' should be a character matrix with a single column for string kernel methods

---

    Code
      train(data.frame(a = strings), y, method = "svmSpectrumString")
    Condition
      Error:
      ! 'x' should be a character matrix with a single column for string kernel methods

# train checks the columns of a supplied tuning grid

    Code
      train(Species ~ ., data = cls, method = "knn", tuneGrid = data.frame(bogus = 5))
    Condition
      Error:
      ! The tuning parameter grid should have columns k

---

    Code
      train(Species ~ ., data = cls, method = "knn", tuneGrid = data.frame(k = 5,
        bogus = 1))
    Condition
      Error:
      ! The tuning parameter grid should have columns k

# train checks the savePredictions option

    Code
      train(reg[, 1:3], reg$y, method = "lm", trControl = trainControl(method = "cv",
        savePredictions = "some"))
    Condition
      Error:
      ! `savePredictions` should be either logical or "all", "final" or "none"

# train needs more than one candidate for adaptive resampling

    Code
      train(Species ~ ., data = cls, method = "knn", tuneGrid = data.frame(k = 5),
      trControl = trainControl(method = "adaptive_cv", number = 4, adaptive = list(
        min = 2, alpha = 0.05, method = "gls", complete = TRUE)))
    Condition
      Error:
      ! For adaptive resampling, there needs to be more than one tuning parameter for evaluation

# train checks what a model's loop function returns

    Code
      train(reg[, 1:3], reg$y, method = bad_loop, tuneLength = 2)
    Condition
      Error:
      ! The 'loop' function should produce a list with elements 'loop' and 'submodels'

# train falls back when the metric is not in the results

    The metric "RMSE" was not in the result set. MedianError will be used instead.

