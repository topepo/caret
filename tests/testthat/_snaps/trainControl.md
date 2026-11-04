# resampling method 'none' doesn't conflict with default tuneLength

    Code
      train(bbbDescr, logBBB, method = "earth", tuneLength = 2, trControl = trainControl(
        method = "none"))
    Condition
      Error:
      ! Only one model should be specified in tuneGrid with no resampling

---

    Code
      train(mpg ~ cyl + disp, data = mtcars, method = "gam", tuneLength = 2,
      trControl = trainControl(method = "none"))
    Condition
      Error:
      ! Only one model should be specified in tuneGrid with no resampling

# trainControl checks the arguments it can

    Code
      trainControl(selectionFunction = NULL)
    Condition
      Error in `trainControl()`:
      ! null selectionFunction values not allowed

---

    Code
      trainControl(returnResamp = "some")
    Condition
      Error in `trainControl()`:
      ! incorrect value of returnResamp

---

    Code
      trainControl(predictionBounds = c(0, 10, 20))
    Condition
      Error in `trainControl()`:
      ! 'predictionBounds' should be a logical or numeric vector of length 2

---

    Code
      trainControl(search = "sobol")
    Condition
      Error in `trainControl()`:
      ! `search` should be either 'grid' or 'random'

# trainControl keeps preProcess options for preProcess

    Code
      trainControl(preProcOptions = list(method = "range"))
    Condition
      Error in `trainControl()`:
      ! 'method' cannot be specified here

---

    Code
      trainControl(preProcOptions = list(x = iris[, 1:4]))
    Condition
      Error in `trainControl()`:
      ! 'x' cannot be specified here

# trainControl warns when repeats cannot be used

    `repeats` has no meaning for this resampling method.

# trainControl checks the adaptive resampling settings

    Code
      trainControl(adaptive = adapt(method = "lm"))
    Condition
      Error in `trainControl()`:
      ! incorrect value of adaptive$method

---

    Code
      trainControl(adaptive = adapt(alpha = 2))
    Condition
      Error in `trainControl()`:
      ! incorrect value of adaptive$alpha

---

    Code
      trainControl(method = "adaptive_cv", number = 5, repeats = 1, adaptive = adapt(
        min = 5))
    Condition
      Error in `trainControl()`:
      ! adaptive$min should be less than 5

---

    Code
      trainControl(method = "adaptive_boot", number = 25, adaptive = adapt(min = 1))
    Condition
      Error in `trainControl()`:
      ! adaptive$min should be greater than 1

# trainControl warns that out-of-bag resampling has fixed measures

    Custom summary measures cannot be computed for out-of-bag resampling. This value of `summaryFunction` will be ignored.

