# predict.train checks the type it is asked for

    Code
      predict(fit, reg, type = "class")
    Condition
      Error in `predict.train()`:
      ! type must be either "raw" or "prob"

---

    Code
      predict(fit, reg, type = "prob")
    Condition
      Error in `predict.train()`:
      ! only classification models that produce probabilities are allowed

# predict.train needs data when none was kept

    Code
      predict(fit)
    Condition
      Error in `predict.train()`:
      ! please specify data via newdata

# predict.train updates an object from an old caret version

    The model was updated to work with the current version of caret. Please re-create the model object since future versions will require objects to be created from caret versions >= 6. Alternatively, do not update caret beyond version 5.17-7.

