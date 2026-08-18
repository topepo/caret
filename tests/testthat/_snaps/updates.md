# safs updating

    Code
      update(sa_xy, iter = new_iter)
    Condition
      Error in `update.safs()`:
      ! argument "x" is missing, with no default

---

    Code
      update(sa_rec, iter = new_iter)
    Condition
      Error:
      ! Recipe is missing data to be juiced.

# gafs updating

    Code
      update(ga_xy, iter = new_iter)
    Condition
      Error in `update.gafs()`:
      ! argument "x" is missing, with no default

---

    Code
      update(ga_rec, iter = new_iter)
    Condition
      Error:
      ! Recipe is missing data to be juiced.

# rfe updating

    The saved resamples are no longer appropriate and were removed

---

    Code
      update(rfe_xy, size = 5)
    Condition
      Error in `update.rfe()`:
      ! argument "x" is missing, with no default

---

    The saved resamples are no longer appropriate and were removed

---

    Code
      update(rfe_rec, size = 5)
    Condition
      Error:
      ! Recipe is missing data to be juiced.

# update.train needs the training data to re-fit

    Code
      update(fit, list(intercept = FALSE))
    Condition
      Error in `update.train()`:
      ! original training data is needed; use returnData = TRUE in trainControl()

# update.train checks the parameters it is given

    Code
      update(fit, 3)
    Condition
      Error in `update.train()`:
      ! param should be a data frame or a named list

---

    Code
      update(fit, data.frame(intercept = c(TRUE, FALSE)))
    Condition
      Error in `update.train()`:
      ! only one set of parameters should be specified

---

    Code
      update(fit, list(intercept = TRUE, extra = 1))
    Condition
      Error in `update.train()`:
      ! Parameters should be intercept

---

    Code
      update(fit, list(wrong_name = TRUE))
    Condition
      Error in `update.train()`:
      ! Parameters should be intercept

# update.train fills in the model information of an old object

    The model was updated to work with the current version of caret. Please re-create the model object since future versions will require objects to be created from caret versions >= 6. Alternatively, do not update caret beyond version 5.17-7.

# update.train cannot rescue an old object of an unknown type

    Code
      update(old)
    Condition
      Error in `update.train()`:
      ! This appears to be from an old version of caret and the model type is unknown to the new version

