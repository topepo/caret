# oob_pred needs saved predictions

    Code
      caret:::oob_pred(fit)
    Condition
      Error in `oob_pred.train()`:
      ! re-fit the model using 'trainControl(savePredictions=TRUE)'

# oob_pred.rfe and oob_pred.sbf need saved details

    Code
      caret:::oob_pred(no_pred_rfe)
    Condition
      Error in `oob_pred.rfe()`:
      ! re-fit the model using 'rfeControl(saveDetails=TRUE)'

---

    Code
      caret:::oob_pred(no_pred_sbf)
    Condition
      Error in `oob_pred.sbf()`:
      ! re-fit the model using 'rfeControl(saveDetails=TRUE)'

# oob_pred.list rejects models with different resampling footprints

    Code
      caret:::oob_pred(list(a = m1, b = m2))
    Condition
      Error in `oob_pred.list()`:
      ! Some averages have different sample sizes than others

