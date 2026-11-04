# progress reports the parameters of the current resample

    Code
      caret:::progress(params, names = c("Fold1", "Fold2"), iter = 1)
    Output
      + Fold1: k=5 

---

    Code
      caret:::progress(params, names = c("Fold1", "Fold2"), iter = 2, start = FALSE)
    Output
      - Fold2: k=5 

# leave-one-out resampling reports a fit that fails for one row

    model fit failed for Fold16: shift=1, scale=1 Error : fit failed on purpose
    

# leave-one-out resampling reports predictions that fail

    predictions failed for Fold16: shift=1, scale=1 Error : predict failed on purpose
    

# leave-one-out resampling reports its progress and debug trace

    Code
      fit <- suppressWarnings(train(reg[, 1:3], reg$y, method = tolerant, tuneLength = 2,
      trControl = trainControl(method = "LOOCV", verboseIter = TRUE), testing = TRUE))
    Output
      after loops
      + Fold01: shift=1, scale=1 
      [1] 1
      - Fold01: shift=1, scale=1 
      after loops
      + Fold01: shift=2, scale=1 
      [1] 2
      - Fold01: shift=2, scale=1 
      after loops
      + Fold02: shift=1, scale=1 
      [1] 1
      - Fold02: shift=1, scale=1 
      after loops
      + Fold02: shift=2, scale=1 
      [1] 2
      - Fold02: shift=2, scale=1 
      after loops
      + Fold03: shift=1, scale=1 
      [1] 1
      - Fold03: shift=1, scale=1 
      after loops
      + Fold03: shift=2, scale=1 
      [1] 2
      - Fold03: shift=2, scale=1 
      after loops
      + Fold04: shift=1, scale=1 
      [1] 1
      - Fold04: shift=1, scale=1 
      after loops
      + Fold04: shift=2, scale=1 
      [1] 2
      - Fold04: shift=2, scale=1 
      after loops
      + Fold05: shift=1, scale=1 
      [1] 1
      - Fold05: shift=1, scale=1 
      after loops
      + Fold05: shift=2, scale=1 
      [1] 2
      - Fold05: shift=2, scale=1 
      after loops
      + Fold06: shift=1, scale=1 
      [1] 1
      - Fold06: shift=1, scale=1 
      after loops
      + Fold06: shift=2, scale=1 
      [1] 2
      - Fold06: shift=2, scale=1 
      after loops
      + Fold07: shift=1, scale=1 
      [1] 1
      - Fold07: shift=1, scale=1 
      after loops
      + Fold07: shift=2, scale=1 
      [1] 2
      - Fold07: shift=2, scale=1 
      after loops
      + Fold08: shift=1, scale=1 
      [1] 1
      - Fold08: shift=1, scale=1 
      after loops
      + Fold08: shift=2, scale=1 
      [1] 2
      - Fold08: shift=2, scale=1 
      after loops
      + Fold09: shift=1, scale=1 
      [1] 1
      - Fold09: shift=1, scale=1 
      after loops
      + Fold09: shift=2, scale=1 
      [1] 2
      - Fold09: shift=2, scale=1 
      after loops
      + Fold10: shift=1, scale=1 
      [1] 1
      - Fold10: shift=1, scale=1 
      after loops
      + Fold10: shift=2, scale=1 
      [1] 2
      - Fold10: shift=2, scale=1 
      after loops
      + Fold11: shift=1, scale=1 
      [1] 1
      - Fold11: shift=1, scale=1 
      after loops
      + Fold11: shift=2, scale=1 
      [1] 2
      - Fold11: shift=2, scale=1 
      after loops
      + Fold12: shift=1, scale=1 
      [1] 1
      - Fold12: shift=1, scale=1 
      after loops
      + Fold12: shift=2, scale=1 
      [1] 2
      - Fold12: shift=2, scale=1 
      Aggregating results
      Selecting tuning parameters
      Fitting shift = 1, scale = 1 on full training set

