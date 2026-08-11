# learning_curve_dat requires an outcome column name

    Code
      learning_curve_dat(dat)
    Condition
      Error in `learning_curve_dat()`:
      ! Please give a character stirng for the outcome column name

# learning_curve_dat rejects method = 'none'

    Code
      learning_curve_dat(dat, outcome = "Class", proportion = c(0.5, 1), verbose = FALSE,
      method = "knn", tuneGrid = data.frame(k = 5), trControl = trainControl(method = "none",
        classProbs = TRUE))
    Condition
      Error:
      ! `learning_curve_dat` uses resampling so please choose a value of `method` that is not 'none'

