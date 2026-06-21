# resampling method 'none' doesn't conflict with default tuneLength

    Code
      train(bbbDescr, logBBB, method = "earth", tuneLength = 2, trControl = trainControl(
        method = "none"))
    Condition
      Error:
      ! Only one model should be specified in tuneGrid with no resampling

---

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

