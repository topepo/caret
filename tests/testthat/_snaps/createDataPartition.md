# createDataPartition warns about empty and single-record classes

    Some classes have no records ( c ) and these will be ignored

---

    Some classes have a single record ( b ) and these will be selected for the sample

# createDataPartition needs at least two data points

    Code
      createDataPartition(factor("a"))
    Condition
      Error in `createDataPartition()`:
      ! y must have at least 2 data points

# groupKFold cannot ask for more folds than groups

    Code
      groupKFold(rep(letters[1:3], each = 2), k = 5)
    Condition
      Error in `groupKFold()`:
      ! `k` should be less than 3

# make_resamples validates user-supplied indices

    Code
      caret:::make_resamples(trainControl(method = "cv", index = list(Fold1 = c(1.5,
        2.5))), y)
    Condition
      Error:
      ! `index` should be lists of integers.

---

    Code
      caret:::make_resamples(trainControl(method = "cv", index = list(Fold1 = 1:5L),
      indexOut = list(Fold1 = c(6.5, 7.5))), y)
    Condition
      Error:
      ! `indexOut` should be lists of integers.

---

    Code
      caret:::make_resamples(trainControl(method = "custom"), y)
    Condition
      Error:
      ! 'custom' resampling is appropriate when the `trControl` argument `index` is used

---

    Code
      caret:::make_resamples(trainControl(method = "nope"), y)
    Condition
      Error:
      ! Not a recognized resampling method.

