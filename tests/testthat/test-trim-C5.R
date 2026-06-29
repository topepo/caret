# Shared datasets (trim_class_*) live in helper-trim.R

test_that('single tree', {
  skip_on_cran()
  skip_if_not_installed("C50")
  grid <- data.frame(
    trials = 1,
    model = "tree",
    winnow = FALSE,
    stringsAsFactors = TRUE
  )
  set.seed(7382)
  class_trim <- train(
    Class ~ .,
    data = trim_class_tr,
    method = "C5.0",
    tuneGrid = grid,
    trControl = trainControl(method = "none", classProbs = TRUE, trim = TRUE)
  )

  set.seed(7382)
  class_notrim <- train(
    Class ~ .,
    data = trim_class_tr,
    method = "C5.0",
    tuneGrid = grid,
    trControl = trainControl(method = "none", classProbs = TRUE, trim = FALSE)
  )

  expect_equal(
    predict(class_trim, trim_class_te),
    predict(class_notrim, trim_class_te)
  )

  expect_equal(
    predict(class_trim, trim_class_te, type = "prob"),
    predict(class_notrim, trim_class_te, type = "prob")
  )

  expect_lt(object.size(class_trim), object.size(class_notrim))
})

test_that('single rule', {
  skip_on_cran()
  skip_if_not_installed("C50")
  grid <- data.frame(
    trials = 1,
    model = "rules",
    winnow = FALSE,
    stringsAsFactors = TRUE
  )
  set.seed(2918)
  class_trim <- train(
    Class ~ .,
    data = trim_class_tr,
    method = "C5.0",
    tuneGrid = grid,
    trControl = trainControl(method = "none", classProbs = TRUE, trim = TRUE)
  )

  set.seed(2918)
  class_notrim <- train(
    Class ~ .,
    data = trim_class_tr,
    method = "C5.0",
    tuneGrid = grid,
    trControl = trainControl(method = "none", classProbs = TRUE, trim = FALSE)
  )

  expect_equal(
    predict(class_trim, trim_class_te),
    predict(class_notrim, trim_class_te)
  )

  expect_equal(
    predict(class_trim, trim_class_te, type = "prob"),
    predict(class_notrim, trim_class_te, type = "prob")
  )

  expect_lt(object.size(class_trim), object.size(class_notrim))
})

test_that('boosted tree', {
  skip_on_cran()
  skip_if_not_installed("C50")
  grid <- data.frame(
    trials = 5,
    model = "tree",
    winnow = FALSE,
    stringsAsFactors = TRUE
  )
  set.seed(5564)
  class_trim <- train(
    Class ~ .,
    data = trim_class_tr,
    method = "C5.0",
    tuneGrid = grid,
    trControl = trainControl(method = "none", classProbs = TRUE, trim = TRUE)
  )

  set.seed(5564)
  class_notrim <- train(
    Class ~ .,
    data = trim_class_tr,
    method = "C5.0",
    tuneGrid = grid,
    trControl = trainControl(method = "none", classProbs = TRUE, trim = FALSE)
  )

  expect_equal(
    predict(class_trim, trim_class_te),
    predict(class_notrim, trim_class_te)
  )

  expect_equal(
    predict(class_trim, trim_class_te, type = "prob"),
    predict(class_notrim, trim_class_te, type = "prob")
  )

  expect_lt(object.size(class_trim), object.size(class_notrim))
})

test_that('boosted rule', {
  skip_on_cran()
  skip_if_not_installed("C50")
  grid <- data.frame(
    trials = 5,
    model = "rules",
    winnow = FALSE,
    stringsAsFactors = TRUE
  )
  set.seed(9047)
  class_trim <- train(
    Class ~ .,
    data = trim_class_tr,
    method = "C5.0",
    tuneGrid = grid,
    trControl = trainControl(method = "none", classProbs = TRUE, trim = TRUE)
  )

  set.seed(9047)
  class_notrim <- train(
    Class ~ .,
    data = trim_class_tr,
    method = "C5.0",
    tuneGrid = grid,
    trControl = trainControl(method = "none", classProbs = TRUE, trim = FALSE)
  )

  expect_equal(
    predict(class_trim, trim_class_te),
    predict(class_notrim, trim_class_te)
  )

  expect_equal(
    predict(class_trim, trim_class_te, type = "prob"),
    predict(class_notrim, trim_class_te, type = "prob")
  )

  expect_lt(object.size(class_trim), object.size(class_notrim))
})
