# Shared datasets (trim_class_*, trim_reg_*) live in helper-trim.R

test_that('rpart classification', {
  skip_on_cran()
  skip_if_not_installed("rpart")
  set.seed(9521)
  class_trim <- train(
    Class ~ .,
    data = trim_class_tr,
    method = "rpart",
    tuneGrid = data.frame(cp = 0.22),
    trControl = trainControl(method = "none", classProbs = TRUE, trim = TRUE)
  )

  set.seed(9521)
  class_notrim <- train(
    Class ~ .,
    data = trim_class_tr,
    method = "rpart",
    tuneGrid = data.frame(cp = 0.22),
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

test_that('rpart regression', {
  skip_on_cran()
  skip_if_not_installed("rpart")
  set.seed(1387)
  reg_trim <- train(
    y ~ .,
    data = trim_reg_tr,
    method = "rpart",
    tuneGrid = data.frame(cp = 0.12),
    trControl = trainControl(method = "none", trim = TRUE)
  )

  set.seed(1387)
  reg_notrim <- train(
    y ~ .,
    data = trim_reg_tr,
    method = "rpart",
    tuneGrid = data.frame(cp = 0.12),
    trControl = trainControl(method = "none", trim = FALSE)
  )
  expect_equal(predict(reg_trim, trim_reg_te), predict(reg_notrim, trim_reg_te))
  expect_lt(object.size(reg_trim), object.size(reg_notrim))
})


test_that('rpart2 classification', {
  skip_on_cran()
  skip_if_not_installed("rpart")
  set.seed(4476)
  class_trim <- train(
    Class ~ .,
    data = trim_class_tr,
    method = "rpart2",
    tuneGrid = data.frame(maxdepth = 3),
    trControl = trainControl(method = "none", classProbs = TRUE, trim = TRUE)
  )

  set.seed(4476)
  class_notrim <- train(
    Class ~ .,
    data = trim_class_tr,
    method = "rpart2",
    tuneGrid = data.frame(maxdepth = 3),
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

test_that('rpart2 regression', {
  skip_on_cran()
  skip_if_not_installed("rpart")
  set.seed(8039)
  reg_trim <- train(
    y ~ .,
    data = trim_reg_tr,
    method = "rpart2",
    tuneGrid = data.frame(maxdepth = 3),
    trControl = trainControl(method = "none", trim = TRUE)
  )

  set.seed(8039)
  reg_notrim <- train(
    y ~ .,
    data = trim_reg_tr,
    method = "rpart2",
    tuneGrid = data.frame(maxdepth = 3),
    trControl = trainControl(method = "none", trim = FALSE)
  )
  expect_equal(predict(reg_trim, trim_reg_te), predict(reg_notrim, trim_reg_te))
  expect_lt(object.size(reg_trim), object.size(reg_notrim))
})
