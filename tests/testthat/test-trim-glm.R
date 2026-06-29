# Shared datasets (trim_class_*, trim_reg_*) live in helper-trim.R

test_that('glm classification', {
  skip_on_cran()
  set.seed(3162)
  class_trim <- train(
    Class ~ .,
    data = trim_class_tr,
    method = "glm",
    tuneLength = 1,
    trControl = trainControl(method = "none", classProbs = TRUE, trim = TRUE)
  )

  set.seed(3162)
  class_notrim <- train(
    Class ~ .,
    data = trim_class_tr,
    method = "glm",
    tuneLength = 1,
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

test_that('glm regression', {
  skip_on_cran()
  set.seed(7748)
  reg_trim <- train(
    y ~ .,
    data = trim_reg_tr,
    method = "glm",
    tuneLength = 1,
    trControl = trainControl(method = "none", trim = TRUE)
  )

  set.seed(7748)
  reg_notrim <- train(
    y ~ .,
    data = trim_reg_tr,
    method = "glm",
    tuneLength = 1,
    trControl = trainControl(method = "none", trim = FALSE)
  )
  expect_equal(predict(reg_trim, trim_reg_te), predict(reg_notrim, trim_reg_te))
  expect_lt(object.size(reg_trim), object.size(reg_notrim))
})
