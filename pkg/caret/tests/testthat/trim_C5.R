library(caret)

test_that('single tree', {
  skip_on_cran()
  set.seed(1)
  tr_dat <- twoClassSim(200)
  te_dat <- twoClassSim(200)
  grid <- data.frame(trials = 1,
                     model = "tree",
                     winnow = FALSE,
                     stringsAsFactors = TRUE)
  set.seed(2)
  class_trim <- train(Class ~ ., data = tr_dat,
                      method = "C5.0",
                      tuneGrid = grid,
                      trControl = trainControl(method = "none",
                                               classProbs = TRUE,
                                               trim = TRUE))

  set.seed(2)
  class_notrim <- train(Class ~ ., data = tr_dat,
                        method = "C5.0",
                        tuneGrid = grid,
                        trControl = trainControl(method = "none",
                                                 classProbs = TRUE,
                                                 trim = FALSE))

  expect_equal(predict(class_trim,   te_dat),
               predict(class_notrim, te_dat))

  expect_equal(predict(class_trim,   te_dat, type = "prob"),
               predict(class_notrim, te_dat, type = "prob"))

  expect_lt(object.size(class_trim)-object.size(class_notrim), 0)
})

test_that('single rule', {
  skip_on_cran()
  set.seed(1)
  tr_dat <- twoClassSim(200)
  te_dat <- twoClassSim(200)
  grid <- data.frame(trials = 1,
                     model = "rules",
                     winnow = FALSE,
                     stringsAsFactors = TRUE)
  set.seed(2)
  class_trim <- train(Class ~ ., data = tr_dat,
                      method = "C5.0",
                      tuneGrid = grid,
                      trControl = trainControl(method = "none",
                                               classProbs = TRUE,
                                               trim = TRUE))

  set.seed(2)
  class_notrim <- train(Class ~ ., data = tr_dat,
                        method = "C5.0",
                        tuneGrid = grid,
                        trControl = trainControl(method = "none",
                                                 classProbs = TRUE,
                                                 trim = FALSE))

  expect_equal(predict(class_trim,   te_dat),
               predict(class_notrim, te_dat))

  expect_equal(predict(class_trim,   te_dat, type = "prob"),
               predict(class_notrim, te_dat, type = "prob"))

  expect_lt(object.size(class_trim)-object.size(class_notrim), 0)
})

test_that('boosted tree', {
  skip_on_cran()
  set.seed(1)
  tr_dat <- twoClassSim(200)
  te_dat <- twoClassSim(200)
  grid <- data.frame(trials = 5,
                     model = "tree",
                     winnow = FALSE,
                     stringsAsFactors = TRUE)
  set.seed(2)
  class_trim <- train(Class ~ ., data = tr_dat,
                      method = "C5.0",
                      tuneGrid = grid,
                      trControl = trainControl(method = "none",
                                               classProbs = TRUE,
                                               trim = TRUE))

  set.seed(2)
  class_notrim <- train(Class ~ ., data = tr_dat,
                        method = "C5.0",
                        tuneGrid = grid,
                        trControl = trainControl(method = "none",
                                                 classProbs = TRUE,
                                                 trim = FALSE))

  expect_equal(predict(class_trim,   te_dat),
               predict(class_notrim, te_dat))

  expect_equal(predict(class_trim,   te_dat, type = "prob"),
               predict(class_notrim, te_dat, type = "prob"))

  expect_lt(object.size(class_trim)-object.size(class_notrim), 0)
})

test_that('boosted rule', {
  skip_on_cran()
  set.seed(1)
  tr_dat <- twoClassSim(200)
  te_dat <- twoClassSim(200)
  grid <- data.frame(trials = 5,
                     model = "rules",
                     winnow = FALSE,
                     stringsAsFactors = TRUE)
  set.seed(2)
  class_trim <- train(Class ~ ., data = tr_dat,
                      method = "C5.0",
                      tuneGrid = grid,
                      trControl = trainControl(method = "none",
                                               classProbs = TRUE,
                                               trim = TRUE))

  set.seed(2)
  class_notrim <- train(Class ~ ., data = tr_dat,
                        method = "C5.0",
                        tuneGrid = grid,
                        trControl = trainControl(method = "none",
                                                 classProbs = TRUE,
                                                 trim = FALSE))

  expect_equal(predict(class_trim,   te_dat),
               predict(class_notrim, te_dat))

  expect_equal(predict(class_trim,   te_dat, type = "prob"),
               predict(class_notrim, te_dat, type = "prob"))

  expect_lt(object.size(class_trim)-object.size(class_notrim), 0)
})



