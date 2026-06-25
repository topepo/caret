test_that('train runs on tibbles and recipes with glm', {
  skip_on_cran()
  skip_if_not_installed("dplyr")
  set.seed(8801)
  dat <- twoClassSim(100)
  df <- data.frame(a = dat[, 5], y = dat[["Class"]], stringsAsFactors = TRUE)
  rec <- recipe(y ~ ., data = df)
  ctrl <- trainControl(
    method = "repeatedcv",
    repeats = 5,
    classProbs = TRUE,
    summaryFunction = twoClassSummary
  )

  expect_no_error(
    train(
      rec,
      data = dplyr::as_tibble(df),
      method = "glm",
      family = "binomial",
      metric = "ROC",
      trControl = ctrl
    )
  )
})

test_that('train runs on tibbles and formulas with glm', {
  skip_on_cran()
  skip_if_not_installed("dplyr")
  set.seed(8801)
  dat <- twoClassSim(100)
  df <- data.frame(a = dat[, 5], y = dat[["Class"]], stringsAsFactors = TRUE)
  ctrl <- trainControl(
    method = "repeatedcv",
    repeats = 5,
    classProbs = TRUE,
    summaryFunction = twoClassSummary
  )

  expect_no_error(
    train(
      y ~ .,
      data = dplyr::as_tibble(df),
      method = "glm",
      family = "binomial",
      metric = "ROC",
      trControl = ctrl
    )
  )
})

test_that('downsampling on tibble', {
  skip_on_cran()
  skip_if_not_installed("dplyr")
  set.seed(8801)
  dat <- twoClassSim(100)
  dat_tb <- dplyr::as_tibble(dat)

  expect_no_error(
    caret:::parse_sampling("down")$func(dat_tb[, 1], dat_tb$Class)
  )
})

test_that('upsampling on tibble', {
  skip_on_cran()
  skip_if_not_installed("dplyr")
  set.seed(8801)
  dat <- twoClassSim(100)
  dat_tb <- dplyr::as_tibble(dat)

  expect_no_error(
    caret:::parse_sampling("up")$func(dat_tb[, 1], dat_tb$Class)
  )
})

# check these manually to avoid more dependencies
# caret:::parse_sampling("smote")$func(dat_tb[, 1], dat_tb$Class)
# caret:::parse_sampling("rose")$func(dat_tb[, 1], dat_tb$Class)
