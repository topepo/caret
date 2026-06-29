# Shared fixtures (tibble_dat, tibble_df, tibble_rec, tibble_ctrl) live in helper-tibble.R

test_that('train runs on tibbles and recipes with glm', {
  skip_on_cran()
  skip_if_not_installed("dplyr")
  expect_no_error(
    train(
      tibble_rec,
      data = tibble_df,
      method = "glm",
      family = "binomial",
      metric = "ROC",
      trControl = tibble_ctrl
    )
  )
})

test_that('train runs on tibbles and formulas with glm', {
  skip_on_cran()
  skip_if_not_installed("dplyr")
  expect_no_error(
    train(
      y ~ .,
      data = tibble_df,
      method = "glm",
      family = "binomial",
      metric = "ROC",
      trControl = tibble_ctrl
    )
  )
})

test_that('downsampling on tibble', {
  skip_on_cran()
  skip_if_not_installed("dplyr")
  dat_tb <- dplyr::as_tibble(tibble_dat)
  expect_no_error(
    caret:::parse_sampling("down")$func(dat_tb[, 1], dat_tb$Class)
  )
})

test_that('upsampling on tibble', {
  skip_on_cran()
  skip_if_not_installed("dplyr")
  dat_tb <- dplyr::as_tibble(tibble_dat)
  expect_no_error(
    caret:::parse_sampling("up")$func(dat_tb[, 1], dat_tb$Class)
  )
})

# check these manually to avoid more dependencies
# caret:::parse_sampling("smote")$func(dat_tb[, 1], dat_tb$Class)
# caret:::parse_sampling("rose")$func(dat_tb[, 1], dat_tb$Class)
