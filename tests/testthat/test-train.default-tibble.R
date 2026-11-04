# train() accepts tibbles through the recipe and formula interfaces. Shared
# fixtures (tibble_dat, tibble_df, tibble_rec, tibble_ctrl) live in
# helper-tibble.R; the tibble sampling helpers are tested in
# test-misc-sampling.R.

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
