# These run adaptive resampling the way someone actually would - through train()
# - and check we get a sensible model back. It's the only route that reaches
# adaptiveWorkflow() (and the diversity filters underneath it), so we keep the
# runs tiny and skip when the optional racing packages aren't around.

adaptive_knn_fit <- function(eval_method) {
  set.seed(1)
  suppressWarnings(train(
    Species ~ .,
    data = iris,
    method = "knn",
    tuneGrid = data.frame(k = c(1, 5, 9, 13, 17)),
    trControl = trainControl(
      method = "adaptive_cv",
      number = 10,
      adaptive = list(
        min = 3,
        alpha = 0.05,
        method = eval_method,
        complete = TRUE
      )
    )
  ))
}

test_that("adaptive_cv tuning runs end to end with the gls racer", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  fit <- adaptive_knn_fit("gls")

  expect_s3_class(fit, "train")
  expect_equal(fit$control$method, "adaptive_cv")
  expect_false(is.null(fit$finalModel))
  # every candidate still shows up in the results, and the winner is one of them
  expect_equal(sort(fit$results$k), c(1, 5, 9, 13, 17))
  expect_true(fit$bestTune$k %in% fit$results$k)
  # and the model we get back can actually make predictions
  expect_length(predict(fit, iris), nrow(iris))
})

test_that("adaptive_cv tuning runs end to end with the Bradley-Terry racer", {
  skip_on_cran()
  skip_if_not_installed("BradleyTerry2")

  fit <- adaptive_knn_fit("BT")

  expect_s3_class(fit, "train")
  expect_true(fit$bestTune$k %in% fit$results$k)
  expect_length(predict(fit, iris), nrow(iris))
})
