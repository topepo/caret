# Shared fixtures (recipe_fs_x, recipe_fs_dat) live in helper-recipe_fs.R

# ------------------------------------------------------------------------------

test_that("sbf with recipes", {
  skip_on_cran()
  ctrl <- sbfControl(functions = lmSBF, method = "cv")

  set.seed(3997)
  sbf_xy <-
    sbf(x = recipe_fs_x[-(1:100), ], y = logBBB[-(1:100)], sbfControl = ctrl)
  pred_xy <- predict(sbf_xy, recipe_fs_x[1:100, ])

  rec <- recipe(y ~ ., data = recipe_fs_dat) %>% step_log(mw)

  set.seed(3997)
  sbf_rec <- sbf(rec, data = recipe_fs_dat[-(1:100), ], sbfControl = ctrl)
  pred_rec <- predict(sbf_rec, recipe_fs_dat[1:100, -4])

  expect_equal(coef(sbf_xy$fit), coef(sbf_rec$fit))
  expect_equal(pred_xy, pred_rec)
})


# ------------------------------------------------------------------------------

test_that("sbf with recipes and boot632 resampling", {
  skip_on_cran()

  rec <- recipe(y ~ ., data = recipe_fs_dat) %>% step_log(mw)

  set.seed(3997)
  sbf_rec <- sbf(
    rec,
    data = recipe_fs_dat[-(1:100), ],
    sbfControl = sbfControl(functions = lmSBF, method = "boot632", number = 5)
  )

  metrics <- c("RMSE", "Rsquared", "MAE")
  apparent <- sbf_rec$resample$Resample == "AllData"
  expected <- (1 - exp(-1)) *
    colMeans(sbf_rec$resample[!apparent, metrics]) +
    exp(-1) * unlist(sbf_rec$resample[apparent, metrics])

  expect_equal(unlist(sbf_rec$results[metrics]), expected)
})


# ------------------------------------------------------------------------------

test_that("safs with recipes", {
  skip_on_cran()
  ctrl <- safsControl(functions = caretSA, method = "cv", number = 3)

  set.seed(3997)
  sa_xy <-
    safs(
      x = recipe_fs_x[-(1:100), ],
      y = logBBB[-(1:100)],
      safsControl = ctrl,
      iters = 2,
      differences = FALSE,
      method = "lm",
      trControl = trainControl(method = "cv")
    )
  pred_xy <- predict(sa_xy, recipe_fs_x[1:100, ])

  rec <- recipe(y ~ ., data = recipe_fs_dat) %>% step_log(mw)

  set.seed(3997)
  sa_rec <-
    safs(
      rec,
      data = recipe_fs_dat[-(1:100), ],
      safsControl = ctrl,
      iters = 2,
      differences = FALSE,
      method = "lm",
      trControl = trainControl(method = "cv")
    )
  pred_rec <- predict(sa_rec, recipe_fs_dat[1:100, -4])

  expect_equal(coef(sa_xy$fit$finalModel), coef(sa_rec$fit$finalModel))
  expect_equal(pred_xy, pred_rec)
})

# ------------------------------------------------------------------------------

test_that("gafs with recipes", {
  skip_on_cran()
  ctrl <- gafsControl(functions = caretGA, method = "cv", number = 3)

  set.seed(3997)
  ga_xy <-
    gafs(
      x = recipe_fs_x[-(1:100), ],
      y = logBBB[-(1:100)],
      gafsControl = ctrl,
      popSize = 4,
      iters = 2,
      differences = FALSE,
      method = "lm",
      trControl = trainControl(method = "cv")
    )
  pred_xy <- predict(ga_xy, recipe_fs_x[1:100, ])

  rec <- recipe(y ~ ., data = recipe_fs_dat) %>% step_log(mw)

  set.seed(3997)
  ga_rec <-
    gafs(
      rec,
      data = recipe_fs_dat[-(1:100), ],
      gafsControl = ctrl,
      popSize = 4,
      iters = 2,
      differences = FALSE,
      method = "lm",
      trControl = trainControl(method = "cv")
    )
  pred_rec <- predict(ga_rec, recipe_fs_dat[1:100, -4])

  expect_equal(coef(ga_xy$fit$finalModel), coef(ga_rec$fit$finalModel))
  expect_equal(pred_xy, pred_rec)
})
