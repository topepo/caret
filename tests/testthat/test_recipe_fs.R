library(testthat)
library(caret)
library(recipes)

context('selection with recipes')

# ------------------------------------------------------------------------------

data(BloodBrain)

x_dat <- bbbDescr[, c("tpsa", "clogp", "mw")]
x_dat$mw <- log(x_dat$mw)

# ------------------------------------------------------------------------------

test_that("sbf with recipes", {
  ctrl <- sbfControl(functions = lmSBF, method = "cv")
  
  set.seed(3997)
  sbf_xy <-
    sbf(x = x_dat[-(1:100),],
        y = logBBB[-(1:100)],
        sbfControl = ctrl)
  pred_xy <- predict(sbf_xy, x_dat[1:100,])
  
  dat <- bbbDescr[, c("tpsa", "clogp", "mw")]
  dat$y <- logBBB
  
  rec <- recipe(y ~ ., data = dat) %>% step_log(mw)
  
  set.seed(3997)
  sbf_rec <- sbf(rec, data = dat[-(1:100), ], sbfControl = ctrl)
  pred_rec <- predict(sbf_rec, dat[1:100,-4])
  
  expect_equal(coef(sbf_xy$fit), coef(sbf_rec$fit))
  expect_equal(pred_xy, pred_rec)
})


# ------------------------------------------------------------------------------

test_that("safs with recipes", {
  ctrl <- safsControl(functions = caretSA, method = "cv", number = 3)
  
  set.seed(3997)
  sa_xy <-
    safs(x = x_dat[-(1:100),],
        y = logBBB[-(1:100)],
        safsControl = ctrl,
        iters = 2,
        differences = FALSE,
        method = "lm",
        trControl = trainControl(method = "cv")
        )
  pred_xy <- predict(sa_xy, x_dat[1:100,])
  
  dat <- bbbDescr[, c("tpsa", "clogp", "mw")]
  dat$y <- logBBB
  
  rec <- recipe(y ~ ., data = dat) %>% step_log(mw)
  
  set.seed(3997)
  sa_rec <-
    safs(rec,
         data = dat[-(1:100),],
         safsControl = ctrl,
         iters = 2,
         differences = FALSE,
         method = "lm",
         trControl = trainControl(method = "cv")
    )
  pred_rec <- predict(sa_rec, dat[1:100,-4])
  
  expect_equal(coef(sa_xy$fit$finalModel), coef(sa_rec$fit$finalModel))
  expect_equal(pred_xy, pred_rec)
})

# ------------------------------------------------------------------------------

test_that("gafs with recipes", {
  ctrl <- gafsControl(functions = caretGA, method = "cv", number = 3)
  
  set.seed(3997)
  ga_xy <-
    gafs(x = x_dat[-(1:100),],
         y = logBBB[-(1:100)],
         gafsControl = ctrl,
         popSize = 4,
         iters = 2,
         differences = FALSE,
         method = "lm",
         trControl = trainControl(method = "cv")
    )
  pred_xy <- predict(ga_xy, x_dat[1:100,])
  
  dat <- bbbDescr[, c("tpsa", "clogp", "mw")]
  dat$y <- logBBB
  
  rec <- recipe(y ~ ., data = dat) %>% step_log(mw)
  
  set.seed(3997)
  ga_rec <-
    gafs(rec,
         data = dat[-(1:100),],
         gafsControl = ctrl,
         popSize = 4,
         iters = 2,
         differences = FALSE,
         method = "lm",
         trControl = trainControl(method = "cv")
    )
  pred_rec <- predict(ga_rec, dat[1:100,-4])
  
  expect_equal(coef(ga_xy$fit$finalModel), coef(ga_rec$fit$finalModel))
  expect_equal(pred_xy, pred_rec)
})

