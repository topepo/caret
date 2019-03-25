library(recipes)
library(caret)
library(testthat)

# ------------------------------------------------------------------------------

context('updating objects')

diff_coef <- function(x, y) {
  x_nm <- names(x$fit$finalModel$coefficients)
  y_nm <- names(y$fit$finalModel$coefficients)
  delta <- c(setdiff(x_nm, y_nm), setdiff(y_nm, x_nm))
  length(delta) > 0
}

# ------------------------------------------------------------------------------

set.seed(3545)
dat <- SLC14_1(100)
y_ind <- which(names(dat) == "y")

# ------------------------------------------------------------------------------

test_that("train updating", {
  ctrl <- trainControl(method = "cv")
  
  lm_obj_form <- train(y ~ Var01 + Var02, data = dat, method = "lm", trControl = ctrl)
  lm_obj_form_2 <- update(lm_obj_form, list(intercept = FALSE))
  expect_equal(length(lm_obj_form_2$finalModel$coefficients), 2)
  
  rec <- recipe(y ~ Var01 + Var02, data = dat) %>% 
    step_mutate(Var01 = Var01/2)
  lm_obj_rec <- train(rec, data = dat, method = "lm", trControl = ctrl)
  lm_obj_rec_2 <- update(lm_obj_rec, list(intercept = FALSE))
  expect_equal(length(lm_obj_rec_2$finalModel$coefficients), 2)
})


# ------------------------------------------------------------------------------

test_that("safs updating", {
  ctrl <- safsControl(functions = caretSA, method = "cv", number = 3)
  
  set.seed(3997)
  sa_xy <-
    safs(x = dat[, -y_ind],
         y = dat$y,
         safsControl = ctrl,
         iters = 2,
         differences = FALSE,
         method = "lm",
         trControl = trainControl(method = "cv")
    )
  new_iter <- ifelse(sa_xy$optIter == 1, 2, 1)
  sa_xy_2 <- update(sa_xy, iter = new_iter, x = dat[, -y_ind], y = dat$y)
  expect_true(diff_coef(sa_xy, sa_xy_2))
  expect_error(update(sa_xy, iter = new_iter))
  
  rec <- recipe(y ~ ., data = dat) %>% 
    step_mutate(Var01 = Var01/2)
  set.seed(3997)
  sa_rec <-
    safs(rec,
         data = dat,
         safsControl = ctrl,
         iters = 2,
         differences = FALSE,
         method = "lm",
         trControl = trainControl(method = "cv")
    )
  new_iter <- ifelse(sa_rec$optIter == 1, 2, 1)
  sa_rec_2 <- update(sa_rec, iter = new_iter)
  expect_true(diff_coef(sa_rec, sa_rec_2))  
  sa_rec$recipe$template <- NULL
  expect_error(update(sa_rec, iter = new_iter))
  
})

# ------------------------------------------------------------------------------

test_that("gafs updating", {
  ctrl <- gafsControl(functions = caretGA, method = "cv", number = 3)
  
  set.seed(3997)
  ga_xy <-
    gafs(x = dat[, -y_ind],
         y = dat$y,
         gafsControl = ctrl,
         popSize = 4,
         iters = 2,
         differences = FALSE,
         method = "lm",
         trControl = trainControl(method = "cv")
    )
  new_iter <- ifelse(ga_xy$optIter == 1, 2, 1)
  ga_xy_2 <- update(ga_xy, iter = new_iter, x = dat[, -y_ind], y = dat$y)
  expect_true(diff_coef(ga_xy, ga_xy_2))
  expect_error(update(ga_xy, iter = new_iter))
  
  rec <- recipe(y ~ ., data = dat) %>% 
    step_mutate(Var01 = Var01/2)
  set.seed(3997)
  ga_rec <-
    gafs(rec,
         data = dat,
         gafsControl = ctrl,
         popSize = 4,
         iters = 2,
         differences = FALSE,
         method = "lm",
         trControl = trainControl(method = "cv")
    )
  new_iter <- ifelse(ga_rec$optIter == 1, 2, 1)
  ga_rec_2 <- update(ga_rec, iter = new_iter)
  expect_true(diff_coef(ga_rec, ga_rec_2))  
  ga_rec$recipe$template <- NULL
  expect_error(update(ga_rec, iter = new_iter))

})


# ------------------------------------------------------------------------------

test_that("rfe updating", {
  ctrl <- rfeControl(functions = caretFuncs, method = "cv", number = 3)
  
  set.seed(3997)
  rfe_xy <-
    rfe(x = dat[, -y_ind],
        y = dat$y,
        rfeControl = ctrl,
        sizes = 1:5,
        method = "lm",
        trControl = trainControl(method = "none")
    )
  rfe_xy_2 <- expect_warning(update(rfe_xy, size = 5, x = dat[, -y_ind], y = dat$y))
  expect_equal(length(rfe_xy_2$fit$finalModel$coefficients), 6)
  expect_error(update(rfe_xy, size = 5))
  
  rec <- recipe(y ~ ., data = dat) %>% 
    step_mutate(Var01 = Var01/2)
  set.seed(3997)
  rfe_rec <-
    rfe(rec,
        data = dat,
        rfeControl = ctrl,
        sizes = 1:5,
        method = "lm",
        trControl = trainControl(method = "none")
    )
  expect_warning(rfe_rec_2 <- update(rfe_rec, size = 5))
  expect_equal(length(rfe_rec_2$fit$finalModel$coefficients), 6)
  rfe_rec$recipe$template <- NULL
  expect_error(update(rfe_rec, size = 5))
  
})


