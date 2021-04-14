library(caret)

test_that('resample calculations', {
  library(MASS)
  set.seed(4793)
  tr_dat <- SLC14_1(200)

  ctrl <- trainControl(method = "cv", number = 3)

  set.seed(5423)
  lm_fit <- train(y ~ ., data = tr_dat, method = "lm", trControl = ctrl)
  set.seed(5423)
  expect_warning(rlm_fit <- train(y ~ ., data = tr_dat, method = "rlm", trControl = ctrl))

  rs <- resamples(list(lm = lm_fit, rlm = rlm_fit))
  rs_d <- diff(rs, metric = "RMSE")
  t_test <- t.test(rs$values$`lm~RMSE`, rs$values$`rlm~RMSE`, paired = TRUE)
  expect_equal(rs_d$statistics$RMSE$lm.diff.rlm$conf.int, t_test$conf.int)

  lm_t_test  <- t.test(rs$values$`lm~RMSE`)
  rlm_t_test <- t.test(rs$values$`rlm~RMSE`)

  rs_plot_dat <- ggplot(rs, metric = "RMSE")$data
  expect_equivalent(lm_t_test$estimate,  rs_plot_dat$Estimate[1])
  expect_equivalent(rlm_t_test$estimate, rs_plot_dat$Estimate[2])
  expect_equivalent(lm_t_test$conf.int[1],  rs_plot_dat$LowerLimit[1])
  expect_equivalent(rlm_t_test$conf.int[1], rs_plot_dat$LowerLimit[2])
  expect_equivalent(lm_t_test$conf.int[2],  rs_plot_dat$UpperLimit[1])
  expect_equivalent(rlm_t_test$conf.int[2], rs_plot_dat$UpperLimit[2])

  rs_const <- rs
  rs_const$values$`lm~RMSE` <- 3

  rs_plot_const_dat <- ggplot(rs_const, metric = "RMSE")$data
  expect_equivalent(rs_plot_const_dat$Estimate[1], 3.0)
  expect_equivalent(rs_plot_const_dat$LowerLimit[1], NA_real_)
  expect_equivalent(rs_plot_const_dat$UpperLimit[1], NA_real_)

})


test_that('test group-k-fold', {
  get_data <- function(n = 500) {
    prevalence <- seq(.1, .9, length = 26)
    dat <- sample(letters, size = n, replace = TRUE, prob = sample(prevalence))
    data.frame(grp = dat, stringsAsFactors = TRUE)
  }

  check_rs <- function(ind, dat) {
    in_samp <- unique(dat$grp[ind])
    out_samp <- unique(dat$grp[-ind])
    length(intersect(out_samp, in_samp)) > 0
  }

  set.seed(5683286)
  running_sum <- 0
  for (k in (1:5) * 100) {
    for (j in 1:20) {
      dat <- get_data(n = k)
      lvls <- length(unique(as.character(dat$grp)))
      for (i in 1:20) {
        inds <- groupKFold(dat$grp, k = i)
        running_sum <- running_sum + sum(vapply(inds, check_rs, logical(1), dat))

      }
    }
  }
  expect_true(running_sum == 0)


})
