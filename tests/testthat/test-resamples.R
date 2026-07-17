test_that("resample calculations", {
  skip_on_cran()
  skip_if_not_installed("MASS")
  set.seed(4793)
  tr_dat <- SLC14_1(200)

  ctrl <- trainControl(method = "cv", number = 3)

  set.seed(5423)
  lm_fit <- train(y ~ ., data = tr_dat, method = "lm", trControl = ctrl)
  set.seed(5423)
  expect_snapshot_warning(
    rlm_fit <- train(y ~ ., data = tr_dat, method = "rlm", trControl = ctrl)
  )

  rs <- resamples(list(lm = lm_fit, rlm = rlm_fit))
  rs_d <- diff(rs, metric = "RMSE")
  t_test <- t.test(rs$values$`lm~RMSE`, rs$values$`rlm~RMSE`, paired = TRUE)
  expect_equal(rs_d$statistics$RMSE$lm.diff.rlm$conf.int, t_test$conf.int)

  lm_t_test <- t.test(rs$values$`lm~RMSE`)
  rlm_t_test <- t.test(rs$values$`rlm~RMSE`)

  rs_plot_dat <- ggplot(rs, metric = "RMSE")$data
  expect_equal(lm_t_test$estimate, rs_plot_dat$Estimate[1], ignore_attr = TRUE)
  expect_equal(rlm_t_test$estimate, rs_plot_dat$Estimate[2], ignore_attr = TRUE)
  expect_equal(
    lm_t_test$conf.int[1],
    rs_plot_dat$LowerLimit[1],
    ignore_attr = TRUE
  )
  expect_equal(
    rlm_t_test$conf.int[1],
    rs_plot_dat$LowerLimit[2],
    ignore_attr = TRUE
  )
  expect_equal(
    lm_t_test$conf.int[2],
    rs_plot_dat$UpperLimit[1],
    ignore_attr = TRUE
  )
  expect_equal(
    rlm_t_test$conf.int[2],
    rs_plot_dat$UpperLimit[2],
    ignore_attr = TRUE
  )

  rs_const <- rs
  rs_const$values$`lm~RMSE` <- 3

  rs_plot_const_dat <- ggplot(rs_const, metric = "RMSE")$data
  expect_equal(rs_plot_const_dat$Estimate[1], 3.0, ignore_attr = TRUE)
  expect_equal(rs_plot_const_dat$LowerLimit[1], NA_real_, ignore_attr = TRUE)
  expect_equal(rs_plot_const_dat$UpperLimit[1], NA_real_, ignore_attr = TRUE)
})

# ------------------------------------------------------------------------------

test_that("test group-k-fold", {
  skip_on_cran()
  get_data <- function(n = 500) {
    prevalence <- seq(0.1, 0.9, length.out = 26)
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
        running_sum <- running_sum +
          sum(vapply(inds, check_rs, logical(1), dat))
      }
    }
  }
  expect_true(running_sum == 0)
})

# ------------------------------------------------------------------------------
# Methods that act on a resamples object; `rs_fixture` lives in
# helper-resamples.R

test_that("as.data.frame.resamples pulls out one metric in long-ish form", {
  df <- as.data.frame(rs_fixture, metric = "RMSE")
  # one column per model, plus the Resample labels tacked on the end
  expect_equal(colnames(df), c("A", "B", "C", "Resample"))
  expect_equal(df$A, c(1, 2, 3, 4, 5))
  expect_equal(df$Resample, paste0("Fold", 1:5))
})

test_that("as.matrix.resamples returns a resample-by-model matrix", {
  m <- as.matrix(rs_fixture, metric = "RMSE")
  expect_equal(dim(m), c(5, 3))
  expect_equal(colnames(m), c("A", "B", "C"))
  expect_equal(rownames(m), paste0("Fold", 1:5))
  expect_equal(m[, "C"], c(7, 6, 5, 4, 3), ignore_attr = TRUE)
  # asking for a metric that isn't there should be an error, not empty output
  expect_error(as.matrix(rs_fixture, metric = "nope"), "no columns fit")
})

test_that("sort.resamples orders models by mean performance", {
  # A has the lowest mean RMSE, C the highest
  expect_equal(sort(rs_fixture, metric = "RMSE"), c("A", "B", "C"))
  expect_equal(
    sort(rs_fixture, metric = "RMSE", decreasing = TRUE),
    c("C", "B", "A")
  )
})

test_that("modelCor correlates models across resamples", {
  cm <- modelCor(rs_fixture, metric = "RMSE")
  expect_equal(diag(cm), c(A = 1, B = 1, C = 1))
  # C was built as a mirror image of A, so they land at exactly -1
  expect_equal(cm["A", "C"], -1)
  # A and B rise together
  expect_gt(cm["A", "B"], 0)
})

test_that("summary.resamples reports the usual five-number summary per model", {
  sm <- summary(rs_fixture)
  expect_s3_class(sm, "summary.resamples")
  expect_equal(names(sm$statistics), c("RMSE", "Rsquared"))
  # the row means match what we put in (A averages 3, C averages 5)
  expect_equal(sm$statistics$RMSE["A", "Mean"], 3)
  expect_equal(sm$statistics$RMSE["C", "Mean"], 5)
})

test_that("diff.resamples computes pairwise differences and tests", {
  d <- diff(rs_fixture, metric = "RMSE")
  expect_s3_class(d, "diff.resamples")
  expect_equal(colnames(d$difs$RMSE), c("A.diff.B", "A.diff.C", "B.diff.C"))
  # A - B averages -1, A - C averages -2 (A is the better, lower-RMSE model)
  expect_equal(mean(d$difs$RMSE[, "A.diff.B"]), -1)
  expect_equal(mean(d$difs$RMSE[, "A.diff.C"]), -2)
  # each comparison carries a t-test, and the CI is Bonferroni-widened
  expect_s3_class(d$statistics$RMSE[["A.diff.B"]], "htest")
  expect_gt(d$confLevel, 0.95)
})

test_that("compare_models runs a paired test between two fitted models", {
  skip_on_cran()

  set.seed(1)
  dat <- data.frame(x1 = rnorm(80), x2 = rnorm(80))
  dat$y <- 2 * dat$x1 + rnorm(80)
  ctrl <- trainControl(method = "cv", number = 5)

  # same seed before each fit so the two models share the same resamples
  set.seed(2)
  m1 <- train(y ~ ., data = dat, method = "lm", trControl = ctrl)
  set.seed(2)
  m2 <- train(y ~ ., data = dat, method = "knn", trControl = ctrl)

  cmp <- compare_models(m1, m2)
  expect_s3_class(cmp, "htest")
  expect_false(is.null(cmp$p.value))
})
