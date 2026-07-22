# The fixtures (adapt_results, adapt_wide, adapt_grid) live in helper-adaptive.R.
# These check the little model-picking helpers behind adaptiveWorkflow() - the
# workflow itself is tested elsewhere.

# --- ccc --------------------------------------------------------------------

test_that("ccc returns Lin's concordance correlation coefficient", {
  # two identical vectors agree perfectly, so we should get exactly 1
  expect_equal(caret:::ccc(1:5, 1:5), 1)
  # a pair that disagrees, with a value we can work out by hand
  expect_equal(caret:::ccc(1:5, c(5, 3, 1, 2, 4)), -0.3)
  # shifting every value by a constant should pull ccc below 1, even though the
  # plain correlation would still be a perfect 1
  expect_lt(caret:::ccc(1:5, (1:5) + 1), 1)
})

# --- cccmat -----------------------------------------------------------------

test_that("cccmat builds a symmetric concordance matrix with unit diagonal", {
  cc <- caret:::cccmat(adapt_wide)
  expect_equal(dim(cc), c(4, 4))
  expect_equal(unname(diag(cc)), rep(1, 4))
  expect_equal(cc, t(cc))
  expect_equal(colnames(cc), colnames(adapt_wide))
})

test_that("cccmat off-diagonals match ccc() and reflect the data", {
  cc <- caret:::cccmat(adapt_wide)
  # each cell should be the real pairwise score - before the fix this matrix
  # came back as all 1s
  expect_equal(
    cc["m1", "m2"],
    caret:::ccc(adapt_wide[, "m1"], adapt_wide[, "m2"])
  )
  expect_equal(
    cc["m1", "m3"],
    caret:::ccc(adapt_wide[, "m1"], adapt_wide[, "m3"])
  )
  # m1 and m2 look almost the same; m1 and m3 don't
  expect_gt(cc["m1", "m2"], 0.9)
  expect_lt(abs(cc["m1", "m3"]), 0.1)
})

# --- diffmat ----------------------------------------------------------------

test_that("diffmat is symmetric with an NA diagonal", {
  dm <- caret:::diffmat(adapt_wide)
  expect_equal(dim(dm), c(4, 4))
  expect_true(all(is.na(diag(dm))))
  # it should match its own transpose (leaving the NA diagonal aside)
  expect_equal(dm[lower.tri(dm)], t(dm)[lower.tri(dm)])
})

test_that("diffmat returns 0 when two columns differ by a constant", {
  # if two columns differ by the same amount everywhere, there's no spread in
  # the difference, so the gap comes out as 0
  m <- cbind(a = c(1, 2, 3), b = c(1, 2, 3) + 5)
  dm <- caret:::diffmat(m)
  expect_equal(dm["a", "b"], 0)
})

# --- long2wide --------------------------------------------------------------

test_that("long2wide reshapes resampling results to one row per resample", {
  w <- caret:::long2wide(adapt_results, "RMSE")
  expect_equal(colnames(w), c("Resample", "m1", "m2", "m3", "m4"))
  expect_equal(nrow(w), length(unique(adapt_results$Resample)))
  expect_equal(w$Resample, sort(unique(adapt_results$Resample)))
  # pick one cell and make sure it survived the reshape intact
  expect_equal(w$m3[w$Resample == "Fold1"], 2.0)
})

# --- get_id -----------------------------------------------------------------

test_that("get_id assigns one id per unique parameter combination", {
  ids <- caret:::get_id(adapt_grid, "k")
  # the two k = 3 rows should fold into one
  expect_equal(ids$k, c(3, 5, 7, 9))
  expect_equal(ids$model_id, c("m1", "m2", "m3", "m4"))
})

test_that("get_id zero-pads ids so they sort correctly", {
  ids <- caret:::get_id(data.frame(k = 1:12), "k")
  expect_equal(ids$model_id[1], "m01")
  expect_equal(ids$model_id[12], "m12")
})

# --- filter_on_corr ---------------------------------------------------------

test_that("filter_on_corr drops a redundant (highly concordant) model", {
  keep <- sort(unique(
    caret:::filter_on_corr(adapt_results, "RMSE", cutoff = 0.9)$model_id
  ))
  # m2 just echoes m1, so it gets dropped and the distinct models stay
  expect_equal(keep, c("m1", "m3", "m4"))
})

test_that("filter_on_corr keeps everything when nothing exceeds the cutoff", {
  keep <- sort(unique(
    caret:::filter_on_corr(adapt_results, "RMSE", cutoff = 1)$model_id
  ))
  expect_equal(keep, c("m1", "m2", "m3", "m4"))
})

test_that("filter_on_corr errors with a single model", {
  one <- adapt_results[adapt_results$model_id == "m1", ]
  expect_snapshot(
    caret:::filter_on_corr(one, "RMSE", cutoff = 0.9),
    error = TRUE
  )
})

# --- filter_on_diff ---------------------------------------------------------

test_that("filter_on_diff drops the weaker of two indistinguishable models", {
  keep <- sort(unique(
    caret:::filter_on_diff(
      adapt_results,
      "RMSE",
      cutoff = 0.1,
      maximize = FALSE
    )$model_id
  ))
  # m1 and m2 are basically the same; keep the better one (m1), drop m2
  expect_equal(keep, c("m1", "m3", "m4"))
})

test_that("filter_on_diff keeps everything when nothing is below the cutoff", {
  # nothing is closer than the cutoff, so it should bail out early and leave
  # every model in place
  keep <- sort(unique(
    caret:::filter_on_diff(
      adapt_results,
      "RMSE",
      cutoff = 0,
      maximize = FALSE
    )$model_id
  ))
  expect_equal(keep, c("m1", "m2", "m3", "m4"))
})
