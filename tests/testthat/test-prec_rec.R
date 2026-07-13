# Fixture data (pr_lvs, pr_pred, pr_truth, pr_tbl, pr_precision, pr_recall,
# pr_fmeas) lives in helper-prec_rec.R

# --- precision --------------------------------------------------------------

test_that("precision works for tables and factors", {
  expect_equal(precision(pr_tbl), pr_precision)
  expect_equal(
    precision(data = pr_pred, reference = pr_truth, relevant = "Relevant"),
    pr_precision
  )
})

test_that("precision errors on bad input", {
  expect_error(precision(as.character(pr_pred), pr_truth), "must be a factor")
  expect_error(
    precision(factor(letters[1:3]), factor(letters[1:3])),
    "same two levels"
  )
})

test_that("precision collapses multiclass tables", {
  irisTabs <- table(iris$Species, iris$Species)
  expect_equal(precision(irisTabs, "versicolor"), 1)
})

# --- recall -----------------------------------------------------------------

test_that("recall works for tables and factors", {
  expect_equal(recall(pr_tbl), pr_recall)
  expect_equal(
    recall(data = pr_pred, reference = pr_truth, relevant = "Relevant"),
    pr_recall
  )
})

test_that("recall errors on bad input", {
  expect_error(recall(as.character(pr_pred), pr_truth), "must be a factor")
  expect_error(
    recall(factor(letters[1:3]), factor(letters[1:3])),
    "same two levels"
  )
})

test_that("recall returns NA when there are no relevant references", {
  none_rel <- factor(rep("Irrelevant", 10), levels = pr_lvs)
  expect_true(is.na(recall(none_rel, none_rel, relevant = "Relevant")))
})

test_that("recall collapses multiclass tables", {
  irisTabs <- table(iris$Species, iris$Species)
  expect_equal(recall(irisTabs, "versicolor"), 1)
})

# --- F_meas -----------------------------------------------------------------

test_that("F_meas works for tables and factors", {
  expect_equal(F_meas(pr_tbl), pr_fmeas)
  expect_equal(
    F_meas(data = pr_pred, reference = pr_truth, relevant = "Relevant"),
    pr_fmeas
  )
})

test_that("F_meas respects the beta weighting", {
  beta <- 2
  f_beta <- (1 + beta^2) *
    pr_precision *
    pr_recall /
    ((beta^2 * pr_precision) + pr_recall)
  expect_equal(F_meas(pr_tbl, beta = beta), f_beta)
})

test_that("F_meas errors on bad input", {
  expect_error(F_meas(as.character(pr_pred), pr_truth), "must be a factor")
  expect_error(
    F_meas(factor(letters[1:3]), factor(letters[1:3])),
    "same two levels"
  )
})

test_that("na.rm drops incomplete cases before scoring", {
  p <- pr_pred
  r <- pr_truth
  p[1:5] <- NA
  r[6:10] <- NA
  # recall, precision and F_meas all drop the incomplete cases identically
  expect_equal(
    recall(p, r, relevant = "Relevant"),
    recall(p[11:100], r[11:100], relevant = "Relevant")
  )
  expect_equal(
    precision(p, r, relevant = "Relevant"),
    precision(p[11:100], r[11:100], relevant = "Relevant")
  )
  expect_equal(
    F_meas(p, r, relevant = "Relevant"),
    F_meas(p[11:100], r[11:100], relevant = "Relevant")
  )
})

# --- prSummary --------------------------------------------------------------

test_that("prSummary returns AUC, precision, recall and F", {
  skip_if_not_installed("MLmetrics")
  res <- prSummary(pr_prob_data, lev = pr_lvs)
  expect_named(res, c("AUC", "Precision", "Recall", "F"))
  expect_equal(unname(res["Precision"]), pr_precision)
  expect_equal(unname(res["Recall"]), pr_recall)
  expect_equal(unname(res["F"]), pr_fmeas)
  expected_auc <- MLmetrics::PRAUC(
    y_pred = pr_prob_data[, pr_lvs[1]],
    y_true = ifelse(pr_prob_data$obs == pr_lvs[1], 1, 0)
  )
  expect_equal(unname(res["AUC"]), expected_auc)
})

test_that("prSummary rejects outcomes with more than two classes", {
  skip_if_not_installed("MLmetrics")
  d <- data.frame(
    obs = factor(c("a", "b", "c")),
    pred = factor(c("a", "b", "c"))
  )
  expect_error(prSummary(d, lev = c("a", "b", "c")), "isn't appropriate")
})

test_that("prSummary errors when predicted and observed levels differ", {
  skip_if_not_installed("MLmetrics")
  d <- data.frame(
    obs = factor(c("A", "B"), levels = c("A", "B")),
    pred = factor(c("A", "B"), levels = c("B", "A"))
  )
  expect_error(prSummary(d, lev = c("A", "B")), "do not match")
})

test_that("prSummary errors when class probabilities are missing", {
  skip_if_not_installed("MLmetrics")
  d <- data.frame(
    obs = factor(c("A", "B"), levels = c("A", "B")),
    pred = factor(c("A", "B"), levels = c("A", "B"))
  )
  expect_error(
    prSummary(d, lev = c("A", "B")),
    "Class probabilities are needed"
  )
})
