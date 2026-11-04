# Shared 2x2 confusion-table fixtures for the metric tests
# (test-sensitivity.R and test-prec_rec.R).

# --- sensitivity / specificity / predictive values --------------------------
# Reference data from the sensitivity() documentation example
sens_lvs <- c("normal", "abnormal")
sens_truth <- factor(rep(sens_lvs, times = c(86, 258)), levels = rev(sens_lvs))
sens_pred <- factor(
  c(rep(sens_lvs, times = c(54, 32)), rep(sens_lvs, times = c(27, 231))),
  levels = rev(sens_lvs)
)
sens_xtab <- table(sens_pred, sens_truth)
# a plain matrix (class "matrix") to exercise the `.matrix` methods, since
# as.matrix() on a table keeps the "table" class
sens_mtab <- matrix(
  as.vector(sens_xtab),
  nrow = 2,
  dimnames = dimnames(sens_xtab)
)

# --- recall / precision / F_meas / prSummary --------------------------------
# Reference data from Table 2 of Powers (2007), used in the prec_rec docs
pr_lvs <- c("Relevant", "Irrelevant")
pr_pred <- factor(rep(pr_lvs, times = c(42, 58)), levels = pr_lvs)
pr_truth <- factor(
  c(rep(pr_lvs, times = c(30, 12)), rep(pr_lvs, times = c(30, 28))),
  levels = pr_lvs
)
pr_tbl <- table(pr_pred, pr_truth)

# Confusion counts: A = 30, B = 12, C = 30, D = 28
pr_precision <- 30 / (30 + 12)
pr_recall <- 30 / (30 + 30)
pr_fmeas <- 2 * pr_precision * pr_recall / (pr_precision + pr_recall)

# Data for prSummary(): needs obs/pred factors plus class-probability columns
# (one per level). Probabilities are deterministic so the test is reproducible.
pr_prob_data <- data.frame(
  obs = pr_truth,
  pred = pr_pred,
  Relevant = seq(0.95, 0.05, length.out = length(pr_truth))
)
pr_prob_data$Irrelevant <- 1 - pr_prob_data$Relevant
