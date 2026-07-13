# Shared test data for test-prec_rec.R

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
