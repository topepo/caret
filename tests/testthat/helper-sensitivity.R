# Shared test data for test-sensitivity.R

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
