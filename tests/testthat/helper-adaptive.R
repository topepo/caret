# Test data shared by test-adaptive.R.
#
# The tests here poke at the internal model-diversity helpers in R/adaptive.R:
# ccc, cccmat, diffmat, long2wide, get_id, filter_on_corr and filter_on_diff.

# RMSE for four candidate models, one column each, scored across five resamples
# (the rows). Lower is better. Everything else is built from this matrix, so if
# you want to tweak the data, tweak it here.
#
#   m1 - our best model
#   m2 - basically a copy of m1: it moves up and down with m1 and is only ~0.05
#        worse, so it's redundant and the filters should drop it
#   m3 - a different, middling model
#   m4 - a different, clearly worse model
adapt_wide <- cbind(
  m1 = c(1.00, 1.20, 0.90, 1.10, 1.00),
  m2 = c(1.05, 1.25, 0.95, 1.15, 1.05),
  m3 = c(2.00, 1.80, 2.20, 1.90, 2.10),
  m4 = c(3.00, 2.70, 3.30, 2.80, 3.20)
)
rownames(adapt_wide) <- paste0("Fold", 1:5)

# The same numbers, reshaped into the long form adaptiveWorkflow() actually
# works with: one row per model per resample. Building it straight from
# adapt_wide keeps the two views in sync and lines the columns up automatically.
adapt_results <- data.frame(
  model_id = rep(colnames(adapt_wide), each = nrow(adapt_wide)),
  Resample = rep(rownames(adapt_wide), times = ncol(adapt_wide)),
  RMSE = as.numeric(adapt_wide),
  stringsAsFactors = FALSE
)

# For reference, here's what the helpers make of this data (rounded):
#
# cccmat() measures how concordant two models are. m1 and m2 land around 0.91
# because they're near-copies; every other pair sits close to 0:
#        m1     m2     m3     m4
#   m1 1.000  0.912 -0.036 -0.014
#   m2 0.912  1.000 -0.040 -0.015
#   m3 ...    ...    1.000  0.073
#   m4 ...    ...    0.073  1.000
#
# diffmat() measures the standardised gap in performance. m1 and m2 differ by
# the same amount everywhere, so their gap is 0; every other pair is far apart
# (the diagonal is NA):
#        m1    m2     m3     m4
#   m1   NA  0.000  3.553  5.374
#   m2 0.000   NA   3.368  5.237
#   ...
#
# Either way m2 looks redundant next to m1, so both filters keep {m1, m3, m4}:
#   filter_on_corr(adapt_results, "RMSE", cutoff = 0.9)         -> m1 m3 m4
#   filter_on_diff(adapt_results, "RMSE", cutoff = 0.1, FALSE)  -> m1 m3 m4

# A little tuning grid with a repeated row, for get_id() - the two k = 3 rows
# should collapse into a single model.
adapt_grid <- data.frame(k = c(3, 3, 5, 7, 9))
