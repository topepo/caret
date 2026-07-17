# Test data shared by test-resamples.R.
#
# A resamples object is normally produced by resamples(list(model1, model2, ...))
# after fitting real models, but the methods that act on it (summary, sort,
# as.matrix, modelCor, diff, ...) only read its $values / $models / $metrics. So
# we hand-build one here with known numbers - no model fitting needed - and let
# the tests check the arithmetic exactly.
#
# Three models (A, B, C) scored on two metrics over five resamples. The RMSE
# values are chosen so that:
#   - mean RMSE goes A (3) < B (4) < C (5), so sorting is predictable
#   - A and B rise together (positive correlation)
#   - C is a mirror image of A (correlation of exactly -1)
rs_values <- data.frame(
  Resample = paste0("Fold", 1:5),
  "A~RMSE" = c(1, 2, 3, 4, 5),
  "B~RMSE" = c(3, 3, 4, 5, 5),
  "C~RMSE" = c(7, 6, 5, 4, 3),
  "A~Rsquared" = c(0.90, 0.85, 0.80, 0.75, 0.70),
  "B~Rsquared" = c(0.82, 0.78, 0.71, 0.66, 0.63),
  "C~Rsquared" = c(0.50, 0.55, 0.60, 0.65, 0.70),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

rs_fixture <- structure(
  list(
    call = quote(resamples(list(A = A, B = B, C = C))),
    values = rs_values,
    models = c("A", "B", "C"),
    metrics = c("RMSE", "Rsquared"),
    timings = as.data.frame(matrix(
      NA,
      nrow = 3,
      ncol = 3,
      dimnames = list(
        c("A", "B", "C"),
        c("Everything", "FinalModel", "Prediction")
      )
    )),
    methods = c(A = "lm", B = "lm", C = "knn")
  ),
  class = "resamples"
)
