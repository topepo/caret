# A stand-in for a survival::Surv() outcome, so the resampling tests can cover
# caret's `inherits(y, "Surv")` branches without depending on survival.
#
# A Surv object is a matrix of "time" and "status" columns carrying a "type"
# attribute, and caret only ever asks it for `y[, "time"]` and `nrow(y)`.
# Matching that structure means the fake behaves the same whether or not
# survival is loaded (its `[.Surv` method returns the same column).
fake_surv <- function(time, status = rep(0:1, length.out = length(time))) {
  structure(
    cbind(time = time, status = status),
    class = "Surv",
    type = "right"
  )
}
