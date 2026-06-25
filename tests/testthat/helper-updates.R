# Shared by test_updates.R
diff_coef <- function(x, y) {
  x_nm <- names(x$fit$finalModel$coefficients)
  y_nm <- names(y$fit$finalModel$coefficients)
  delta <- c(setdiff(x_nm, y_nm), setdiff(y_nm, x_nm))
  length(delta) > 0
}
