# Shared by test_preProcess.R
check.medianImpute <- function(x) {
  # Reference column medians for checks
  med <- apply(x, 2, median, na.rm = TRUE)
  anyEmptyColumn <- any(is.na(med))
  med[is.na(med)] <- 0

  if (anyEmptyColumn) {
    expect_snapshot_warning(
      pp <- preProcess(x, method = "medianImpute")
    )
  } else {
    pp <- preProcess(x, method = "medianImpute")
  }
  expect_equal(pp$median, med)

  x.filled <- predict(pp, newdata = x)
  expect_false(any(is.na(x.filled)))
  expect_equal(x[!is.na(x)], x.filled[!is.na(x)])

  med.filled <- apply(x.filled, 2, median)
  expect_equal(med.filled, med)
}
