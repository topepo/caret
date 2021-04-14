context('Test preProcess internals')

test_that("handles situation when there's a single column per transformation", {
  # For the data below, the list of transformations used internally by
  # "preProcess" contains a single related attribute
  # for each transformation; namely, `method=list(center="y", ignore="x")`.
  # At certain point such setting didn't work properly.
  x <- data.frame(x=factor(c("a", "a")), y=c(1, 2), stringsAsFactors = TRUE)
  model <- caret::preProcess(method="center", x=x)
  preprocessed <- predict(model, x)
  expect_equal(preprocessed, data.frame(x=c("a", "a"), y=c(-0.5, 0.5), stringsAsFactors = TRUE))
})
