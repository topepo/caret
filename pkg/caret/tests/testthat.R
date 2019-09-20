library(testthat)
library(caret)

if (requireNamespace("xml2")) {
  test_check("caret", reporter = MultiReporter$new(reporters = list(JunitReporter$new(file = "test-results.xml"), CheckReporter$new())))
} else {
  test_check("caret")
}
