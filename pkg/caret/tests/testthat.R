Sys.setenv("R_TESTS" = "")

library(testthat)
library(caret)

test_check("caret")
