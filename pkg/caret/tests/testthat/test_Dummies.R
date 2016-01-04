context('Dummy Variables')

## Test cases by Josh Brady (doublej2) from issue #344

check_dummies <- function(x, expected = NULL) {
  dfTrain <- data.frame(xf = c('a','b','c'))
  dfTest <- data.frame(xf = c('a','b'))
  dummyObj <- dummyVars(~., dfTrain)

  expected_train <- diag(3)
  colnames(expected_train) <- paste0("xf.", letters[1:3])
  rownames(expected_train) <- paste(1:3)
  expected_test <- expected_train[1:2,]
  
  expect_equal(predict(dummyObj, newdata = dfTrain), 
               expected_train)
  expect_equal(predict(dummyObj, newdata = dfTest), 
               expected_test)  
}
