# knnreg fits from a formula, matrix and data frame

    Code
      knnreg(mtcars$mpg)
    Condition
      Error in `knnreg.default()`:
      ! knnreg only implemented for formula objects

# print.knnreg identifies the model

    Code
      print(knnreg(mpg ~ ., data = mtcars, k = 5))
    Output
      5-nearest neighbor regression model

