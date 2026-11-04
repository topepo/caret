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

# knnregTrain stops when there are too many tied neighbours

    Code
      caret:::knnregTrain(train, test, y, k = 1)
    Condition
      Error in `caret:::knnregTrain()`:
      ! too many ties in knn

# knnregTrain validates its arguments

    k = 10 exceeds number 3 of patterns

---

    Code
      caret:::knnregTrain(train, test, y, k = 0)
    Condition
      Error in `caret:::knnregTrain()`:
      ! k = 0 must be at least 1

---

    Code
      caret:::knnregTrain(train, test, y[1:2], k = 1)
    Condition
      Error in `caret:::knnregTrain()`:
      ! 'train' and 'class' have different lengths

---

    Code
      caret:::knnregTrain(train, test[, 1, drop = FALSE], y, k = 1)
    Condition
      Error in `caret:::knnregTrain()`:
      ! dims of 'test' and 'train differ

---

    Code
      caret:::knnregTrain(train, test, c(1, 2, NA), k = 1)
    Condition
      Error in `caret:::knnregTrain()`:
      ! no missing values are allowed

# knnreg.formula rejects a malformed formula

    Code
      knnreg(~., data = mtcars)
    Condition
      Error in `knnreg.formula()`:
      ! formula missing or incorrect

# knnreg needs a numeric outcome

    Code
      knnreg(as.matrix(iris[, 1:4]), iris$Species)
    Condition
      Error in `knnreg.matrix()`:
      ! y must be numeric

---

    Code
      knnreg(iris[, 1:4], iris$Species)
    Condition
      Error in `knnreg.data.frame()`:
      ! y must be numeric

# predict.knnreg checks the object and can reuse the model frame

    Code
      caret:::predict.knnreg(structure(list(), class = "nope"))
    Condition
      Error in `caret:::predict.knnreg()`:
      ! object not of class knnreg

