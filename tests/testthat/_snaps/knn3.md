# knn3 fits from a formula, matrix and data frame

    Code
      knn3(iris$Sepal.Length)
    Condition
      Error in `knn3.default()`:
      ! knn3 only implemented for formula objects

# print.knn3 shows the neighbour count and class distribution

    Code
      print(knn3(Species ~ ., data = iris, k = 5))
    Output
      5-nearest neighbor model
      Training set outcome distribution:
      
          setosa versicolor  virginica 
              50         50         50 
      

# knn3Train stops when there are too many tied neighbours

    Code
      knn3Train(train, test, cl, k = 1)
    Condition
      Error in `knn3Train()`:
      ! too many ties in knn

# knn3Train validates its arguments

    k = 10 exceeds number 3 of patterns

---

    Code
      knn3Train(train, test, cl, k = 0)
    Condition
      Error in `knn3Train()`:
      ! k = 0 must be at least 1

---

    Code
      knn3Train(train, test, cl[1:2], k = 1)
    Condition
      Error in `knn3Train()`:
      ! 'train' and 'class' have different lengths

---

    Code
      knn3Train(train, test[, 1, drop = FALSE], cl, k = 1)
    Condition
      Error in `knn3Train()`:
      ! dims of 'test' and 'train differ

---

    Code
      knn3Train(train, test, factor(c("a", "b", NA)), k = 1)
    Condition
      Error in `knn3Train()`:
      ! no missing values are allowed

# knn3.formula rejects a malformed formula

    Code
      knn3(~., data = iris)
    Condition
      Error in `knn3.formula()`:
      ! formula missing or incorrect

# knn3.matrix needs a factor outcome

    Code
      knn3(as.matrix(iris[, 1:4]), as.numeric(iris$Species))
    Condition
      Error in `knn3.matrix()`:
      ! y must be a factor

# print.knn3 summarises a non-factor outcome

    Code
      print(fit)
    Output
      5-nearest neighbor model
      Training set outcome distribution:
         Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
            1       1       2       2       3       3 
      

# predict.knn3 checks the object and can reuse the model frame

    Code
      caret:::predict.knn3(structure(list(), class = "nope"))
    Condition
      Error in `caret:::predict.knn3()`:
      ! object not of class knn3

