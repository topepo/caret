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

