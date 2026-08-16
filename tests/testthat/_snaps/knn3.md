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
      

