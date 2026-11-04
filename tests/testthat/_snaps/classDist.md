# errors working

    Code
      distData <- classDist(iris[trainSet, 1:4], iris$Species[trainSet])
    Condition
      Error in `FUN()`:
      ! there must be more rows than columns for this class

# print.classDist describes a factor-outcome model

    Code
      print(classDist(x, y, pca = FALSE))
    Output
      
      Call:
      classDist.default(x = x, y = y, pca = FALSE)
      
      # predictors variables: 4 
      # samples: 30 (setosa), 30 (versicolor), 30 (virginica) 

---

    Code
      print(classDist(x, y, pca = TRUE))
    Output
      
      Call:
      classDist.default(x = x, y = y, pca = TRUE)
      
      PCA applied, 4 components retained
      
      # predictors variables: 4 
      # samples: 30 (setosa), 30 (versicolor), 30 (virginica) 

# print.classDist describes a binned numeric outcome

    Code
      print(classDist(x, y, pca = FALSE, groups = 3))
    Output
      
      Call:
      classDist.default(x = x, y = y, groups = 3, pca = FALSE)
      
      Classes based on 2 cuts of the data
      
      # predictors variables: 4 
      # samples: 30 [1,1.67], 30 (1.67,2.33], 30 (2.33,3] 

# classDist needs more rows than columns in every class

    Code
      classDist(x, y)
    Condition
      Error in `FUN()`:
      ! there must be more rows than columns for this class

# classDist reports an uninvertible covariance matrix

    Code
      classDist(x, y)
    Condition
      Error in `FUN()`:
      ! Cannot invert the covariance matrix

