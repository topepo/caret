# extractProb requires models that produce probabilities

    Code
      extractProb(list(lm = fit))
    Condition
      Error in `extractProb()`:
      ! only classification models that produce probabilities are allowed

# extractPrediction reports progress and handles unknowns

    Code
      ep <- extractPrediction(list(fit), testX = te, testY = iris$Species[1:20],
      unkX = unk, verbose = TRUE)
    Output
      Number of training samples: 150 
      Number of test samples:     20 
      
      There were  0 rows with missing values
      
      knn : 150 training predictions were added
      knn : 20 test predictions were added
      
      knn : 10 unknown predictions were added
      

