# pp_list prints the expanded pre-processing names

    Code
      caret:::pp_list(c("center", "scale"))
    Output
      Pre-processing: centered, scaled 

---

    Code
      caret:::pp_list("BoxCox")
    Output
      Pre-processing: Box-Cox transformation 

# print.train describes a classification model with tuning

    Code
      print(fit)
    Output
      k-Nearest Neighbors 
      
      150 samples
        4 predictor
        3 classes: 'setosa', 'versicolor', 'virginica' 
      
      Pre-processing: centered (4), scaled (4) 
      Resampling: Cross-Validated (3 fold) 
      Summary of sample sizes: 100, 101, 99 
      Resampling results across tuning parameters:
      
        k  Accuracy   Kappa    
        5  0.9399840  0.9099884
        7  0.9533173  0.9299724
        9  0.9601200  0.9401616
      
      Accuracy was used to select the optimal model using the largest value.
      The final value used for the model was k = 9.

# print.train abbreviates the sample sizes with many resamples

    Code
      print(fit)
    Output
      k-Nearest Neighbors 
      
      150 samples
        4 predictor
        3 classes: 'setosa', 'versicolor', 'virginica' 
      
      No pre-processing
      Resampling: Cross-Validated (10 fold) 
      Summary of sample sizes: 135, 135, 135, 135, 135, 135, ... 
      Resampling results:
      
        Accuracy  Kappa
        0.96      0.94 
      
      Tuning parameter 'k' was held constant at a value of 5

# print.train handles a model fit without resampling

    Code
      print(fit)
    Output
      k-Nearest Neighbors 
      
      150 samples
        4 predictor
        3 classes: 'setosa', 'versicolor', 'virginica' 
      
      No pre-processing
      Resampling: None 

# print.train reports regression metrics

    Code
      print(fit)
    Output
      Linear Regression 
      
      120 samples
       20 predictor
      
      No pre-processing
      Resampling: Cross-Validated (3 fold) 
      Summary of sample sizes: 80, 80, 80 
      Resampling results:
      
        RMSE    Rsquared   MAE     
        23.241  0.1239973  17.10724
      
      Tuning parameter 'intercept' was held constant at a value of TRUE

# pp_list says None when there is nothing to report

    Code
      caret:::pp_list(list(center = character(0)))
    Output
      Pre-processing:  (None) 

---

    Code
      caret:::pp_list(character(0))
    Output
      Pre-processing: None 

# print.train can show the call and the final-model rows

    Code
      print(fit, printCall = TRUE)
    Output
      k-Nearest Neighbors 
      
      
      Call:
      train.formula(form = Species ~ ., data = dat, method = "knn", tuneGrid
       = data.frame(k = 5), trControl = trainControl(method = "cv", index =
       folds, indexFinal = folds[[1]]))
      
      45 samples, 30 used for final model
       4 predictor
       3 classes: 'setosa', 'versicolor', 'virginica' 
      
      No pre-processing
      Resampling: Cross-Validated (10 fold) 
      Summary of sample sizes: 30, 30, 30 
      Resampling results:
      
        Accuracy   Kappa    
        <num>  <num>
      
      Tuning parameter 'k' was held constant at a value of 5

# print.train lists the steps of a recipe

    Code
      print(fit)
    Output
      k-Nearest Neighbors 
      
      45 samples
       4 predictor
       3 classes: 'setosa', 'versicolor', 'virginica' 
      
      Recipe steps: normalize, pca 
      Resampling: Cross-Validated (3 fold) 
      Summary of sample sizes: 30, 30, 30 
      Resampling results:
      
        Accuracy   Kappa    
        <num>  <num>
      
      Tuning parameter 'k' was held constant at a value of 5

# print.train names the additional sampling scheme

    Code
      print(fit)
    Output
      k-Nearest Neighbors 
      
      60 samples
      17 predictors
       2 classes: 'Class1', 'Class2' 
      
      Pre-processing: centered (17) 
      Resampling: Cross-Validated (3 fold) 
      Summary of sample sizes: 40, 40, 40 
      Addtional sampling using down-sampling prior to pre-processing
      
      Resampling results:
      
        Accuracy   Kappa    
        <num>  <num>
      
      Tuning parameter 'k' was held constant at a value of 5

# print.train can show the standard deviations

    Code
      print(fit, showSD = TRUE)
    Output
      k-Nearest Neighbors 
      
      45 samples
       4 predictor
       3 classes: 'setosa', 'versicolor', 'virginica' 
      
      No pre-processing
      Resampling: Cross-Validated (3 fold) 
      Summary of sample sizes: 30, 30, 30 
      Resampling results across tuning parameters (values below are 'mean (sd)'):
      
        k  Accuracy                Kappa                 
        3  <num> (<num>)  <num> (<num>)
        5  <num> (<num>)  <num> (<num>)
      
      Accuracy was used to select the optimal model using the largest value.
      The final value used for the model was k = 5.

