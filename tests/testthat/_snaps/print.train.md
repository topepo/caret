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

