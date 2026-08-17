# BoxCoxTrans validates its input

    Code
      BoxCoxTrans(factor(letters))
    Condition
      Error in `BoxCoxTrans.default()`:
      ! y must be numeric

---

    Code
      predict(BoxCoxTrans(skew_y), "abc")
    Condition
      Error in `predict.BoxCoxTrans()`:
      ! newdata should be a numeric vector

---

    Code
      predict(BoxCoxTrans(skew_y), c(-1, 1, 2))
    Condition
      Warning in `predict.BoxCoxTrans()`:
      newdata should have values > 0
      Warning in `log()`:
      NaNs produced
    Output
      [1]       NaN 0.0000000 0.6931472

# BoxCoxTrans skips estimation for non-positive or constant data

    Code
      print(neg)
    Output
      Box-Cox Transformation
      
      6 data points used to estimate Lambda
      
      Input data summary:
         Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
       -1.000   1.250   2.500   2.333   3.750   5.000 
      
      Lambda could not be estimated; no transformation is applied
      

# print.BoxCoxTrans reports the fudge-factor rounding

    Code
      print(bc)
    Output
      Box-Cox Transformation
      
      120 data points used to estimate Lambda
      
      Input data summary:
         Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
       0.1092  0.6103  1.0601  1.6427  1.9968 11.0410 
      
      Largest/Smallest: 101 
      Sample Skewness: 2.7 
      
      Estimated Lambda: 0.01 
      With fudge factor, Lambda = 0 will be used for transformations
      

---

    Code
      print(bc)
    Output
      Box-Cox Transformation
      
      120 data points used to estimate Lambda
      
      Input data summary:
         Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
       0.1092  0.6103  1.0601  1.6427  1.9968 11.0410 
      
      Largest/Smallest: 101 
      Sample Skewness: 2.7 
      
      Estimated Lambda: 0.99 
      With fudge factor, no transformation is applied
      

