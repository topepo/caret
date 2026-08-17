# expoTrans validates its newdata

    Code
      predict(expoTrans(skew_y), "abc")
    Condition
      Error in `predict.expoTrans()`:
      ! newdata should be a numeric vector

# expoTrans refuses missing data when na.rm is FALSE

    Code
      expoTrans(y, na.rm = FALSE)
    Condition
      Error in `expoTrans.numeric()`:
      ! missing data found

# expoTrans skips estimation for too few unique values

    Code
      print(et)
    Output
      Exponential Transformation
      
      20 data points used to estimate Lambda
      
      Input data summary:
         Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
            2       2       2       2       2       2 
      
      Largest/Smallest: 1 
      Sample Skewness: NaN 
      
      Lambda could not be estimated; no transformation is applied
      

# expoTrans.numeric handles the same edge cases

    Code
      caret:::expoTrans.numeric(y, na.rm = FALSE)
    Condition
      Error in `caret:::expoTrans.numeric()`:
      ! missing data found

# expoTrans.default guards its input like the numeric method

    Code
      caret:::expoTrans.default(y, na.rm = FALSE)
    Condition
      Error in `caret:::expoTrans.default()`:
      ! missing data found

