# print.bagFDA summarises the model

    Code
      print(fit)
    Output
      Data:
         # variables:	 4 
         # samples:	 150 
      
      Model: 
         B:        	 3 
         dimension:	 2 
      case weights used
      

# summary.bagFDA reports out-of-bag and model statistics

    Code
      print(sm)
    Output
      Out of bag statistics:
      
            Accuracy  Kappa
      0%      0.9492 0.9235
      2.5%    0.9508 0.9260
      50%     0.9818 0.9726
      97.5%   0.9835 0.9750
      100%    0.9836 0.9751
      
      Model Selection Statistics:
      
         Num Terms      Num Variables
       Min.   : 9.000   Min.   :3    
       1st Qu.: 9.000   1st Qu.:3    
       Median : 9.000   Median :3    
       Mean   : 9.333   Mean   :3    
       3rd Qu.: 9.500   3rd Qu.:3    
       Max.   :10.000   Max.   :3    
      

# bagFDA.formula needs a formula

    Code
      caret:::bagFDA.formula(iris[, 1:4])
    Condition
      Error in `caret:::bagFDA.formula()`:
      ! method is only for formula objects

