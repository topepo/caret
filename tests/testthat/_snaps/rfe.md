# rfe runs and its methods behave (default interface)

    Code
      print(rf)
    Output
      
      Recursive feature selection
      
      Outer resampling method: Cross-Validated (3 fold) 
      
      Resampling performance over subset size:
      
       Variables Accuracy  Kappa AccuracySD KappaSD Selected
               2   0.7133 0.3786    0.07024 0.14768         
               4   0.7533 0.4720    0.06429 0.13298         
               6   0.8467 0.6749    0.04619 0.09194        *
               8   0.8467 0.6767    0.02309 0.04100         
      
      The top 5 variables (out of 6):
         TwoFactor2, TwoFactor1, Linear02, Linear03, Linear04
      

