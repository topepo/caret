# bagEarth requires a binomial glm for a factor outcome

    Code
      bagEarth(dat[, 1:5], dat$Class, B = 2)
    Condition
      Error in `bagEarth.default()`:
      ! must declare a binomal glm using the glm argument to earth

# predict.bagEarth validates the type and can use the training data

    Code
      predict(fit, trees[, -3], type = "nope")
    Condition
      Error:
      ! type must be either response, class or prob

# print.bagEarth describes the ensemble

    Code
      print(fit)
    Output
      
      Call:
      bagEarth.default(x = trees[, -3], y = trees[, 3], B = 3)
      
      Data:
         # variables:	 2 
         # samples:	 31 
      case weights used
      
      B: 3 

# summary.bagEarth reports the term and variable counts

    Code
      print(smry)
    Output
      
      Call:
      bagEarth.default(x = trees[, -3], y = trees[, 3], B = 3)
      
      Out of bag statistics:
      
             RMSE Rsquared   MAE
      0%    <num>   <num> <num>
      <num>%  <num>   <num> <num>
      25%   <num>   <num> <num>
      50%   <num>   <num> <num>
      75%   <num>   <num> <num>
      <num>% <num>   <num> <num>
      100%  <num>   <num> <num>
      
      Model Selection Statistics:
      
         Num Terms     Num Variables
       Min.   :<num>   Min.   :2    
       1st Qu.:<num>   1st Qu.:2    
       Median :<num>   Median :2    
       Mean   :<num>   Mean   :2    
       3rd Qu.:<num>   3rd Qu.:2    
       Max.   :<num>   Max.   :2    
      

# bagEarth.formula needs a formula

    Code
      caret:::bagEarth.formula(iris[, 1:4])
    Condition
      Error in `caret:::bagEarth.formula()`:
      ! method is only for formula objects

# bagEarth fits from a formula and formats its terms

    Code
      format(fit)
    Output
      (   <num>
        -  <num> * h(<num>-Girth)
        +  <num> * h(Girth-<num>)
        + <num> * h(Height-74)
      +  <num>
        -  <num> * h(<num>-Girth)
        +  <num> * h(Girth-<num>)
        - <num> * h(77-Height)
        + <num> * h(Height-77)
       ) / 2 

