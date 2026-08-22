# safsControl errors working

    Code
      safsControl(method = "larry")
    Condition
      Error in `safsControl()`:
      ! method should be one of: "cv", "boot", "repeatedcv", "LGOCV" or "LOOCV"

---

    Code
      safsControl(metric = c("larry", "harry", "moe"))
    Condition
      Error in `safsControl()`:
      ! 'metric' should be a two-element named vector. See ?safsControl

---

    Code
      safsControl(maximize = c("larry", "harry", "moe"))
    Condition
      Error in `safsControl()`:
      ! 'maximize' should be a two-element named vector. See ?safsControl

---

    Code
      safsControl(holdout = -1)
    Condition
      Error in `safsControl()`:
      ! 'holdout' should be in [0, 1)

---

    Code
      safsControl(improve = 1)
    Condition
      Error in `safsControl()`:
      ! 'improve' should be >= 2

# safs runs with random-forest functions

    Variable differences could not be computed: 
      Not enough results to compute differences
    

# sa_func_check reports the functions a search needs

    Code
      caret:::sa_func_check(caretSA[c("fit", "pred")])
    Condition
      Error in `caret:::sa_func_check()`:
      ! The following functions are missing from the 'func' argument: fitness_intern,fitness_extern,initial,perturb,prob,selectIter

---

    Code
      caret:::sa_func_check(wrong_args)
    Condition
      Error in `caret:::sa_func_check()`:
      ! Arguments to function perturb should be { x, vars, number }  and these were given { x, y }

# print.safs describes the search

    Code
      print(sa)
    Output
      
      Simulated Annealing Feature Selection
      
      60 samples
      4 predictors
      
      Maximum search iterations: 4 
      
      Internal performance values: RMSE, Rsquared, MAE
      Subset selection driven to minimize internal RMSE 
      
      External performance values: RMSE, Rsquared, MAE
      Best iteration chose by minimizing external RMSE 
      External resampling method: Cross-Validated (3 fold) 
      
      During resampling:
        * the top 3 selected variables (out of a possible 4):
          x1 (<num>%), x2 (<num>%), x4 (<num>%)
        * on average, <num> variables were selected (min = 1, max = 2)
      
      In the final search using the entire training set:
         * 2 features selected at iteration 3 including:
           x1, x2  
         * external performance at this iteration is
      
            RMSE   Rsquared        MAE 
           <num>      <num>      <num> 
      

# print.safs names the classes and the restart rule

    Code
      print(sa)
    Output
      
      Simulated Annealing Feature Selection
      
      60 samples
      4 predictors
      2 classes: 'one', 'two' 
      
      Maximum search iterations: 4 
      Restart after 2 iterations without improvement (<num> restarts on average)
      
      Internal performance values: Accuracy, Kappa
      Subset selection driven to maximize internal Accuracy 
      
      External performance values: Accuracy, Kappa
      Best iteration chose by maximizing external Accuracy 
      External resampling method: Cross-Validated (3 fold) 
      Subsampling for internal fitness calculation: 20%
      
      During resampling:
        * the top 3 selected variables (out of a possible 4):
          x1 (<num>%), x3 (<num>%), x4 (<num>%)
        * on average, <num> variables were selected (min = 1, max = 2)
      
      In the final search using the entire training set:
         * 1 features selected at iteration 3 including:
           x3  
         * external performance at this iteration is
      
         Accuracy       Kappa 
           <num>      <num> 
      

# varImp.safs needs the differences to have been computed

    Code
      varImp(sa)
    Condition
      Error in `varImp.safs()`:
      ! must have used `differences = TRUE`

# plot.safs checks the metric it was asked for

    Code
      plot(sa, metric = "Bogus")
    Condition
      Error in `plot.safs()`:
      ! ' Bogus ' not computed in both estimates

---

    Code
      plot(sa, metric = "Bogus", estimate = "internal")
    Condition
      Error in `plot.safs()`:
      ! ' Bogus ' not computed internally

# safs needs a seed per resample plus one

    Code
      safs(x = dat[, 1:4], y = dat$y, safsControl = ctrl, iters = 2, method = "lm",
      trControl = trainControl(method = "cv", number = 3))
    Condition
      Error in `safs.default()`:
      ! There must be at least 4 random number seeds passed to safsControl

# safs names an unnamed external fitness result

    Code
      fit <- safs(x = dat[, 1:4], y = dat$y, safsControl = ctrl, iters = 2,
      differences = FALSE, method = "lm", trControl = trainControl(method = "cv",
        number = 3))
    Condition
      Warning in `safs.default()`:
      The external fitness results should be a *named* vector; new name(s) are external1, external2, external3
      Warning in `safs.default()`:
      The metric 'RMSE' is not created by the external summary function; 'external1' will be used instead

# safs falls back when the external metric is not computed

    The metric 'Bogus' is not created by the external summary function; 'RMSE' will be used instead

