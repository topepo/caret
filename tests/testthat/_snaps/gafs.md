# gafsControl errors working

    Code
      gafsControl(method = "larry")
    Condition
      Error in `gafsControl()`:
      ! method should be one of: "cv", "boot", "repeatedcv", "LGOCV" or "LOOCV"

---

    Code
      gafsControl(metric = c("larry", "harry", "moe"))
    Condition
      Error in `gafsControl()`:
      ! 'metric' should be a two-element named vector. See ?gafsControl

---

    Code
      gafsControl(maximize = c("larry", "harry", "moe"))
    Condition
      Error in `gafsControl()`:
      ! 'maximize' should be a two-element named vector. See ?gafsControl

# ga_func_check reports the functions a search needs

    Code
      caret:::ga_func_check(caretGA[c("fit", "pred")])
    Condition
      Error in `caret:::ga_func_check()`:
      ! The following functions are missing from the 'func' argument: fitness_intern,fitness_extern,initial,selection,crossover,mutation,selectIter

# ga_func_check checks each function's arguments

    Code
      caret:::ga_func_check(wrong_args)
    Condition
      Error in `caret:::ga_func_check()`:
      ! Arguments to function mutation should be { population, parent, ... }  and these were given { x, y }

# print.gafs describes the search

    Code
      print(ga)
    Output
      
      Genetic Algorithm Feature Selection
      
      60 samples
      4 predictors
      
      Maximum generations: 3 
      Population per generation: 4 
      Crossover probability: <num> 
      Mutation probability: <num> 
      Elitism: 0 
      
      Internal performance values: RMSE, Rsquared, MAE
      Subset selection driven to minimize internal RMSE 
      
      External performance values: RMSE, Rsquared, MAE
      Best iteration chose by minimizing external RMSE 
      External resampling method: Cross-Validated (3 fold) 
      
      During resampling:
        * the top 4 selected variables (out of a possible 4):
          x1 (100%), x2 (100%), x3 (100%), x4 (<num>%)
        * on average, <num> variables were selected (min = 3, max = 4)
      
      In the final search using the entire training set:
         * 4 features selected at iteration 1 including:
           x1, x2, x3, x4  
         * external performance at this iteration is
      
             RMSE    Rsquared         MAE 
           <num>      <num>      <num> 
      

# print.gafs names the classes and a varying mutation rate

    Code
      print(ga)
    Output
      
      Genetic Algorithm Feature Selection
      
      60 samples
      4 predictors
      2 classes: 'one', 'two' 
      
      Maximum generations: 2 
      Population per generation: 4 
      Crossover probability: <num> 
      Mutation probability: variable
      Elitism: 0 
      
      Internal performance values: Accuracy, Kappa
      Subset selection driven to maximize internal Accuracy 
      
      External performance values: Accuracy, Kappa
      Best iteration chose by maximizing external Accuracy 
      External resampling method: Cross-Validated (3 fold) 
      
      During resampling:
        * the top 4 selected variables (out of a possible 4):
          x1 (100%), x2 (<num>%), x3 (<num>%), x4 (<num>%)
        * on average, <num> variables were selected (min = 2, max = 3)
      
      In the final search using the entire training set:
         * 3 features selected at iteration 1 including:
           x1, x2, x3  
         * external performance at this iteration is
      
         Accuracy       Kappa 
           <num>      <num> 
      

# varImp.gafs needs the differences to have been computed

    Code
      varImp(ga)
    Condition
      Error in `varImp.gafs()`:
      ! must have used `differences = TRUE`

# plot.gafs checks the metric it was asked for

    Code
      plot(ga, metric = "Bogus")
    Condition
      Error in `plot.gafs()`:
      ! ' Bogus ' not computed in both estimates

---

    Code
      plot(ga, metric = "Bogus", estimate = "internal")
    Condition
      Error in `plot.gafs()`:
      ! ' Bogus ' not computed internally

