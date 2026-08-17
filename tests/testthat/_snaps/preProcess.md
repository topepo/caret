# median Impute works for matrix with named columns

    These variables are never filled: Var.3

# median Impute works for data.frames

    These variables are never filled: Var.3

# preProcess print method

    Code
      print(preProcess(iris[, 1:4], method = c("center", "scale", "pca")))
    Output
      Created from 150 samples and 4 variables
      
      Pre-processing:
        - centered (4)
        - ignored (0)
        - principal component signal extraction (4)
        - scaled (4)
      
      PCA needed 2 components to capture 95 percent of the variance

---

    Code
      print(preProcess(iris[, 1:4], method = "range"))
    Output
      Created from 150 samples and 4 variables
      
      Pre-processing:
        - ignored (0)
        - re-scaling to [0, 1] (4)
      

# preProcess reports its progress when verbose

    Code
      pp <- preProcess(dat, method = c("zv", "BoxCox", "center", "scale"), verbose = TRUE)
    Output
        1 zero variance predictors were removed.
      Estimating Box-Cox transformations for 3 predictors...
      Box-Cox failed for: c applying them to training data
      Calculating 3 means for centering
      Calculating 3 standard deviations for scaling

# preProcess adds a zero-variance filter for the correlation filter

    Code
      pp <- preProcess(dat, method = "corr", verbose = TRUE)
    Output
      A zero-variance filter was added for the correlation filter
        1 zero variance predictors were removed.
        1 highly correlated predictors were removed.

# preProcess reports near-zero variance and conditional filters

    Code
      pp <- preProcess(dat, method = "nzv", verbose = TRUE)
    Output
        1 near-zero variance predictors were removed.

# preProcess reports the other transformations when verbose

    Code
      pp <- preProcess(dat, method = c("YeoJohnson", "invHyperbolicSine"), verbose = TRUE)
    Output
       applying invHyperbolicSine
       applying them to training data

---

    Code
      pp2 <- preProcess(dat, method = "expoTrans", verbose = TRUE)
    Output
      Estimating exponential transformations for 3 predictors... applying them to training data

# preProcess reports imputation and dimension reduction

    Code
      pp <- preProcess(dat, method = c("knnImpute", "pca"), verbose = TRUE)
    Output
      Calculating 3 means for centering
      Calculating 3 standard deviations for scaling
      Computing PCA loadings for 3 predictors

# preProcess requires a matrix or data frame

    Code
      preProcess(1:10)
    Condition
      Error:
      ! Matrices or data frames are required for preprocessing

# print.preProcess summarises the transformations

    Code
      print(preProcess(dat, method = c("BoxCox", "center", "scale")))
    Output
      Created from 40 samples and 3 variables
      
      Pre-processing:
        - Box-Cox transformation (3)
        - centered (3)
        - ignored (0)
        - scaled (3)
      
      Lambda estimates for Box-Cox transformation:
      <num>, <num>, <num>

---

    Code
      print(preProcess(dat, method = "YeoJohnson"))
    Output
      Created from 40 samples and 3 variables
      
      Pre-processing:
        - Yeo-Johnson transformation (3)
        - ignored (0)
      
      Lambda estimates for Yeo-Johnson transformation:
      -<num>, -<num>, -<num>

# print.preProcess describes PCA and ICA components

    Code
      print(preProcess(dat, method = "pca", thresh = 0.8))
    Output
      Created from 40 samples and 6 variables
      
      Pre-processing:
        - centered (6)
        - ignored (0)
        - principal component signal extraction (6)
        - scaled (6)
      
      PCA needed 5 components to capture 80 percent of the variance

---

    Code
      print(preProcess(dat, method = "pca", pcaComp = 2))
    Output
      Created from 40 samples and 6 variables
      
      Pre-processing:
        - centered (6)
        - ignored (0)
        - principal component signal extraction (6)
        - scaled (6)
      
      PCA used 2 components as specified

---

    Code
      print(preProcess(dat, method = "ica", n.comp = 2))
    Output
      Created from 40 samples and 6 variables
      
      Pre-processing:
        - centered (6)
        - independent component signal extraction (6)
        - ignored (0)
        - scaled (6)
      
      ICA used 2 components

# print.preProcess notes the wildcard spatial sign

    Code
      print(pp)
    Output
      Created from 40 samples and 6 variables
      
      Pre-processing:
        - centered (6)
        - ignored (0)
        - principal component signal extraction (6)
        - scaled (6)
        - spatial sign transformation (2)
      
      PCA used 2 components as specified and will be used in the spatial sign transformation

# print.preProcess summarises many Box-Cox lambdas

    Code
      print(preProcess(dat, method = "BoxCox"))
    Output
      Created from 40 samples and 12 variables
      
      Pre-processing:
        - Box-Cox transformation (12)
        - ignored (0)
      
      Lambda estimates for Box-Cox transformation:
          Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
      -<num> -<num>  <num>  <num>  <num>  <num> 
      

---

    Code
      print(preProcess(dat, method = "YeoJohnson"))
    Output
      Created from 40 samples and 12 variables
      
      Pre-processing:
        - Yeo-Johnson transformation (12)
        - ignored (0)
      
      Lambda estimates for Yeo-Johnson transformation:
         Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      -<num> -<num> -<num> -<num> -<num> -<num> 
      

# preProcess warns about predictors it cannot scale or range

    These variables have zero variances: flat

---

    No variation for for: flat

# preProcess reports conditionally zero-variance predictors

    Code
      pp <- preProcess(dat, method = "conditionalX", outcome = y, verbose = TRUE)
    Output
        1 conditionally zero variance predictors.

# the correlation filter needs more than one predictor

    Code
      preProcess(dat, method = "corr")
    Condition
      Error in `findCorrelation_exact()`:
      ! only one variable given

# preProcess drops transformations that fail for every predictor

    Code
      pp <- preProcess(dat, method = "BoxCox", verbose = TRUE)
    Output
      Estimating Box-Cox transformations for 2 predictors...
      Box-Cox failed for: a, b applying them to training data

