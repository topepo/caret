# conversion to range trans

    Code
      preProcess(rng_dat1, "range", rangeBounds = "")
    Condition
      Error in `preProcess.default()`:
      ! 'rangeBounds' should be a two-element numeric vector

---

    Code
      preProcess(rng_dat1, "range", rangeBounds = c(0.4, -0.7))
    Condition
      Error in `preProcess.default()`:
      ! 'rangeBounds' interval is empty

# print.preProcess summarises the transformations

    Code
      print(preProcess(x, method = c("center", "scale")))
    Output
      Created from 10 samples and 4 variables
      
      Pre-processing:
        - centered (4)
        - ignored (0)
        - scaled (4)
      

# YeoJohnson reports when nothing could be transformed

    Code
      pp <- preProcess(dat, method = "YeoJohnson", verbose = TRUE)
    Output
       all of the transformations failed

# expoTrans is dropped for columns it sends to infinity

    Expo. transform induced infinite values in several predictors and is ommitted: big

# scaling substitutes one for a standard deviation it cannot compute

    Std. deviations could not be computed for: empty

# preProcess reports its progress when asked

    Code
      pp <- preProcess(dat, method = "bagImpute", verbose = TRUE)
    Output
      Computing bagging models for 3 predictors... done

---

    Code
      pp <- preProcess(dat, method = "medianImpute", verbose = TRUE)
    Output
      Computing medians for 3 predictors... done

---

    Code
      pp <- preProcess(dat, method = "range", verbose = TRUE)
    Output
      Calculating 3 statistcs for scaling to a range

# preProcess reports progress for the component methods

    Code
      pp <- preProcess(dat, method = "ica", n.comp = 2, verbose = TRUE)
    Output
      Calculating 4 means for centering
      Calculating 4 standard deviations for scaling
      Computing ICA loadings for 4 predictors

# predict.preProcess errors when everything was filtered out

    Code
      predict(pp, dat)
    Condition
      Error in `predict.preProcess()`:
      ! All predctors were removed as determined by `preProcess`

# preProcess warns when the correlation matrix cannot be computed

    correlation matrix could not be computed:
     Error in cor(x[, !(colnames(x) %in% c(method$ignore, method$remove)),  : 
      'x' is empty
    

# nearest-neighbour imputation needs something to go on

    Code
      predict(pp, all_missing)
    Condition
      Error in `FUN()`:
      ! cannot impute when all predictors are missing in the new data point

# preProcess applies the spatial sign to the components

    Code
      print(pp)
    Output
      Created from 30 samples and 3 variables
      
      Pre-processing:
        - centered (3)
        - ignored (0)
        - principal component signal extraction (3)
        - scaled (3)
        - spatial sign transformation (2)
      
      PCA used 2 components as specified and will be used in the spatial sign transformation

# preProcess applies the spatial sign to independent components

    Code
      print(pp)
    Output
      Created from 30 samples and 3 variables
      
      Pre-processing:
        - centered (3)
        - independent component signal extraction (3)
        - ignored (0)
        - scaled (3)
        - spatial sign transformation (2)
      
      ICA used 2 components and will be used in the spatial sign transformation

# print.preProcess counts the Box-Cox transformations it could not fit

    Code
      print(pp)
    Output
      Created from 30 samples and 2 variables
      
      Pre-processing:
        - Box-Cox transformation (2)
        - ignored (0)
      
      Lambda estimates for Box-Cox transformation:
      0.3 (#NA: 1)
      

