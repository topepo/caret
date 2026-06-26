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
      

