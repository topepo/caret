# icr handles a single predictor and single-row prediction

    ICA is a group transformation and only a single predictor is listed. This method is eliminated.

# icr requires a numeric outcome

    Code
      icr(iris[, 1:4], iris$Species, n.comp = 2)
    Condition
      Error in `icr.default()`:
      ! y must be numeric

# predict.icr rejects missing data in newdata

    Code
      predict(fit, na_row)
    Condition
      Error in `predict.icr()`:
      ! missing values in 'x'

# predict.icr checks the object's class

    Code
      caret:::predict.icr(structure(list(), class = "nope"))
    Condition
      Error in `caret:::predict.icr()`:
      ! object not of class "icr"

# print.icr reports when there are no coefficients

    Code
      print(fit)
    Output
      Independent Component Regression
      
      Created from 32 samples and 2 variables
      
      No coefficients
      

