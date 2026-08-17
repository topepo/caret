# print.pcaNNet reports the PCA and network structure

    Code
      print(fit)
    Output
      Neural Network Model with PCA Pre-Processing
      
      Created from 150 samples and 4 variables
      PCA needed 3 components to capture 99 percent of the variance
      
      a 3-3-3 network with 24 weights
      options were -
      

# predict.pcaNNet rejects objects of the wrong class

    Code
      caret:::predict.pcaNNet(structure(list(), class = "notPcaNNet"), iris)
    Condition
      Error in `caret:::predict.pcaNNet()`:
      ! object not of class "pcaNNet"

# predict.pcaNNet checks the object's class

    Code
      caret:::predict.pcaNNet(structure(list(), class = "nope"))
    Condition
      Error in `caret:::predict.pcaNNet()`:
      ! object not of class "pcaNNet"

