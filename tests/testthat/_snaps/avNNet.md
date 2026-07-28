# avNNet fits an averaged classifier and predicts each type

    Code
      print(fit)
    Output
      Model Averaged Neural Network with 3 Repeats  
      
      a 4-3-3 network with 27 weights
      options were -
      

# predict.avNNet rejects objects of the wrong class

    Code
      caret:::predict.avNNet(structure(list(), class = "notAvNNet"), iris)
    Condition
      Error in `caret:::predict.avNNet()`:
      ! object not of class "avNNet"

