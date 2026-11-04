# print.varImp.train shows the model and importances

    Code
      print(vi)
    Output
      lm variable importance
      
        Overall
      a     100
      b      50
      c       0

---

    Code
      print(vi, top = 2)
    Output
      lm variable importance
      
        only 2 most important variables shown (out of 3)
      
        Overall
      a     100
      b      50

# print.varImp.train collapses two-column importances

    Code
      print(vi)
    Output
      ROC curve variable importance
      
        Importance
      a        100
      b          0

# print.varImp.train sorts multi-class importances by the maximum

    Code
      print(vi)
    Output
      pam variable importance
      
        variables are sorted by maximum importance across the classes
          x  y  z
      b 100  0 50
      a  10 30 80
      c  20 25  0

