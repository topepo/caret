# bag validates its control and arguments

    Code
      bag(iris[, 1:4], iris$Species)
    Condition
      Error in `bag.default()`:
      ! Please specify 'bagControl' with the appropriate functions

---

    Code
      bag(iris[, 1:4], iris$Species, vars = 0, bagControl = full_ctrl)
    Condition
      Error in `bag.default()`:
      ! vars must be an integer > 0

---

    Code
      bag(iris[, 1:4], iris$Species, bagControl = bagControl())
    Condition
      Error in `bag.default()`:
      ! The control arguments 'fit', 'predict' and 'aggregate' should have non-NULL values

# bag fits an ensemble and predicts

    Code
      print(fit)
    Output
      
      Call:
      bag.default(x = iris[, 1:4], y = iris$Species, B = 5, bagControl
       = bagControl(fit = ldaBag$fit, predict = ldaBag$pred, aggregate
       = ldaBag$aggregate))
      
      
      B: 5 
      Training data: 4 variables and 150 samples
      All variables were used in each model

---

    Code
      summary(fit)
    Output
      
      Call:
      bag.default(x = iris[, 1:4], y = iris$Species, B = 5, bagControl
       = bagControl(fit = ldaBag$fit, predict = ldaBag$pred, aggregate
       = ldaBag$aggregate))
      
      Out of bag statistics (B = 5):
      
             Accuracy  Kappa
        0.0%   0.9464 0.9190
        2.5%   0.9484 0.9220
       25.0%   0.9661 0.9491
       50.0%   0.9818 0.9726
       75.0%   1.0000 1.0000
       97.5%   1.0000 1.0000
      100.0%   1.0000 1.0000
      

