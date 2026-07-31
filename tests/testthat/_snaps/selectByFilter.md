# anovaScores rejects factor predictors

    Code
      caret:::anovaScores(factor(c("a", "b")), factor(c("a", "b")))
    Condition
      Error in `caret:::anovaScores()`:
      ! The predictors should be numeric

# nullModel predicts the mean for a numeric outcome

    Code
      predict(nm, type = "prob")
    Condition
      Error in `predict.nullModel()`:
      ! ony raw predicitons are applicable to regression models

# print.nullModel labels the model type correctly

    Code
      print(caret:::nullModel(y = factor(c("a", "a", "b"))))
    Output
      Null Classification Model
      
      Call:
      nullModel.default(y = factor(c("a", "a", "b")))
      
      Predicted Value: a 

---

    Code
      print(caret:::nullModel(y = c(1, 2, 3)))
    Output
      Null Regression Model
      
      Call:
      nullModel.default(y = c(1, 2, 3))
      
      Predicted Value: 2 

# sbf runs and its methods behave (default interface)

    Code
      print(sf)
    Output
      
      Selection By Filter
      
      Outer resampling method: Cross-Validated (3 fold) 
      
      Resampling performance:
      
       Accuracy  Kappa AccuracySD KappaSD
         0.7067 0.3847    0.04619  0.1019
      
      Using the training set, 7 variables were selected:
         TwoFactor1, TwoFactor2, Linear02, Linear03, Linear04...
      
      During resampling, the top 5 selected variables (out of a possible 7):
         TwoFactor2 (100%), Linear02 (66.7%), Linear03 (66.7%), Linear04 (66.7%), Linear06 (66.7%)
      
      On average, 4.3 variables were selected (min = 3, max = 6)

