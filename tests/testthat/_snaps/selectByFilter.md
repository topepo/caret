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

