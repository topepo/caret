# BoxCoxTrans validates its input

    Code
      BoxCoxTrans(factor(letters))
    Condition
      Error in `BoxCoxTrans.default()`:
      ! y must be numeric

---

    Code
      predict(BoxCoxTrans(skew_y), "abc")
    Condition
      Error in `predict.BoxCoxTrans()`:
      ! newdata should be a numeric vector

---

    Code
      predict(BoxCoxTrans(skew_y), c(-1, 1, 2))
    Condition
      Warning in `predict.BoxCoxTrans()`:
      newdata should have values > 0
      Warning in `log()`:
      NaNs produced
    Output
      [1]       NaN 0.0000000 0.6931472

# expoTrans validates its newdata

    Code
      predict(expoTrans(skew_y), "abc")
    Condition
      Error in `predict.expoTrans()`:
      ! newdata should be a numeric vector

