# check_dims requires x rows to match the outcome length

    Code
      caret:::check_dims(m, 1:3)
    Condition
      Error in `caret:::check_dims()`:
      ! nrow(x) == n is not TRUE

---

    Code
      caret:::check_dims(matrix(1:3, nrow = 1), 1)
    Condition
      Error in `caret:::check_dims()`:
      ! nrow(x) > 1 is not TRUE

