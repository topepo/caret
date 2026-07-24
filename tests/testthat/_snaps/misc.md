# var_seq builds a default grid of predictor counts

    Code
      vs <- caret:::var_seq(2, classification = FALSE, len = 3)
    Output
      note: only 1 unique complexity parameters in default grid. Truncating the grid to 1 .
      

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

