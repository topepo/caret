# get_vector passes vectors through and unwraps one-column frames

    Code
      caret:::get_vector(data.frame(a = 1:3, b = 4:6))
    Condition
      Error in `caret:::get_vector()`:
      ! Only one column should be available

# preproc_dots warns only about leftover preProc arguments

    Code
      caret:::preproc_dots(preProcOptions = list(k = 5))
    Condition
      Warning:
      When using a recipe with `train`, `preProcOptions` will be ignored.

