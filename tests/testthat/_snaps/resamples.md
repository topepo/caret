# resample calculations

    'rlm' failed to converge in 20 steps

# as.matrix.resamples returns a resample-by-model matrix

    Code
      as.matrix(rs_fixture, metric = "nope")
    Condition
      Error in `as.matrix.resamples()`:
      ! no columns fit that metric

