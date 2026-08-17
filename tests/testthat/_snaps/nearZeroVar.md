# checkResamples validates its outcome

    Code
      checkResamples(list(1:4), x, 1:4)
    Condition
      Error in `checkResamples()`:
      ! y must be a factor

---

    Code
      checkResamples(list(1:4), x, one_level)
    Condition
      Error in `checkResamples()`:
      ! y must have at least 2 levels

