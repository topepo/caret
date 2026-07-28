# conversion to range trans

    Code
      preProcess(rng_dat1, "range", rangeBounds = "")
    Condition
      Error in `preProcess.default()`:
      ! 'rangeBounds' should be a two-element numeric vector

---

    Code
      preProcess(rng_dat1, "range", rangeBounds = c(0.4, -0.7))
    Condition
      Error in `preProcess.default()`:
      ! 'rangeBounds' interval is empty

