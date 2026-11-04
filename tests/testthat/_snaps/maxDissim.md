# maxDissim warns and falls back to indices without row names

    Cannot use rownames; swithcing to indices

# maxDissim prints progress when verbose

    Code
      maxDissim(maxdiss_base, maxdiss_pool, n = 1, verbose = TRUE)
    Output
        adding:
      Iter 1 
      Number of candidates: 3 
      Sampling from 3 samples
      new sample: 3 
      [1] 3

# maxDissim validates its arguments

    Code
      maxDissim(maxdiss_base, maxdiss_pool[1, , drop = FALSE])
    Condition
      Error in `maxDissim()`:
      ! there must be at least 2 samples in b

---

    Code
      maxDissim(maxdiss_base[, 1, drop = FALSE], maxdiss_pool)
    Condition
      Error in `maxDissim()`:
      ! a and b must have the same number of columns

---

    Code
      maxDissim(maxdiss_base, maxdiss_pool, n = 99)
    Condition
      Error in `maxDissim()`:
      ! n must be less than nrow(b)

---

    Code
      maxDissim(maxdiss_base, maxdiss_pool, randomFrac = 2)
    Condition
      Error in `maxDissim()`:
      ! randomFrac must be in (0, 1]

---

    Code
      maxDissim(maxdiss_base, maxdiss_pool, randomFrac = 0)
    Condition
      Error in `maxDissim()`:
      ! randomFrac must be in (0, 1]

