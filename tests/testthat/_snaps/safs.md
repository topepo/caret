# safsControl errors working

    Code
      safsControl(method = "larry")
    Condition
      Error in `safsControl()`:
      ! method should be one of: "cv", "boot", "repeatedcv", "LGOCV" or "LOOCV"

---

    Code
      safsControl(metric = c("larry", "harry", "moe"))
    Condition
      Error in `safsControl()`:
      ! 'metric' should be a two-element named vector. See ?safsControl

---

    Code
      safsControl(maximize = c("larry", "harry", "moe"))
    Condition
      Error in `safsControl()`:
      ! 'maximize' should be a two-element named vector. See ?safsControl

---

    Code
      safsControl(holdout = -1)
    Condition
      Error in `safsControl()`:
      ! 'holdout' should be in [0, 1)

---

    Code
      safsControl(improve = 1)
    Condition
      Error in `safsControl()`:
      ! 'improve' should be >= 2

