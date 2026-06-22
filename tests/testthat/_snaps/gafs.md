# gafsControl errors working

    Code
      gafsControl(method = "larry")
    Condition
      Error in `gafsControl()`:
      ! method should be one of: "cv", "boot", "repeatedcv", "LGOCV" or "LOOCV"

---

    Code
      gafsControl(metric = c("larry", "harry", "moe"))
    Condition
      Error in `gafsControl()`:
      ! 'metric' should be a two-element named vector. See ?gafsControl

---

    Code
      gafsControl(maximize = c("larry", "harry", "moe"))
    Condition
      Error in `gafsControl()`:
      ! 'maximize' should be a two-element named vector. See ?gafsControl

