# ggplot.train validates output and needs a varying parameter

    Code
      ggplot(fit, output = "nope")
    Condition
      Error in `ggplot.train()`:
      ! 'outout' should be either 'data', 'ggplot' or 'layered'

---

    Code
      ggplot(reg)
    Condition
      Error in `ggplot.train()`:
      ! There are no tuning parameters with more than 1 value.

