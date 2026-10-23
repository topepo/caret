# yardstickSummary gives informative errors

    Code
      yardstickSummary(yardstick::rmse)
    Condition
      Error in `yardstickSummary()`:
      ! `metrics` must be a metric set created by `yardstick::metric_set()`, not a <numeric_metric> object.

---

    Code
      reg_summ(dat, lev = c("a", "b"))
    Condition
      Error in `reg_summ()`:
      ! The metric set contains regression metrics but the outcome is a factor.

---

    Code
      cls_summ(dat[, c("obs", "pred")], c("a", "b"))
    Condition
      Error in `cls_summ()`:
      ! The metric set contains metrics that require class probabilities.
      i Set `classProbs = TRUE` in `trainControl()`.

---

    Code
      cls_summ(num_dat)
    Condition
      Error in `cls_summ()`:
      ! The metric set contains classification metrics but the outcome is numeric.

# is_minimized warns about metrics that are optimal at zero

    The metric "msd" is optimal at zero, which caret cannot optimize; larger values will be treated as better.
    i Choose a different `metric` to tune the model.

