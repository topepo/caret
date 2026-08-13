# the plot methods warn when 'data' is supplied

    Code
      invisible(densityplot(fit, data = iris))
    Condition
      Warning in `densityplot.train()`:
      explicit 'data' specification ignored

---

    Code
      invisible(histogram(fit, data = iris))
    Condition
      Warning in `histogram.train()`:
      explicit 'data' specification ignored

---

    Code
      invisible(stripplot(fit, data = iris))
    Condition
      Warning in `stripplot.train()`:
      explicit 'data' specification ignored

---

    Code
      invisible(xyplot(fit, data = iris))
    Condition
      Warning in `xyplot.train()`:
      explicit 'data' specification ignored

# the plot methods reject LOOCV/oob resampling

    Code
      densityplot(fit)
    Condition
      Error in `densityplot.train()`:
      ! Resampling plots cannot be done with leave-out-out CV or out-of-bag resampling

---

    Code
      histogram(fit)
    Condition
      Error in `histogram.train()`:
      ! Resampling plots cannot be done with leave-out-out CV or out-of-bag resampling

---

    Code
      stripplot(fit)
    Condition
      Error in `stripplot.train()`:
      ! Resampling plots cannot be done with leave-out-out CV or out-of-bag resampling

---

    Code
      xyplot(fit)
    Condition
      Error in `xyplot.train()`:
      ! Resampling plots cannot be done with leave-out-out CV or out-of-bag resampling

