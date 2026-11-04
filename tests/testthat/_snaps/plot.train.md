# plot.train draws scatter and line plots for a tuned model

    Code
      plot(fit, plotType = "nope")
    Condition
      Error in `plot.train()`:
      ! plotType must be either level, scatter or line

# plot.train errors when no tuning parameter varies

    Code
      plot(fit)
    Condition
      Error in `plot.train()`:
      ! There are no tuning parameters with more than 1 value.

# plot.train needs something to plot

    Code
      plot(none)
    Condition
      Error in `plot.train()`:
      ! There are no tuning parameters for this model.

---

    Code
      plot(one, plotType = "level")
    Condition
      Error in `plot.train()`:
      ! There must be at least 2 tuning parameters with multiple values

# plot.train warns about adaptive resampling

    When using adaptive resampling, this plot may not accurately capture the relationship between the tuning parameters and model performance.

