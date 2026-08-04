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

