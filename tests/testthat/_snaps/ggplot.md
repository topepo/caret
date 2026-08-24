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

# a level plot needs two tuning parameters

    Code
      ggplot(fit, plotType = "level")
    Condition
      Error in `ggplot.train()`:
      ! Two tuning parameters are required for a level plot

# random_search_plot needs more than one parameter combination

    Code
      caret:::random_search_plot(fit)
    Condition
      Error in `caret:::random_search_plot()`:
      ! Can't plot results with a single tuning parameter combination

# random search plots refuse what they cannot draw

    Code
      caret:::random_search_plot(random_search_obj(num = 2, constant = TRUE))
    Condition
      Error in `caret:::random_search_plot()`:
      ! Can't plot results with a single tuning parameter combination

---

    Code
      caret:::random_search_plot(random_search_obj(num = 0, other = 4))
    Condition
      Error in `caret:::random_search_plot()`:
      ! There are 4 non-numeric variables; I don't have code for that Dave

---

    Code
      caret:::random_search_plot(random_search_obj(num = 2, other = 2))
    Condition
      Error in `caret:::random_search_plot()`:
      ! There are 2 numeric tuning variables and 2 non-numeric variables; I don't have code for that Dave

# ggplot.train warns about adaptive resampling

    When using adaptive resampling, this plot may not accurately capture the relationship between the tuning parameters and model performance.

# ggplot.train refuses more than four tuning parameters

    Code
      ggplot2::ggplot(fake)
    Condition
      Error in `ggplot.train()`:
      ! The function can only handle <= 4 tuning parameters for scatter plots. Use output = 'ggplot' to create your own

---

    Code
      ggplot2::ggplot(fake, plotType = "level")
    Condition
      Error in `ggplot.train()`:
      ! The function can only handle <= 4 tuning parameters for level plots. Use output = 'ggplot' to create your own

