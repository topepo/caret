# bad class levels

    Code
      foo(dat)
    Condition
      Error:
      ! At least one of the class levels is not a valid R variable name; This will cause errors when class probabilities are generated because the variables names will be converted to  X0, X1 . Please use factor levels that can be used as valid R variable names  (see ?make.names for help).

# no class probs with ROC

    Code
      foo(dat)
    Condition
      Error:
      ! Class probabilities are needed to score models using the area under the ROC curve. Set `classProbs = TRUE` in the trainControl() function.

# numeric y and classification

    You are trying to do regression and your outcome only has two possible values Are you trying to do classification? If so, use a 2 level factor as your outcome column.

# 3+ classes and twoClassSummary

    Code
      foo()
    Condition
      Error in `ctrl$summaryFunction()`:
      ! Your outcome has 7 levels. The twoClassSummary() function isn't appropriate.

