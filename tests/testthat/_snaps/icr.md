# icr handles a single predictor and single-row prediction

    ICA is a group transformation and only a single predictor is listed. This method is eliminated.

# icr requires a numeric outcome

    Code
      icr(iris[, 1:4], iris$Species, n.comp = 2)
    Condition
      Error in `icr.default()`:
      ! y must be numeric

