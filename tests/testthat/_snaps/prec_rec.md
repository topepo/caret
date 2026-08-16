# precision errors on bad input

    Code
      precision(as.character(pr_pred), pr_truth)
    Condition
      Error in `precision.default()`:
      ! input data must be a factor

---

    Code
      precision(factor(letters[1:3]), factor(letters[1:3]))
    Condition
      Error in `precision.default()`:
      ! input data must have the same two levels

# recall errors on bad input

    Code
      recall(as.character(pr_pred), pr_truth)
    Condition
      Error in `recall.default()`:
      ! input data must be a factor

---

    Code
      recall(factor(letters[1:3]), factor(letters[1:3]))
    Condition
      Error in `recall.default()`:
      ! input data must have the same two levels

# F_meas errors on bad input

    Code
      F_meas(as.character(pr_pred), pr_truth)
    Condition
      Error in `F_meas.default()`:
      ! input data must be a factor

---

    Code
      F_meas(factor(letters[1:3]), factor(letters[1:3]))
    Condition
      Error in `F_meas.default()`:
      ! input data must have the same two levels

# prSummary rejects outcomes with more than two classes

    Code
      prSummary(d, lev = c("a", "b", "c"))
    Condition
      Error in `prSummary()`:
      ! Your outcome has 3 levels. `prSummary`` function isn't appropriate. FALSE

# prSummary errors when predicted and observed levels differ

    Code
      prSummary(d, lev = c("A", "B"))
    Condition
      Error:
      ! Levels of observed and predicted data do not match.

# prSummary errors when class probabilities are missing

    Code
      prSummary(d, lev = c("A", "B"))
    Condition
      Error:
      ! Class probabilities are needed to score models using the area under the PR curve. Set `classProbs = TRUE` in the trainControl() function.

