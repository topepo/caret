# sensitivity errors on bad input

    Code
      sensitivity(as.character(sens_pred), sens_truth)
    Condition
      Error in `sensitivity.default()`:
      ! inputs must be factors

---

    Code
      sensitivity(factor(letters[1:3]), factor(letters[1:3]))
    Condition
      Error in `sensitivity.default()`:
      ! input data must have the same two levels

---

    Code
      sensitivity(as.table(matrix(1:6, nrow = 2)))
    Condition
      Error in `!all.equal(nrow(data), ncol(data))`:
      ! invalid argument type

# specificity errors on bad input

    Code
      specificity(as.character(sens_pred), sens_truth)
    Condition
      Error in `specificity.default()`:
      ! input data must be a factor

---

    Code
      specificity(factor(letters[1:3]), factor(letters[1:3]))
    Condition
      Error in `specificity.default()`:
      ! input data must have the same two levels

---

    Code
      specificity(as.table(matrix(1:6, nrow = 2)))
    Condition
      Error in `!all.equal(nrow(data), ncol(data))`:
      ! invalid argument type

# posPredValue errors on bad input

    Code
      posPredValue(as.character(sens_pred), sens_truth)
    Condition
      Error in `posPredValue.default()`:
      ! inputs must be factors

---

    Code
      posPredValue(factor(letters[1:3]), factor(letters[1:3]))
    Condition
      Error in `posPredValue.default()`:
      ! input data must have the same two levels

# negPredValue errors on bad input

    Code
      negPredValue(as.character(sens_pred), sens_truth)
    Condition
      Error in `negPredValue.default()`:
      ! input data must be a factor

---

    Code
      negPredValue(factor(letters[1:3]), factor(letters[1:3]))
    Condition
      Error in `negPredValue.default()`:
      ! input data must have the same two levels

