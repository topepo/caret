# thresholder validates its inputs

    Code
      thresholder(list(), threshold = 0.5)
    Condition
      Error:
      ! `x` should be an object of class 'train'

---

    Code
      thresholder(fit, threshold = 1.5)
    Condition
      Error:
      ! `threshold` should be on [0,1]

---

    Code
      thresholder(no_probs, threshold = 0.5)
    Condition
      Error:
      ! `classProbs` must be TRUE in `trainControl`

---

    Code
      thresholder(no_save, threshold = 0.5)
    Condition
      Error in `thresholder()`:
      ! `savePredictions` should be TRUE, 'all', or 'final'

# thresholder only supports two-class problems

    Code
      thresholder(fit, threshold = 0.5)
    Condition
      Error in `thresholder()`:
      ! For two class problems only

