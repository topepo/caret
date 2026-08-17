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

# thresholder requires threshold values and saved logical flag

    Code
      thresholder(fit, threshold = NULL)
    Condition
      Error:
      ! Please supply probability threshold values.

---

    Code
      thresholder(logical_flag, threshold = 0.5)
    Condition
      Error in `thresholder()`:
      ! `savePredictions` should be TRUE, 'all', or 'final'

# thresholder validates the statistics argument

    Code
      thresholder(fit, threshold = 0.5, statistics = "bogus")
    Condition
      Error in `thresholder()`:
      ! `statistics` should be either 'all', or one or more of 'Sensitivity', 'Specificity', 'Pos Pred Value', 'Neg Pred Value', 'Precision', 'Recall', 'F1', 'Prevalence', 'Detection Rate', 'Detection Prevalence', 'Balanced Accuracy', 'Accuracy', 'Kappa', 'J', 'Dist'.

---

    Code
      thresholder(fit, threshold = 0.5, statistics = c("all", "J"))
    Condition
      Error in `thresholder()`:
      ! `statistics` should be either 'all', or one or more of 'Sensitivity', 'Specificity', 'Pos Pred Value', 'Neg Pred Value', 'Precision', 'Recall', 'F1', 'Prevalence', 'Detection Rate', 'Detection Prevalence', 'Balanced Accuracy', 'Accuracy', 'Kappa', 'J', 'Dist'.

# summ_stats warns about and removes missing values

    The following columns have missing values (NA), which have been removed: 'a'.
    

