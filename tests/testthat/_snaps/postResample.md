# twoClassSummary validates its levels and tolerates flat ROC input

    Code
      twoClassSummary(d, lev = c("A", "B"))
    Condition
      Error in `twoClassSummary()`:
      ! levels of observed and predicted data do not match

# mnLogLoss validates its inputs

    Code
      mnLogLoss(d, lev = NULL)
    Condition
      Error in `mnLogLoss()`:
      ! 'lev' cannot be NULL

---

    Code
      mnLogLoss(d, lev = c("A", "C"))
    Condition
      Error in `mnLogLoss()`:
      ! 'data' should have columns consistent with 'lev'

---

    Code
      mnLogLoss(d2, lev = c("A", "B"))
    Condition
      Error in `mnLogLoss()`:
      ! 'data$obs' should have levels consistent with 'lev'

# multiClassSummary validates levels and works without probabilities

    Code
      multiClassSummary(d, lev = c("A", "B"))
    Condition
      Error in `multiClassSummary()`:
      ! levels of observed and predicted data do not match

