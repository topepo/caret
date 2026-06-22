# Confusion matrix works

    Levels are not in the same order for reference and data. Refactoring data to match.

---

    Levels are not in the same order for reference and data. Refactoring data to match.

---

    Code
      confusionMatrix(dat5, ref2)
    Condition
      Error in `confusionMatrix.default()`:
      ! The data contain levels not found in the data.

---

    Code
      confusionMatrix(dat5, ref)
    Condition
      Error in `confusionMatrix.default()`:
      ! The data contain levels not found in the data.

