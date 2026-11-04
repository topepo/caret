# Test data shared by test-confusionMatrix.R.
#
# Deterministic confusion tables let the confusionMatrix() methods be tested
# without fitting a model. The train / rfe / sbf paths need real fitted objects,
# so those are exercised by the integration tests (which fit small models).

# A 2x2 table with positive class "yes":
#   sensitivity = 8 / (8 + 2) = 0.8, specificity = 9 / (9 + 1) = 0.9
cm_tab2 <- as.table(matrix(
  c(8, 2, 1, 9),
  nrow = 2,
  dimnames = list(Prediction = c("yes", "no"), Reference = c("yes", "no"))
))

# A 3x3 table for the multiclass path.
cm_tab3 <- as.table(matrix(
  c(5, 1, 1, 1, 4, 1, 0, 1, 6),
  nrow = 3,
  dimnames = list(Prediction = LETTERS[1:3], Reference = LETTERS[1:3])
))
