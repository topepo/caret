# predict.plsda needs a number of components

    Code
      predict(fit, iris[, 1:4])
    Condition
      Error in `predict.plsda()`:
      ! specify ncomp

# print.plsda describes the model

    Code
      print(fit)
    Output
      Partial least squares classification, fitted with the kernel algorithm.
      The softmax function was used to compute class probabilities.

# plsda takes an indicator matrix and validates the outcome

    Code
      plsda(x, as.numeric(iris$Species), ncomp = 2)
    Condition
      Error in `plsda.default()`:
      ! y must be a matrix or a factor

---

    Code
      plsda(x, unnamed, ncomp = 2)
    Condition
      Error in `plsda.default()`:
      ! the y matrix must have column names

---

    Code
      plsda(x, not_one, ncomp = 2)
    Condition
      Error in `plsda.default()`:
      ! the rows of y must be 0/1 and sum to 1

# plsda warns about several ncomp values and unused priors

    A value single ncomp must be specified. max(ncomp) was used. Predictions can be obtained for values <= ncomp

---

    Priors are ignored unless probMethod = "Bayes"

# print.plsda names the fitting algorithm and the probability rule

    Code
      print(fit)
    Output
      Partial least squares classification, fitted with the kernel algorithm.
      The softmax function was used to compute class probabilities.

---

    Code
      print(fit)
    Output
      Partial least squares classification, fitted with the simpls algorithm.
      The softmax function was used to compute class probabilities.

---

    Code
      print(fit)
    Output
      Partial least squares classification, fitted with the orthogonal scores algorithm.
      The softmax function was used to compute class probabilities.

---

    Code
      print(pcr_like)
    Output
      Principal component classification, fitted with the singular value decomposition algorithm.
      The softmax function was used to compute class probabilities.

---

    Code
      print(bogus)
    Condition
      Error in `print.plsda()`:
      ! Unknown fit method.

# print.plsda reports cross-validation and the Bayes rule

    Code
      print(cv_fit)
    Output
      Partial least squares classification, fitted with the kernel algorithm.
      Cross-validated using 10 random segments.
      The softmax function was used to compute class probabilities.

---

    Code
      print(bayes)
    Output
      Partial least squares classification, fitted with the kernel algorithm.
      Bayes rule was used to compute class probabilities.

