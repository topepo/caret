# Fixtures for test-varImp.R.

# A minimal stand-in for an FCNN4R multilayer-perceptron fit, used to exercise
# GarsonWeights_FCNN4R() without the (archived) FCNN4R package. The real
# object stores the network in an S4 `net` slot with weight values, active-
# weight flags and layer sizes.
setClass(
  "fake_fcnn_net",
  representation(
    m_w_values = "numeric",
    m_w_flags = "integer",
    m_layers = "integer"
  )
)

# A 2-input, 2-hidden, 1-output network: (2 + 1) * 2 + (2 + 1) * 1 = 9 weights
# (the leading weight of each neuron is its bias, which GarsonWeights_FCNN4R
# drops). All weights are active.
make_fcnn_fit <- function() {
  list(
    net = new(
      "fake_fcnn_net",
      m_w_values = c(0.5, 1, 2, -0.5, 3, -4, 0.25, 5, -6),
      m_w_flags = rep(1L, 9),
      m_layers = c(2L, 2L, 1L)
    )
  )
}

# Stand-ins for glmnet fits, so the glmnet importance code can be tested
# without the package. The registry code only calls
# `predict(object, s = , type = "coef")`, so a registered predict method is
# enough; the classes are test-only, so they cannot clash with real glmnet
# methods. Coefficients are deliberately negative - the importances are their
# absolute values, and the intercept is dropped.
predict.fake_glmnet <- function(object, s = NULL, type = "coef", ...) {
  matrix(
    c(2, -3, 1.5, -0.5),
    ncol = 1,
    dimnames = list(c("(Intercept)", "a", "b", "c"), "s1")
  )
}
registerS3method("predict", "fake_glmnet", predict.fake_glmnet)

# A multi-response fit returns one coefficient matrix per class
predict.fake_glmnet_multi <- function(object, s = NULL, type = "coef", ...) {
  one <- function(v) {
    matrix(v, ncol = 1, dimnames = list(c("(Intercept)", "a", "b"), "s1"))
  }
  list(first = one(c(1, -2, 0.5)), second = one(c(2, -4, 1)))
}
registerS3method("predict", "fake_glmnet_multi", predict.fake_glmnet_multi)

# A registry stub for varImp dispatchers whose packages are burdensome
# (glmnet is heavy, RWeka needs Java, partDSA and FCNN4R are effectively
# archived). It records the arguments the dispatcher forwards so the test can
# assert on them.
recording_registry <- function() {
  list(
    library = NULL,
    varImp = function(object, ...) {
      c(list(object = object), list(...))
    }
  )
}
