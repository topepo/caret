# Small data sets shared by the train()/workflow tests. They are deliberately
# tiny: the point is to reach every branch of the resampling machinery, not to
# fit anything well.

# A two-class problem with valid R names for its levels, so classProbs works.
engine_two_class <- function(n = 60) {
  withr::with_seed(3517, twoClassSim(n, noiseVars = 2))
}

# A three-class problem, for the paths that behave differently with more than
# two classes.
engine_three_class <- function() {
  iris[c(1:15, 51:65, 101:115), ]
}

# A regression problem, small enough that lm() is not rank deficient in a
# three-fold split.
engine_regression <- function(n = 60) {
  withr::with_seed(8240, {
    data.frame(
      x1 = rnorm(n),
      x2 = rnorm(n),
      x3 = rnorm(n),
      y = rnorm(n)
    )
  })
}
