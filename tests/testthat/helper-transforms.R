# Test data shared by test-BoxCoxTrans.R and test-expoTrans.R.
#
# Right-skewed (lognormal) positive data, the kind a power transform is meant to
# symmetrise. A fixed seed makes the estimated lambdas deterministic.
set.seed(1)
skew_y <- rlnorm(120)
