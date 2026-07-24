# Test data shared by test-lift.R.
#
# Ten samples, balanced two-class outcome ("yes" is the event). prob1 ranks the
# cases perfectly (all events sit above all non-events), while prob2 ranks them
# poorly. This lets the lift/gain numbers be worked out by hand.
lift_data <- data.frame(
  Class = factor(rep(c("yes", "no"), each = 5), levels = c("yes", "no")),
  prob1 = c(0.90, 0.80, 0.70, 0.60, 0.55, 0.50, 0.40, 0.30, 0.20, 0.10),
  prob2 = c(0.40, 0.30, 0.60, 0.20, 0.50, 0.55, 0.70, 0.10, 0.90, 0.80)
)
