# Shared test data for test_mnLogLoss.R

mnll_classes <- LETTERS[1:3]

mnll_dat <- data.frame(
  obs = c("A", "A", "A", "B", "B", "C"),
  pred = c("A", "A", "A", "B", "B", "C"),
  A = c(1, .80, .51, .1, .2, .3),
  B = c(0, .05, .29, .8, .6, .3),
  C = c(0, .15, .20, .1, .2, .4),
  stringsAsFactors = TRUE
)

# Issue #637: two-class case
mnll_classes_b <- c("A", "B")

mnll_dat_b <- data.frame(
  obs = c("A", "A", "A", "B", "B"),
  pred = c("A", "A", "A", "B", "B"),
  A = c(1, .80, .51, .1, .2),
  B = c(0, .20, .49, .9, .8),
  stringsAsFactors = TRUE
)
