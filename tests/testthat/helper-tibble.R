# Shared fixtures for test_tibble.R

set.seed(8801)
tibble_dat <- twoClassSim(100)
tibble_df <- data.frame(
  a = tibble_dat[, 5],
  y = tibble_dat[["Class"]],
  stringsAsFactors = TRUE
)
tibble_rec <- recipes::recipe(y ~ ., data = tibble_df)
tibble_ctrl <- trainControl(
  method = "repeatedcv",
  repeats = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)
