# Shared fixtures for test_tibble.R

# tibble isn't in depends or imports but recipes is. Here is a helper function
# to convert a data frame to tibble in the tests without having to worry about
# tibble being installed or laoded

to_tibble <- function(dat) {
  recipes::recipe(dat) |>
    recipes::prep() |>
    recipes::bake(new_data = NULL)
}


set.seed(8801)
tibble_dat <- to_tibble(twoClassSim(100))
tibble_df <- data.frame(
  a = tibble_dat[, 5],
  y = tibble_dat[["Class"]],
  stringsAsFactors = TRUE
) |>
  to_tibble()

tibble_rec <- recipes::recipe(y ~ ., data = tibble_df)
tibble_ctrl <- trainControl(
  method = "repeatedcv",
  repeats = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)
