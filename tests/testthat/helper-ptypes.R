# Shared fixtures for test_ptypes.R

# mtcars-based prototypes
mtcars_x <- mtcars[, -1]
mtcars_0 <- mtcars[0, -1]
wt_0 <- mtcars[0, 6, drop = FALSE]

# Sacramento housing data split with ~900 rows in training (reused across tests)
utils::data("Sacramento", package = "caret", envir = environment())
set.seed(8244)
sac_in <- sort(sample(seq_len(nrow(Sacramento)), 900))
sac_train <- Sacramento[sac_in, ]
sac_test <- Sacramento[-sac_in, ]
sac_x_train <- sac_train[, -7]
sac_x_test <- sac_test[, -7]
sac_y_train <- sac_train$price
sac_y_test <- sac_test$price
sac_0 <- sac_x_train[0, ]
