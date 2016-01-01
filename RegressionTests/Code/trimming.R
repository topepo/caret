library(caret)

model <- "trimming"

SLC14_1 <- function(n = 100) {
  dat <- matrix(rnorm(n*20, sd = 3), ncol = 20)
  foo <- function(x) x[1] + sin(x[2]) + log(abs(x[3])) + x[4]^2 + x[5]*x[6] + 
    ifelse(x[7]*x[8]*x[9] < 0, 1, 0) +
    ifelse(x[10] > 0, 1, 0) + x[11]*ifelse(x[11] > 0, 1, 0) + 
    sqrt(abs(x[12])) + cos(x[13]) + 2*x[14] + abs(x[15]) + 
    ifelse(x[16] < -1, 1, 0) + x[17]*ifelse(x[17] < -1, 1, 0) -
    2 * x[18] - x[19]*x[20]
  dat <- as.data.frame(dat)
  colnames(dat) <- paste0("Var", 1:ncol(dat))
  dat$y <- apply(dat[, 1:20], 1, foo) + rnorm(n, sd = 3)
  dat
}

set.seed(1)
training <- SLC14_1(30)
testing <- SLC14_1(100)

set.seed(1)
cv_no_trimming <- train(y ~ ., 
                        data = training,
                        method = "lm",
                        preProc = c("center", "scale"),
                        trControl = trainControl(method = "cv", 
                                                 savePredictions = TRUE))

set.seed(1)
cv_bottom_only <- train(y ~ ., 
                        data = training,
                        method = "lm",
                        preProc = c("center", "scale"),
                        trControl = trainControl(method = "cv",
                                                 predictionBounds = c(0, NA), 
                                                 savePredictions = TRUE))

set.seed(1)
cv_boolean <- train(y ~ ., 
                    data = training,
                    method = "lm",
                    preProc = c("center", "scale"),
                    trControl = trainControl(method = "cv",
                                             predictionBounds = c(TRUE, TRUE), 
                                             savePredictions = TRUE))

set.seed(1)
cv_top_only <- train(y ~ ., 
                data = training,
                method = "lm",
                preProc = c("center", "scale"),
                trControl = trainControl(method = "cv",
                                         predictionBounds = c(NA, 25), 
                                         savePredictions = TRUE))

set.seed(1)
loo_no_trimming <- train(y ~ ., 
                         data = training,
                         method = "lm",
                         preProc = c("center", "scale"),
                         trControl = trainControl(method = "LOOCV", 
                                                  savePredictions = TRUE))

set.seed(1)
loo_bottom_only <- train(y ~ ., 
                         data = training,
                         method = "lm",
                         preProc = c("center", "scale"),
                         trControl = trainControl(method = "LOOCV",
                                                  predictionBounds = c(0, NA), 
                                                  savePredictions = TRUE))

set.seed(1)
loo_boolean <- train(y ~ ., 
                     data = training,
                     method = "lm",
                     preProc = c("center", "scale"),
                     trControl = trainControl(method = "LOOCV",
                                              predictionBounds = c(TRUE, TRUE), 
                                              savePredictions = TRUE))

set.seed(1)
loo_top_only <- train(y ~ ., 
                      data = training,
                      method = "lm",
                      preProc = c("center", "scale"),
                      trControl = trainControl(method = "LOOCV",
                                               predictionBounds = c(NA, 25), 
                                               savePredictions = TRUE))

test_external_cv_no_trimming  <- predict(cv_no_trimming, testing)
test_external_cv_bottom_only  <- predict(cv_bottom_only, testing)
test_external_cv_booleans     <- predict(cv_boolean, testing)
test_external_cv_top_only     <- predict(cv_top_only, testing)
test_external_loo_no_trimming <- predict(loo_no_trimming, testing)
test_external_loo_bottom_only <- predict(loo_bottom_only, testing)
test_external_loo_booleans    <- predict(loo_boolean, testing)
test_external_loo_top_only    <- predict(loo_top_only, testing)

test_internal_cv_no_trimming  <- cv_no_trimming$pred
test_internal_cv_bottom_only  <- cv_bottom_only$pred
test_internal_cv_booleans     <- cv_boolean$pred
test_internal_cv_top_only     <- cv_top_only$pred
test_internal_loo_no_trimming <- loo_no_trimming$pred
test_internal_loo_bottom_only <- loo_bottom_only$pred
test_internal_loo_booleans    <- loo_boolean$pred
test_internal_loo_top_only    <- loo_top_only$pred

#########################################################################

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()

save(list = c(tests, "sInfo", "timestamp"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

q("no")

