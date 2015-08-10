library(caret)
timestamp <- format(Sys.time(), "%Y_%m_%d_%H_%M")

model <- "preProcess"

###################################################################
##

set.seed(1234)
training <- twoClassSim(100, linearVars = 2)
for(i in 1:ncol(training)) {
  if(is.numeric(training[,i])) training[,i] <- abs(training[,i])
}
testing  <- twoClassSim(100, linearVars = 2)
for(i in 1:ncol(testing)) {
  if(is.numeric(testing[,i])) testing[,i] <- abs(testing[,i])
}
testing2 <- testing
testing2[sample(1:nrow(testing), 10), 4] <- NA
testing2[sample(1:nrow(testing), 10), 2] <- NA

###################################################################
##

set.seed(657)
c_s <- preProcess(training[, -ncol(training)], method = c("center", "scale"))
set.seed(657)
c_s_p <- preProcess(training[, -ncol(training)], method = c("center", "scale", "pca"))
set.seed(657)
c_s_i <- preProcess(training[, -ncol(training)], method = c("center", "scale", "ica"), n.comp = 3)
set.seed(657)
c_s_ss <- preProcess(training[, -ncol(training)], method = c("center", "scale", "spatialSign"))
set.seed(657)
c_s_ki <- preProcess(training[, -ncol(training)], method = c("center", "scale", "knnImpute"))
set.seed(657)
c_s_bi <- preProcess(training[, -ncol(training)], method = c("center", "scale", "bagImpute"))
set.seed(657)
c_s_mi <- preProcess(training[, -ncol(training)], method = c("center", "scale", "medianImpute"))
set.seed(657)
c_s_y <- preProcess(training[, -ncol(training)], method = c("center", "scale", "YeoJohnson"))
set.seed(657)
c_s_bc <- preProcess(training[, -ncol(training)], method = "BoxCox")
set.seed(657)
c_s_e <- preProcess(training[, -ncol(training)], method = "expoTrans")
set.seed(657)
c_s_y_p_s <- preProcess(training[, -ncol(training)], method = c("center", "scale", "YeoJohnson", "pca", "spatialSign"))
set.seed(657)
r_p <- preProcess(training[, -ncol(training)], method = c("range", "pca"))

###################################################################
##

set.seed(657)
test_c_s <- predict(c_s, newdata = testing[, -ncol(testing)])
set.seed(657)
test_c_s_p <- predict(c_s_p, newdata = testing[, -ncol(testing)])
set.seed(657)
test_c_s_i <- predict(c_s_i, newdata = testing[, -ncol(testing)])
set.seed(657)
test_c_s_ss <- predict(c_s_ss, newdata = testing[, -ncol(testing)])
set.seed(657)
test_c_s_ki <- predict(c_s_ki, newdata = testing2[, -ncol(testing)])
set.seed(657)
test_c_s_bi <- predict(c_s_bi, newdata = testing2[, -ncol(testing)])
set.seed(657)
test_c_s_mi <- predict(c_s_mi, newdata = testing2[, -ncol(testing)])
set.seed(657)
test_c_s_y <- predict(c_s_y, newdata = testing[, -ncol(testing)])
set.seed(657)
test_c_s_bc <- predict(c_s_bc, newdata = testing[, -ncol(testing)])
set.seed(657)
test_c_s_e <- predict(c_s_e, newdata = testing[, -ncol(testing)])
set.seed(657)
test_c_s_y_p_s <- predict(c_s_y_p_s, newdata = testing[, -ncol(testing)])
set.seed(657)
test_r_p <- predict(r_p, newdata = testing[, -ncol(testing)])


#########################################################################

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()

save(list = c(tests, "sInfo", "timestamp", "training", "testing", "testing2"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

q("no")
