library(caret)

set.seed(1)
reg_tr <- SLC14_1(200)[, 17:21]
reg_tr[, -ncol(reg_tr)] <- abs(reg_tr[, -ncol(reg_tr)])
reg_te <- SLC14_1(200)[, 17:21]
reg_te[, -ncol(reg_te)] <- abs(reg_te[, -ncol(reg_te)])

###################################################################
##

set.seed(2)
center_scale_old <- train(y ~ ., data = reg_tr,
                          method = "lm",
                          preProc = c("center", "scale"),
                          trControl = trainControl(method = "cv"))

center_scale_pred_old <- predict(center_scale_old, reg_te)

###################################################################
##

set.seed(2)
range_old <- train(y ~ ., data = reg_tr,
                   method = "lm",
                   preProc = c("range"),
                   trControl = trainControl(method = "cv"))

range_pred_old <- predict(range_old, reg_te)

###################################################################
##

set.seed(2)
bc_old <- train(y ~ ., data = reg_tr,
                method = "lm",
                preProc = c("BoxCox"),
                trControl = trainControl(method = "cv"))

bc_pred_old <- predict(bc_old, reg_te)

###################################################################
##

set.seed(2)
yj_old <- train(y ~ ., data = reg_tr,
                method = "lm",
                preProc = c("YeoJohnson"),
                trControl = trainControl(method = "cv"))

yj_pred_old <- predict(yj_old, reg_te)

###################################################################
##

set.seed(2)
et_old <- train(y ~ ., data = reg_tr,
                method = "lm",
                preProc = c("expoTrans"),
                trControl = trainControl(method = "cv"))

et_pred_old <- predict(et_old, reg_te)

###################################################################
##

set.seed(2)
knni_old <- train(y ~ ., data = reg_tr,
                  method = "lm",
                  preProc = c("knnImpute"),
                  trControl = trainControl(method = "cv"))

knni_pred_old <- predict(knni_old, reg_te)

###################################################################
##

set.seed(2)
bagi_old <- train(y ~ ., data = reg_tr,
                  method = "lm",
                  preProc = c("bagImpute"),
                  trControl = trainControl(method = "cv"))

bagi_pred_old <- predict(bagi_old, reg_te)

###################################################################
##

set.seed(2)
medi_old <- train(y ~ ., data = reg_tr,
                  method = "lm",
                  preProc = c("medianImpute"),
                  trControl = trainControl(method = "cv"))

medi_pred_old <- predict(medi_old, reg_te)

###################################################################
##

set.seed(2)
pca_old <- train(y ~ ., data = reg_tr,
                 method = "lm",
                 preProc = c("pca"),
                 trControl = trainControl(method = "cv"))

pca_pred_old <- predict(pca_old, reg_te)

###################################################################
##

set.seed(2)
ica_old <- train(y ~ ., data = reg_tr,
                 method = "lm",
                 preProc = c("ica"),
                 trControl = trainControl(method = "cv"))

ica_pred_old <- predict(ica_old, reg_te)

###################################################################
##

set.seed(2)
ss_old <- train(y ~ ., data = reg_tr,
                method = "lm",
                preProc = c("spatialSign"),
                trControl = trainControl(method = "cv"))

ss_pred_old <- predict(ss_old, reg_te)

###################################################################
##

info_old <- sessionInfo()

save(list = grep("_old$", ls(), value = TRUE), 
     file = "expected_preprocess.RData")

