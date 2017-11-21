library(caret)
library(plyr)
library(recipes)
library(dplyr)

set.seed(1)
reg_tr <- SLC14_1(200)[, 17:21]
reg_tr[, -ncol(reg_tr)] <- abs(reg_tr[, -ncol(reg_tr)])
reg_te <- SLC14_1(200)[, 17:21]
reg_te[, -ncol(reg_te)] <- abs(reg_te[, -ncol(reg_te)])

###################################################################
##

set.seed(2)
center_scale_current <- train(y ~ ., data = reg_tr,
                              method = "lm",
                              preProc = c("center", "scale"),
                              trControl = trainControl(method = "cv"))

center_scale_pred_current <- predict(center_scale_current, reg_te)

###################################################################
##

set.seed(2)
range_current <- train(y ~ ., data = reg_tr,
                       method = "lm",
                       preProc = c("range"),
                       trControl = trainControl(method = "cv"))

range_pred_current <- predict(range_current, reg_te)

###################################################################
##

set.seed(2)
bc_current <- train(y ~ ., data = reg_tr,
                    method = "lm",
                    preProc = c("BoxCox"),
                    trControl = trainControl(method = "cv"))

bc_pred_current <- predict(bc_current, reg_te)

###################################################################
##

set.seed(2)
yj_current <- train(y ~ ., data = reg_tr,
                    method = "lm",
                    preProc = c("YeoJohnson"),
                    trControl = trainControl(method = "cv"))

yj_pred_current <- predict(yj_current, reg_te)

###################################################################
##

set.seed(2)
et_current <- train(y ~ ., data = reg_tr,
                    method = "lm",
                    preProc = c("expoTrans"),
                    trControl = trainControl(method = "cv"))

et_pred_current <- predict(et_current, reg_te)

###################################################################
##

set.seed(2)
knni_current <- train(y ~ ., data = reg_tr,
                      method = "lm",
                      preProc = c("knnImpute"),
                      trControl = trainControl(method = "cv"))

knni_pred_current <- predict(knni_current, reg_te)

###################################################################
##

set.seed(2)
bagi_current <- train(y ~ ., data = reg_tr,
                      method = "lm",
                      preProc = c("bagImpute"),
                      trControl = trainControl(method = "cv"))

bagi_pred_current <- predict(bagi_current, reg_te)

###################################################################
##

set.seed(2)
medi_current <- train(y ~ ., data = reg_tr,
                      method = "lm",
                      preProc = c("medianImpute"),
                      trControl = trainControl(method = "cv"))

medi_pred_current <- predict(medi_current, reg_te)

###################################################################
##

set.seed(2)
pca_current <- train(y ~ ., data = reg_tr,
                     method = "lm",
                     preProc = c("pca"),
                     trControl = trainControl(method = "cv"))

pca_pred_current <- predict(pca_current, reg_te)

###################################################################
##

set.seed(2)
ica_current <- train(y ~ ., data = reg_tr,
                     method = "lm",
                     preProc = c("ica"),
                     trControl = trainControl(method = "cv"))

ica_pred_current <- predict(ica_current, reg_te)

###################################################################
##

set.seed(2)
ss_current <- train(y ~ ., data = reg_tr,
                    method = "lm",
                    preProc = c("spatialSign"),
                    trControl = trainControl(method = "cv"))

ss_pred_current <- predict(ss_current, reg_te)

###################################################################
##

info_current <- sessionInfo()

###################################################################
##

load("expected_preprocess.RData")

pred_obj <- ls(pattern = "_pred_current$")

for(i in pred_obj) {
 cat("  testing", gsub("_pred_current$", "", i), " ...") 
  tmp <- all.equal(get(i), get(gsub("_current", "_old", i)))
  if(isTRUE(tmp)) {
    cat("passed\n")
  } else {
    cat("failed:") 
    print(summary(get(i) - get(gsub("_current", "_old", i))))
    cat("\n")
    stop()
  }
}

if(!interactive())
   q("no")

