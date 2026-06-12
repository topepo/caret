suggestions <- function(model)
{

  out <- c("center" = FALSE, "scale" = FALSE,
           "nzv" = FALSE, "corr" = FALSE)

  if(model %in% c("enet", "gaussprLinear", "gaussprPoly",
                  "gaussprRadial", "glmnet", "gpls", "knn",
                  "lars", "lars2", "lasso", "lssvmLinear",
                  "lssvmPoly", "lssvmRadial", "multinom",
                  "neuralnet", "nnet", "penalized", "pls",
                  "relaxo", "rocc", "rvmLinear", "rvmPoly",
                  "rvmRadial", "smda", "sparseLDA", "spls",
                  "superpc", "svmLinear", "svmPoly", "svmRadial")) out["center"] <- TRUE
  if(model %in% c("gaussprLinear", "gaussprPoly",
                  "gaussprRadial", "glmnet", "gpls", "knn",
                  "lars", "lars2", "lasso", "lssvmLinear",
                  "lssvmPoly", "lssvmRadial", "multinom",
                  "neuralnet", "nnet", "penalized", "pls",
                  "relaxo", "rocc", "rvmLinear", "rvmPoly",
                  "rvmRadial", "smda", "sparseLDA", "spls",
                  "superpc", "svmLinear", "svmPoly", "svmRadial")) out["scale"] <- TRUE
  if(model %in% c("enet", "gaussprLinear", "gaussprPoly",
                  "gaussprRadial", "gpls", "hda", "hdda", "icr",
                  "knn", "lars", "lars2", "lasso", "lda", "knn",
                  "lm", "lmStepAIC", "glm", "glmStepAIC",
                  "lssvmLinear", "lssvmPoly", "lssvmRadial",
                  "lvq", "mda", "multinom", "nb",
                  "neuralnet", "nnet", "pam", "pcaNNet",  "pcr",
                  "pda", "pda2", "penalized", "pls", "qda",
                  "QdaCov", "rda", "relaxo", "rlm", "rocc",
                  "rvmLinear", "rvmPoly", "rvmRadial",
                  "scrda", "sda", "sddaLDA", "sddaQDA",      
                  "slda", "smda", "sparseLDA", "spls",         
                  "stepLDA", "stepQDA", "superpc", "svmLinear",  
                  "svmPoly", "svmRadial", "vbmpRadial")) out["nzv"] <- TRUE

  if(model %in% c("enet", "lars", "lars2", "lasso", "lda",
                  "lm", "lmStepAIC", "glm", "glmStepAIC",
                  "multinom", "nb", "neuralnet", "nnet",
                  "pda", "pda2", "penalized", "relaxo", "rlm" )) out["nzv"] <- TRUE
  out
}
