"varImp" <-
function(object, ...){
   UseMethod("varImp")
}

GarsonWeights <- function(object)
  {
    beta <- coef(object)
    abeta <- abs(beta)
    nms <- names(beta)
    i2h <- array(NA, dim = object$n[2:1])
    h2o <- array(NA, dim = object$n[2:3])

    for (hidden in 1:object$n[2]) {
      for (input in 1:object$n[1]) {
        label <- paste("i", input, "->h", hidden,"$", sep = "")
        i2h[hidden, input] <- abeta[grep(label, nms, fixed = FALSE)]
      }
    }
    for(hidden in 1:object$n[2]){
        for(output in 1:object$n[3]){
            label <- paste("h", hidden, "->o",
                           ifelse(object$n[3] == 1, "", output),
                           sep = "")
            h2o[hidden,output] <- abeta[grep(label, nms, fixed = TRUE)]
          }
      }    

    if(FALSE)
      {
        ## Test case from Gevrey, M., Dimopoulos, I., & Lek,
        ## S. (2003). Review and comparison of methods to study the
        ## contribution of variables in artificial neural network
        ## models. ecological modelling, 160(3), 249–264.
        i2h <- matrix(c(-1.67624,  3.29022,  1.32466, 
                        -0.51874, -0.22921, -0.25526,
                        -4.01764,  2.12486, -0.08168,
                        -1.75691, -1.44702,  0.58286),
                      ncol = 3, byrow = TRUE)
        h2o <- matrix(c(4.57857, -0.48815, -5.73901, -2.65221),
                      ncol = 1)
      }

    ##  From Gevrey et al. (2003): "For each hidden neuron i, multiply
    ##  the absolute value of the hidden-output layer connection
    ##  weight by the absolute value of the hidden-input layer
    ##  connection weight. Do this for each input variable j. The
    ##  following products Pij are obtained"


    ## We'll do this one response at a time. Gevrey et al. (2003) do
    ## not discuss multiple outputs, but the results are the same (at
    ## least in the case of classification).

    imp <- matrix(NA, nrow = object$n[1], ncol = object$n[3])
    

    for(output in 1:object$n[3])
      {
        Pij <- i2h * NA
        for(hidden in 1:object$n[2]) Pij[hidden,] <- i2h[hidden,] * h2o[hidden,output]

        ## "For each hidden neuron, divide Pij by the sum for all the
        ## input variables to obtain Qij. For example for Hidden 1, Q11 =
        ## P11/(P11+P12+P13).
        
        Qij <- Pij * NA
        for(hidden in 1:object$n[2]) Qij[hidden,] <- Pij[hidden,] / sum(Pij[hidden,])
        

        ## "For each input neuron, sum the product Sj formed from the
        ## previous computations of Qij. For example, S1 =
        ## Q11+Q21+Q31+Q41."

        Sj <- apply(Qij, 2, sum)

        ## "Divide Sj by the sum for all the input variables. Expressed as
        ## a percentage, this gives the relative importance or
        ## distribution of all output weights attributable to the given
        ## input variable. For example, for the input neuron 1, the
        ## relative importance is equal to (S1/100)/(S1+S2+S3)"

        imp[,output] <- Sj/sum(Sj)*100
        rm(Pij, Qij, Sj)
      }
    
    colnames(imp) <- if(!is.null(colnames(object$residuals))) colnames(object$residuals) else paste("Y", 1:object$n[3], sep = "")
    rownames(imp) <- if(!is.null(object$coefnames)) object$coefnames else  paste("X", 1:object$n[1], sep = "")
    imp
  }

varImp.bagEarth <- function(object, ...){
  code <- getModelInfo("bagEarth", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library))
    do.call("require", list(package = code$library[i]))
  code$varImp(object, ...)
}

varImp.bagFDA <- function(object, ...){
  code <- getModelInfo("bagFDA", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library))
    do.call("require", list(package = code$library[i]))
  code$varImp(object, ...)
}

varImp.C5.0 <- function(object, ...){
  code <- getModelInfo("C5.0", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library))
    do.call("require", list(package = code$library[i]))
  code$varImp(object, ...)
}

varImp.cubist <- function(object, weights = c(0.5, 0.5), ...){
  code <- getModelInfo("cubist", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library))
    do.call("require", list(package = code$library[i]))
  code$varImp(object, weights = weights, ...)
}

varImp.dsa <- function(object, cuts = NULL, ...){
  code <- getModelInfo("partDSA", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library))
    do.call("require", list(package = code$library[i]))
  code$varImp(object, cuts = cuts, ...)
}

varImp.glm <- function(object, ...){
  code <- getModelInfo("glm", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library))
    do.call("require", list(package = code$library[i]))
  code$varImp(object, ...)
}

varImp.glmnet <- function(object, lambda = NULL, ...){
  code <- getModelInfo("glmnet", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library))
    do.call("require", list(package = code$library[i]))
  code$varImp(object, lambda = lambda, ...)
}

varImp.JRip <- function(object, ...){
  code <- getModelInfo("JRip", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library))
    do.call("require", list(package = code$library[i]))
  code$varImp(object, ...)
}

varImp.multinom <- function(object, ...){
  code <- getModelInfo("multinom", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library))
    do.call("require", list(package = code$library[i]))
  code$varImp(object, ...)
}

varImp.nnet <- function(object, ...){
  code <- getModelInfo("nnet", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library))
    do.call("require", list(package = code$library[i]))
  code$varImp(object, ...)
}

varImp.PART <- function(object, ...){
  code <- getModelInfo("PART", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library))
    do.call("require", list(package = code$library[i]))
  code$varImp(object, ...)
}

varImp.RRF <- function(object, ...){
  code <- getModelInfo("RRF", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library))
    do.call("require", list(package = code$library[i]))
  code$varImp(object, ...)
}

varImp.rpart <- function(object, surrogates = FALSE, competes = TRUE, ...){
  code <- getModelInfo("rpart", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library))
    do.call("require", list(package = code$library[i]))
  code$varImp(object, surrogates = surrogates, competes = competes, ...)
}

varImp.randomForest <- function(object, ...){
  code <- getModelInfo("rf", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library))
    do.call("require", list(package = code$library[i]))
  code$varImp(object, ...)
}

varImp.gbm <- function(object, numTrees = NULL, ...){
  code <- getModelInfo("gbm", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library))
    do.call("require", list(package = code$library[i]))
  code$varImp(object, numTrees = numTrees, ...)
}

varImp.classbagg <- function(object, ...){
  code <- getModelInfo("treebag", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library))
    do.call("require", list(package = code$library[i]))
  code$varImp(object, ...)
}

varImp.regbagg <- function(object, ...){
  code <- getModelInfo("treebag", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library))
    do.call("require", list(package = code$library[i]))
  code$varImp(object, ...)
}

varImp.pamrtrained <- function(object, threshold, data, ...){
  code <- getModelInfo("pam", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library))
    do.call("require", list(package = code$library[i]))
  code$varImp(object, 
              threshold = object$bestTune$threshold, 
              data = object$finalModel$xData, 
              ...)
}

varImp.lm <- function(object, ...){
  code <- getModelInfo("lm", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library))
    do.call("require", list(package = code$library[i]))
  code$varImp(object, ...)
}

varImp.mvr <- function(object, estimate = NULL, ...){
  code <- getModelInfo("pls", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library))
    do.call("require", list(package = code$library[i]))
  code$varImp(object, estimate = estimate, ...)
}

varImp.earth <- function(object, value = "gcv", ...){
  code <- getModelInfo("earth", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library))
    do.call("require", list(package = code$library[i]))
  code$varImp(object, value = value, ...)
}

varImp.RandomForest <- function(object, ...){
  code <- getModelInfo("cforest", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library))
    do.call("require", list(package = code$library[i]))
  code$varImp(object, ...)
}

varImp.plsda <- function(object, ...){
  code <- getModelInfo("pls", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library))
    do.call("require", list(package = code$library[i]))
  code$varImp(object, ...)
}

varImp.fda <- function(object, value = "gcv", ...){
  code <- getModelInfo("fda", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library))
    do.call("require", list(package = code$library[i]))
  code$varImp(object, value = value, ...)
}

varImp.gam <- function(object, ...){
  mod <- if(any(names(object) == "optimizer")) "gam" else "gamLoess"
  code <- getModelInfo(mod, regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library))
    do.call("require", list(package = code$library[i]))
  code$varImp(object, ...)
}
