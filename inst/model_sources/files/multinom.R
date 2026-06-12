modelInfo <- list(label = "Penalized Multinomial Regression",
                  library = "nnet",
                  loop = NULL,
                  type = c("Classification"),
                  parameters = data.frame(parameter = c('decay'),
                                          class = c("numeric"),
                                          label = c('Weight Decay')),
                  grid = function(x, y, len = NULL, search = "grid"){
                    if(search == "grid") {
                      out <- expand.grid(decay = c(0, 10 ^ seq(-1, -4, length = len - 1)))
                    } else {
                      out <- data.frame(decay = 10^runif(len, min = -5, 1))
                    }
                    out
                  }, 
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
                    dat$.outcome <- y
                    if(!is.null(wts)) {
                      out <- nnet::multinom(.outcome ~ .,
                                            data = dat,
                                            weights = wts,                      
                                            decay = param$decay,
                                            ...)
                    } else out <- nnet::multinom(.outcome ~ .,
                                                 data = dat,
                                                 decay = param$decay,
                                                 ...)
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL)
                    predict(modelFit, newdata, type="class"),
                  prob = function(modelFit, newdata, submodels = NULL){
                    out <- predict(modelFit, newdata, type = "probs")
                    if(nrow(newdata) == 1) {
                      out <- as.data.frame(t(out), stringsAsFactors = TRUE)
                    }
                    if(length(modelFit$obsLevels) == 2) {
                      out <- cbind(1 - out, out)
                      colnames(out) <-  modelFit$obsLevels
                    }
                    out
                  },
                  predictors = function(x, ...) if(hasTerms(x)) predictors(x$terms) else NA,
                  varImp = function(object, ...) {
                    out <- abs(coef(object))
                    if(is.vector(out)) {
                      out <- data.frame(Overall = out)
                      rownames(out) <- names(coef(object))
                    } else {
                      out <- as.data.frame(apply(out, 2, sum), stringsAsFactors = TRUE)
                      names(out)[1] <- "Overall"
                    }
                    subset(out, rownames(out) != "(Intercept)")
                  },
                  levels = function(x) x$obsLevels,
                  tags = c("Neural Network", "L2 Regularization", 
                           "Logistic Regression", "Linear Classifier", 
                           "Accepts Case Weights"),
                  sort = function(x) x[order(-x[,1]),])
