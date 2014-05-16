modelInfo <- list(label = "Penalized Multinomial Regression",
                  library = "nnet",
                  loop = NULL,
                  type = c("Classification"),
                  parameters = data.frame(parameter = c('decay'),
                                          class = c("numeric"),
                                          label = c('Weight Decay')),
                  grid = function(x, y, len = NULL) expand.grid(decay = c(0, 10 ^ seq(-1, -4, length = len - 1))),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- x
                    dat$.outcome <- y
                    if(!is.null(wts))
                    {
                      out <- multinom(.outcome ~ .,
                                      data = dat,
                                      weights = wts,                                       
                                      decay = param$decay,
                                      ...)
                    } else out <- multinom(.outcome ~ .,
                                           data = dat,
                                           decay = param$decay,
                                           ...)
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL)
                    predict(modelFit, newdata, type="class"),
                  prob = function(modelFit, newdata, submodels = NULL){
                    out <- predict(modelFit, newdata, type = "probs")
                    if(ncol(as.data.frame(out)) == 1)
                    {
                      out <- cbind(out, 1-out)
                      colnames(out) <-  rev(modelFit$obsLevels)
                    }
                    out
                  },
                  predictors = function(x, ...) if(hasTerms(x)) predictors(x$terms) else NA,
                  varImp = function(object, ...) {
                    out <- abs(coef(object))
                    if(is.vector(out))
                    {
                      out <- data.frame(Overall = out)
                      rownames(out) <- names(coef(object))
                    } else {
                      out <- as.data.frame(apply(out, 2, sum))
                      names(out)[1] <- "Overall"
                    }
                    subset(out, rownames(out) != "(Intercept)")
                  },
                  tags = c("Neural Network", "L2 Regularization", "Logistic Regression", "Linear Classifier"),
                  sort = function(x) x[order(-x[,1]),])
