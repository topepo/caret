modelInfo <- list(label = "glmnet",
                  library = "h2o",
                  type = c("Regression", "Classification"),
                  parameters = data.frame(parameter = c('alpha', 'lambda'),
                                          class = c("numeric", "numeric"),
                                          label = c('Mixing Percentage', 'Regularization Parameter')),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- data.frame(alpha = seq(0, 1, length = len),
                                        lambda = c(0, 10 ^ seq(-1, -4, length = len - 1)))
                    } else {
                      out <- data.frame(alpha = runif(len, min = 0, 1),
                                        lambda = 2^runif(len, min = -10, 3))
                    }
                    out
                  },
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- if(!is.data.frame(x)) as.data.frame(x) else x
                    dat$.outcome <- y
                    p <- ncol(dat)
                    frame_name <- paste0("tmp_train_dat_",sample.int(10000, 1))
                    tmp_train_dat = as.h2o(dat, destination_frame = frame_name)
                    out <- h2o.glm(x = colnames(x), y = ".outcome",
                                   training_frame = tmp_train_dat,
                                   family = if(is.factor(y)) "binomial" else "gaussian",
                                   alpha = param$alpha, lambda = param$lambda, ...)
                    h2o.getModel(out@model_id) 
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    frame_name <- paste0("new_dat_",sample.int(10000, 1))
                    newdata <- as.h2o(newdata, destination_frame = frame_name)
                    as.data.frame(predict(modelFit, newdata))[,1]
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    frame_name <- paste0("new_dat_",sample.int(10000, 1))
                    newdata <- as.h2o(newdata, destination_frame = frame_name)
                    as.data.frame(predict(modelFit, newdata))[,-1]
                  },
                  predictors = NULL,
                  varImp = function(object, numTrees = NULL, ...) {
                    out <- as.data.frame(h2o.varimp(object))
                    colnames(out)[colnames(out) == "relative_importance"] <- "Overall"
                    rownames(out) <- out$variable
                    out[, c("Overall"), drop = FALSE]   
                  },
                  levels = NULL,
                  tags = c("Generalized Linear Model", "Implicit Feature Selection", 
                           "L1 Regularization", "L2 Regularization", "Linear Classifier",
                           "Linear Regression", "Two Class Only"),
                  sort = function(x) x[order(-x$lambda, x$alpha),],
                  trim = NULL)
