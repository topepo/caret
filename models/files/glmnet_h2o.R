modelInfo <- list(label = "glmnet",
                  library = "h2o",
                  type = c("Regression", "Classification"),
                  parameters = data.frame(parameter = c('alpha', 'lambda'),
                                          class = c("numeric", "numeric"),
                                          label = c('Mixing Percentage', 'Regularization Parameter')),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(alpha = seq(0, 1, length = len),
                                        lambda = c(0, 10 ^ seq(-1, -4, length = len - 1)))
                    } else {
                      out <- data.frame(alpha = runif(len, min = 0, 1),
                                        lambda = 2^runif(len, min = -10, 3))
                    }
                    out
                  },
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- if(!is.data.frame(x)) as.data.frame(x, stringsAsFactors = TRUE) else x
                    dat$.outcome <- y
                    p <- ncol(dat)
                    frame_name <- paste0("tmp_glmnet_dat_",sample.int(100000, 1))
                    tmp_train_dat = h2o::as.h2o(dat, destination_frame = frame_name)
                    out <- h2o::h2o.glm(x = colnames(x), y = ".outcome",
                                       training_frame = tmp_train_dat,
                                       family = if(is.factor(y)) "binomial" else "gaussian",
                                       alpha = param$alpha, lambda = param$lambda, ...)
                    h2o::h2o.getModel(out@model_id)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    frame_name <- paste0("new_glmnet_dat_",sample.int(100000, 1))
                    newdata <- h2o::as.h2o(newdata, destination_frame = frame_name)
                    as.data.frame(predict(modelFit, newdata), stringsAsFactors = TRUE)[,1]
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    frame_name <- paste0("new_glmnet_dat_",sample.int(100000, 1))
                    newdata <- h2o::as.h2o(newdata, destination_frame = frame_name)
                    as.data.frame(predict(modelFit, newdata), stringsAsFactors = TRUE)[,-1]
                  },
                  predictors = function(object, ...) {
                    out <- as.data.frame(h2o::h2o.varimp(object), stringsAsFactors = TRUE)
                    colnames(out)[colnames(out) == "coefficients"] <- "Overall"
                    out <- out[!is.na(out$Overall),]   
                    out$names
                  },
                  varImp = function(object, ...) {
                    out <- as.data.frame(h2o::h2o.varimp(object), stringsAsFactors = TRUE)
                    colnames(out)[colnames(out) == "coefficients"] <- "Overall"
                    rownames(out) <- out$names
                    out <- out[!is.na(out$Overall), c("Overall"), drop = FALSE]   
                    all_var <- object@allparameters$x
                    if(any(!(all_var %in% rownames(out)))) {
                      missing <- all_var[!(all_var %in% rownames(out))]
                      tmp <- data.frame(OVerall = rep(0, length(missing)))
                      rownames(tmp) <- missing
                      out <- rbind(out, tmp)
                    }
                    out
                  },
                  levels = NULL,
                  tags = c("Generalized Linear Model", "Implicit Feature Selection", 
                           "L1 Regularization", "L2 Regularization", "Linear Classifier",
                           "Linear Regression", "Two Class Only"),
                  sort = function(x) x[order(-x$lambda, x$alpha),],
                  trim = NULL)
