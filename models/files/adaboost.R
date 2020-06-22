modelInfo <- list(label = "AdaBoost Classification Trees",
                  library = c("fastAdaboost"),
                  loop = NULL,
                  type = c("Classification"),
                  parameters = data.frame(parameter = c('nIter', 'method'),
                                          class = c("numeric", "character"),
                                          label = c('#Trees', 'Method')),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out  = expand.grid(nIter = floor((1:len) * 50),
                                         method = c("Adaboost.M1", "Real adaboost"))
                    } else {
                      out <- data.frame(nIter =  sample(1:1000, replace = TRUE, size = len),
                                        method = sample(c("Adaboost.M1", "Real adaboost"), replace = TRUE, size = len))
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <-
                      if (!is.data.frame(x) | inherits(x, "tbl_df"))
                        as.data.frame(x, stringsAsFactors = TRUE)
                    else
                      x
                    dat$.outcome <- y
                    out <- if(param$method == "Adaboost.M1") 
                      fastAdaboost::adaboost(.outcome ~ ., data = dat, nIter = param$nIter, ...) else 
                      fastAdaboost::real_adaboost(.outcome ~ ., data = dat, nIter = param$nIter, ...) 
                    out     
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata) | inherits(newdata, "tbl_df")) 
                      newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    predict(modelFit, newdata)$class
                  },
                  prob = function(modelFit, newdata, submodels = NULL){
                    if(!is.data.frame(newdata) | inherits(newdata, "tbl_df")) 
                      newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    out <- predict(modelFit, newdata)$prob
                    out <- t(apply(out, 1, function(x) ifelse(x == Inf, 1, x)))
                    out <- t(apply(out, 1, function(x) ifelse(x == -Inf, 0, x))) 
                    out <- as.data.frame(out, stringsAsFactors = TRUE)
                    colnames(out) <- as.vector(modelFit$classnames)
                    out 
                  },
                  levels = function(x) as.vector(x$classnames),
                  predictors = function(x, ...) unique(unlist(lapply(x$trees, predictors))),
                  tags = c("Tree-Based Model", "Ensemble Model", "Boosting", 
                           "Implicit Feature Selection", "Two Class Only"),
                  sort = function(x) x[order(x$nIter),])


