modelInfo <- list(label = "Ensemble Partial Least Squares Regression with Feature Selection",
                  library = "enpls",
                  type = "Regression",
                  parameters = data.frame(parameter = c('maxcomp', 'threshold'),
                                          class = rep("numeric", 2),
                                          label = c('Max. #Components', "Importance Cutoff")),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      comps <- caret::var_seq(p = ncol(x), 
                                              classification = is.factor(y), 
                                              len = 1)
                      out <- expand.grid(maxcomp = comps, 
                                         threshold = seq(0, 2, length = len)) 
                    } else {
                      out <- data.frame(maxcomp = sample(1:comps, size = len, replace = TRUE),
                                        threshold = runif(len, min = 0, max = 5))
                    }
                    out
                  },
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    x <- if(is.matrix(x)) x else as.matrix(x)
                    vi <- enpls::enpls.fs(x = x, y = y, maxcomp = param$maxcomp, ...)[[1]]
                    if(any(vi > param$threshold)) {
                      keepers <- names(vi)[vi > param$threshold]
                    } else keepers <- names(vi)[which.max(vi)]
                    enpls::enpls.en(x = x[, keepers, drop = FALSE], y = y,
                             maxcomp = min(param$maxcomp, length(keepers)),
                             ...)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    newdata <- if(is.matrix(newdata)) newdata else as.matrix(newdata)
                    keepers <- rownames(modelFit[[1]][[1]]$loadings)
                    predict(modelFit, newdata[, keepers, drop = FALSE])          
                  },
                  predictors = function(x, ...) rownames(x$projection),
                  tags = c("Partial Least Squares", "Ensemble Model"),
                  prob = NULL,
                  sort = function(x) x[order(x[,1]),])
