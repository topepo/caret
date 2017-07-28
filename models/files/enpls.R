modelInfo <- list(label = "Ensemble Partial Least Squares Regression",
                  library = "enpls",
                  type = "Regression",
                  parameters = data.frame(parameter = c('maxcomp'),
                                          class = "numeric",
                                          label = c('Max. #Components')),
                  grid = function(x, y, len = NULL, search = "grid") {
                    comps <- caret::var_seq(p = ncol(x), 
                                            classification = is.factor(y), 
                                            len = 1)
                    if(search == "grid") {
                      out <- data.frame(maxcomp = comps) 
                    } else {
                      out <- data.frame(maxcomp = sample(1:comps, size = len, replace = TRUE))
                    }
                    out
                  },
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    x <- if(is.matrix(x)) x else as.matrix(x)
                    enpls::enpls.en(x = x, y = y, maxcomp = param$maxcomp,
                             ...)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    newdata <- if(is.matrix(newdata)) newdata else as.matrix(newdata)
                    predict(modelFit, newdata)          
                  },
                  predictors = function(x, ...) rownames(x$projection),
                  tags = c("Partial Least Squares", "Ensemble Model"),
                  prob = NULL,
                  sort = function(x) x[order(x[,1]),])
