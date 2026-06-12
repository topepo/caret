modelInfo <- list(label = "Naive Bayes",
                  library = "naivebayes",
                  loop = NULL,
                  type = c('Classification'),
                  parameters = data.frame(parameter = c('laplace', 'usekernel', "adjust"),
                                          class = c('numeric', 'logical', "numeric"),
                                          label = c('Laplace Correction', 'Distribution Type', "Bandwidth Adjustment")),
                  grid = function(x, y, len = NULL, search = "grid") 
                    expand.grid(usekernel = c(TRUE, FALSE), laplace = 0, adjust = 1),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                   if(param$usekernel) {
                          out <- naivebayes::naive_bayes(x, y, usekernel = TRUE,  laplace = param$laplace, adjust = param$adjust, ...)
                   } else out <- naivebayes::naive_bayes(x, y, usekernel = FALSE, laplace = param$laplace, ...)
                   out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    predict(modelFit , newdata)
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    as.data.frame(predict(modelFit, newdata, type = "prob"), stringsAsFactors = TRUE)
                    },
                  predictors = function(x, ...) if(hasTerms(x)) predictors(x$terms) else names(x$tables),
                  tags = c("Bayesian Model"),
                  levels = function(x) x$levels,
                  sort = function(x) x[order(x[,1]),])
