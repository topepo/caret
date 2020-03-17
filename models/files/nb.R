modelInfo <- list(label = "Naive Bayes",
                  library = "klaR",
                  loop = NULL,
                  type = c('Classification'),
                  parameters = data.frame(parameter = c('fL', 'usekernel', "adjust"),
                                          class = c('numeric', 'logical', "numeric"),
                                          label = c('Laplace Correction', 'Distribution Type', "Bandwidth Adjustment")),
                  grid = function(x, y, len = NULL, search = "grid") 
                    expand.grid(usekernel = c(TRUE, FALSE), fL = 0, adjust = 1),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                   if(param$usekernel) {
                          out <- klaR::NaiveBayes(x, y, usekernel = TRUE,  fL = param$fL, adjust = param$adjust, ...)
                   } else out <- klaR::NaiveBayes(x, y, usekernel = FALSE, fL = param$fL, ...)
                   out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    predict(modelFit , newdata)$class
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    predict(modelFit, newdata, type = "raw")$posterior
                    },
                  predictors = function(x, ...) if(hasTerms(x)) predictors(x$terms) else x$varnames,
                  tags = c("Bayesian Model"),
                  levels = function(x) x$levels,
                  sort = function(x) x[order(x[,1]),])
