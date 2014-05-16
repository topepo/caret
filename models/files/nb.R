modelInfo <- list(label = "Naive Bayes",
                  library = "klaR",
                  loop = NULL,
                  type = c('Classification'),
                  parameters = data.frame(parameter = c('fL', 'usekernel'),
                                          class = c('numeric', 'logical'),
                                          label = c('Laplace Correction', 'Distribution Type')),
                  grid = function(x, y, len = NULL) 
                    data.frame(usekernel = c(TRUE, FALSE), fL = 0),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) 
                    NaiveBayes(x, y, usekernel= param$usekernel, fL = param$fL, ...),
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(is.vector(newdata)) newdata <- as.data.frame(newdata)
                    predict(modelFit , newdata)$class
                  },
                  prob = function(modelFit, newdata, submodels = NULL)
                    predict(modelFit, newdata, type = "raw")$posterior,
                  predictors = function(x, ...) if(hasTerms(x)) predictors(x$terms) else x$varnames,
                  tags = c("Bayesian Model"),
                  levels = function(x) x$levels,
                  sort = function(x) x[order(x[,1]),])
