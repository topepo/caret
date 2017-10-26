modelInfo <- list(label = "Gaussian Process",
                  library = "kernlab",
                  type = c("Regression", "Classification"),
                  parameters = data.frame(parameter = c('parameter'),
                                          class = c("character"),
                                          label = c('Parameter')),
                  grid = function(x, y, len = NULL, search = "grid") data.frame(parameter = "none"),
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    kernlab::gausspr(x = as.matrix(x), y = y,
                                     kernel = "vanilladot",
				     kpar = list(), ...)
                    },
                  predict = function(modelFit, newdata, submodels = NULL) {            
                    out <- kernlab::predict(modelFit, as.matrix(newdata))
                    if(is.matrix(out)) out <- out[,1]
                    out
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    kernlab::predict(modelFit, as.matrix(newdata), type = "probabilities")
                  },
                  predictors = function(x, ...) {
                    if(hasTerms(x) & !is.null(x@terms))
                    {
                      out <- predictors.terms(x@terms)
                    } else {
                      out <- colnames(attr(x, "xmatrix"))
                    }
                    if(is.null(out)) out <- names(attr(x, "scaling")$x.scale$`scaled:center`)
                    if(is.null(out)) out <-NA
                    out
                  },
                  tags = c("Kernel Method", "Gaussian Process", "Linear Classifier"),
                  levels = function(x) lev(x),
                  sort = function(x) x)
