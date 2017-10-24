modelInfo <- list(label = "Relevance Vector Machines with Linear Kernel",
                  library = "kernlab",
                  loop = NULL,
                  type = "Regression",
                  parameters = data.frame(parameter = "parameter",
                                          class = "character",
                                          label = "parameter"),
                  grid = function(x, y, len = NULL, search = "grid") data.frame(parameter = "none"),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    kernlab:::rvm(x = as.matrix(x), y = y,
                                  kernel = kernlab::vanilladot(),
                                  ...)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) 
                    kernlab::predict(modelFit, newdata),
                  prob = NULL,
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
                  tags = c("Kernel Method", "Relevance Vector Machines", "Linear Regression",
                           "Robust Methods"),
                  sort = function(x) x)
