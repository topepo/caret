modelInfo <- list(label = "Relevance Vector Machines with Radial Basis Function Kernel",
                  library = "kernlab",
                  loop = NULL,
                  type = "Regression",
                  parameters = data.frame(parameter = c("sigma"),
                                          class = "numeric",
                                          label = "Sigma"),
                  grid = function(x, y, len = NULL, search = "grid"){
                    sigmas <- kernlab::sigest(as.matrix(x), na.action = na.omit, scaled = TRUE)
                    if(search == "grid") {
                      out <- expand.grid(sigma = mean(as.vector(sigmas[-2])))
                    } else {
                      rng <- extendrange(log(sigmas), f = .75)
                      out <- data.frame(sigma = exp(runif(len, min = rng[1], max = rng[2])))
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last,classProbs, ...) {
                    kernlab:::rvm(x = as.matrix(x), y = y,
                                  kernel = "rbfdot",
                                  kpar = list(sigma = param$sigma),
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
                  tags = c("Kernel Method", "Relevance Vector Machines", "Radial Basis Function",
                           "Robust Methods"),
                  sort = function(x) x[order(-x$sigma),])
