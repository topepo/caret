modelInfo <- list(label = "Flexible Discriminant Analysis",
                  library = c("earth", "mda"),
                  loop = NULL,
                  type = "Classification",
                  parameters = data.frame(parameter = c("degree", "nprune"),
                                          class = c("numeric", "numeric"),
                                          label = c('Product Degree', '#Terms')),
                  grid = function(x, y, len = NULL, search = "grid") {
                    dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
                    dat$.outcome <- y

                    mod <- earth::earth( .outcome~., data = dat, pmethod = "none")
                    maxTerms <- nrow(mod$dirs)

                    maxTerms <- min(200, floor(maxTerms * .75) + 2)
                    if(search == "grid") {
                      out <- data.frame(nprune = unique(floor(seq(2, to = maxTerms, length = len))),
                                        degree = 1)
                    } else {
                      out <- data.frame(nprune = sample(2:maxTerms, size = len, replace = TRUE),
                                        degree = sample(1:2, size = len, replace = TRUE))
                    }
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                  	require(earth)
                    dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
                    dat$.outcome <- y

                    mda::fda(.outcome ~ ., data = dat, method = earth::earth,
                        degree = param$degree,
                        nprune = param$nprune,
                        weights = wts,
                        ...)
                  },
                  levels = function(x) x$obsLevels,
                  tags = c("Multivariate Adaptive Regression Splines", "Implicit Feature Selection",
                           "Accepts Case Weights"),
                  notes = paste(
                    "Unlike other packages used by `train`, the `earth`",
                    "package is fully loaded when this model is used."
                  ),
                  predict = function(modelFit, newdata, submodels = NULL)
                    predict(modelFit , newdata),
                  prob = function(modelFit, newdata, submodels = NULL)
                    predict(modelFit, newdata, type= "posterior"),
                  predictors = function(x, ...) {
                    code <- getModelInfo("earth", regex = FALSE)[[1]]$predictors
                    tmp <- predictors(x$terms)
                    out <- if(class(x$fit) == "earth") code(x$fit) else tmp
                    out
                  },
                  varImp = function(object, value = "gcv", ...)
                    varImp(object$fit, value = value, ...),
                  sort = function(x) x[order(x$degree, x$nprune),])
