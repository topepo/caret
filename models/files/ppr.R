modelInfo <- list(label = "Projection Pursuit Regression",
                  library = NULL,
                  loop = NULL,
                  type = c('Regression'),
                  parameters = data.frame(parameter = c('nterms'),
                                          class = c('numeric'),
                                          label = c('# Terms')),
                  grid = function(x, y, len = NULL, search = "grid") data.frame(nterms = 1:len),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    if(!is.null(wts))
                    {
                      out <- ppr(as.matrix(x),
                                 y,
                                 weights = wts,
                                 nterms = param$nterms,
                                 ...)
                    } else {
                      out <- ppr(as.matrix(x), y, nterms = param$nterms, ...)
                    }
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) predict(modelFit, newdata),
                  prob = NULL,
                  predictors = function(x, ...) x$xnames,
                  tags = c("Feature Extraction", "Accepts Case Weights"),
                  sort = function(x) x[order(x[,1]),])
