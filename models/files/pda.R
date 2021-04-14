modelInfo <- list(label = "Penalized Discriminant Analysis",
                  library = "mda",
                  loop = NULL,
                  type = c('Classification'),
                  parameters = data.frame(parameter = c('lambda'),
                                          class = c('numeric'),
                                          label = c('Shrinkage Penalty Coefficient')),
                  grid = function(x, y, len = NULL, search = "grid"){
                    if(search == "grid") {
                      out <- data.frame(lambda =  c(0, 10 ^ seq(-1, -4, length = len - 1)))
                    } else {
                      out <- data.frame(lambda = 10^runif(len, min = -5, 1))
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
                    dat$.outcome <- y
                    if(!is.null(wts))
                    {
                      out <- mda::fda(as.formula(".outcome ~ ."),
                                      data = dat,
                                      method = mda::gen.ridge,
                                      weights = wts,
                                      lambda = param$lambda,
                                      ...)
                    } else {
                      out <- mda::fda(as.formula(".outcome ~ ."),
                                      data = dat,
                                      method = mda::gen.ridge,
                                      lambda = param$lambda,
                                      ...)
                    }
                    out                   
                  },
                  predict = function(modelFit, newdata, submodels = NULL) 
                    predict(modelFit, newdata),
                  prob = function(modelFit, newdata, submodels = NULL)
                    predict(modelFit, newdata, type = "posterior"),
                  levels = function(x) x$obsLevels,
                  tags = c("Discriminant Analysis", "Polynomial Model", "Accepts Case Weights"),
                  sort = function(x) x[order(x[,1]),])
