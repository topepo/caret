modelInfo <- list(label = "Penalized Discriminant Analysis",
                  library = "mda",
                  loop = NULL,
                  type = c('Classification'),
                  parameters = data.frame(parameter = c('df'),
                                          class = c('numeric'),
                                          label = c('Degrees of Freedom')),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- data.frame(df = 2* (0:(len - 1) + 1))
                    } else {
                      out <- data.frame(df = runif(len, min = 1, max = 5))
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
                                      df = param$df,
                                      ...)
                    } else {
                      out <- mda::fda(as.formula(".outcome ~ ."),
                                      data = dat,
                                      method = mda::gen.ridge,
                                      df = param$df,
                                      ...)
                    }
                    out                   
                  },
                  levels = function(x) x$obsLevels,
                  predict = function(modelFit, newdata, submodels = NULL) 
                    predict(modelFit, newdata),
                  prob = function(modelFit, newdata, submodels = NULL)
                    predict(modelFit, newdata, type = "posterior"),
                  tags = c("Discriminant Analysis", "Polynomial Model", "Accepts Case Weights"),
                  sort = function(x) x[order(x[,1]),])
