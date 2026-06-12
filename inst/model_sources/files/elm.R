modelInfo <- list(label = "Extreme Learning Machine",
                  library = "elmNN",
                  loop = NULL,
                  type = c("Classification", "Regression"),
                  parameters = data.frame(parameter = c('nhid', 'actfun'),
                                          class = c("numeric", "character"),
                                          label = c('#Hidden Units', 'Activation Function')),
                  grid = function(x, y, len = NULL, search = "grid") {
                    funs <- c("sin", "radbas", "purelin", "tansig")
                    if(search == "grid") {
                      out <- expand.grid(nhid = ((1:len) * 2) - 1, actfun = funs)
                    } else {
                      out <- data.frame(nhid = floor(runif(len, min = 1, max = 20)),
                                        actfun = sample(funs, replace = TRUE, size = len))
                    }
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    if(is.factor(y)) {
                      factor2ind <- function(x) {
                        x <- model.matrix(~x - 1, contrasts = list(x = "contr.treatment"))
                        colnames(x) <- gsub("^x", "", colnames(x))
                        att <- attributes(x)
                        att$assign <- NULL
                        att$contrasts <- NULL
                        attributes(x) <- att
                        x
                      }
                      out <- elmNN::elmtrain.default(
                        x = x,
                        y = factor2ind(y),
                        nhid = param$nhid,
                        actfun = param$actfun,
                        ...
                        )
                      out$lev <- levels(y)

                    } else {
                      out <- elmNN::elmtrain.default(
                        x = x,
                        y = y,
                        nhid = param$nhid,
                        actfun = param$actfun,
                        ...
                        )
                    }
                    out$xNames <- colnames(x)
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL)
                  {
                    out <- elmNN::predict.elmNN(modelFit, newdata)
                    if(modelFit$problemType == "Classification") {
                      out <- modelFit$lev[apply(out, 1, which.max)]
                      out <- factor(out, levels = modelFit$lev)
                    }
                    out
                  },
                  prob = NULL,
                  varImp = NULL,
                  predictors = function(x, ...) x$xNames,
                  tags = c("Neural Network"),
                  levels = function(x) x$lev,
                  notes = paste(
                    "The package is no longer on CRAN but can be installed",
                    "from the archive at ",
                    "https://cran.r-project.org/src/contrib/Archive/elmNN/"
                  ),
                  sort = function(x) x[order(x$nhid),])
