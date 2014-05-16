modelInfo <- list(label = "Extreme Learning Machine",
                  library = "elmNN",
                  loop = NULL,
                  type = c("Classification", "Regression"),
                  parameters = data.frame(parameter = c('nhid', 'actfun'),
                                          class = c("numeric", "character"),
                                          label = c('#Hidden Units', 'Activation Function')),
                  grid = function(x, y, len = NULL) expand.grid(nhid = ((1:len) * 2) - 1, 
                                                                actfun = c("sin", "radbas", 
                                                                           "purelin", "tansig")),
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
                      out <- elmtrain(x = x, y = factor2ind(y), 
                                      nhid = param$nhid, actfun = param$actfun, ...)
                      out$lev <- levels(y)
                      
                    } else {
                      out <- elmtrain(x = x, y = y, nhid = param$nhid, actfun = param$actfun, ...)
                    }
                    out$xNames <- colnames(x)
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL)
                  {
                    out <- predict(modelFit, newdata, type="class")
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
                  sort = function(x) x[order(x$nhid),])
