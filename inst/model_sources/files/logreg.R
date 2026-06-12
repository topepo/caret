modelInfo <- list(label = "Logic Regression",
                  library = "LogicReg",
                  loop = NULL,
                  type = c('Regression', 'Classification'),
                  parameters = data.frame(parameter = c('treesize', 'ntrees'),
                                          class = c('numeric', 'numeric'),
                                          label = c('Maximum Number of Leaves', 'Number of Trees')),
                  grid = function(x, y, len = NULL, search = "grid") expand.grid(ntrees = (1:3) + 1, treesize = 2^(1+(1:len))),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...){
                    isReg <- is.numeric(y)
                    if(is.factor(y)) y <- ifelse(y == levels(y)[1], 1, 0)
                    LogicReg::logreg(resp = y, bin = x,
                                     ntrees = param$ntrees,
                                     tree.control = LogicReg::logreg.tree.control(treesize = param$treesize),
                                     select = 1,
                                     type = ifelse(isReg, 2, 3),
                                     ...)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(modelFit$type == "logistic")
                    {
                      out <- ifelse(predict(modelFit, newbin = newdata) >= .5,
                                    modelFit$obsLevels[1], modelFit$obsLevels[2])
                    } else out <- predict(modelFit, newbin = newdata)
                    out
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    tmp <- predict(modelFit, newbin = newdata)
                    out <- cbind(tmp, 1 - tmp)
                    colnames(out) <- modelFit$obsLevels
                    out
                  },
                  predictors = function(x, ...) {
                    getVarIndex <- function(y) unique(y$trees$knot)
                    varNums <- unique(unlist(lapply(x$model$trees, getVarIndex)))
                    varNums <- varNums[varNums > 0]
                    if(length(varNums) > 0) colnames(x$binary)[varNums] else NA
                  },
                  levels = function(x) x$obsLevels,
                  tags = c("Logic Regression", "Linear Classifier", "Linear Regression", "Logistic Regression", "Two Class Only", "Binary Predictors Only"),
                  sort = function(x) x[order(x$ntrees, x$treesize),])
