modelInfo <- list(label = "Bagged Logic Regression",
                  library = "logicFS",
                  loop = NULL,
                  type = c('Regression', 'Classification'),
                  parameters = data.frame(parameter = c('nleaves', 'ntrees'),
                                          class = c('numeric', 'numeric'),
                                          label = c('Maximum Number of Leaves', 'Number of Trees')),
                  grid = function(x, y, len = NULL, search = "grid"){
                    if(search == "grid") {
                      out <- expand.grid(ntrees = (1:len) + 1, nleaves = 2^((1:len) + 6))
                    } else {
                      out <- data.frame(ntrees = sample(1:10, size = len, replace = TRUE),
                                        nleaves = sample(1:10, size = len, replace = TRUE))
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...){
                    require(logicFS)
                    logicFS::logic.bagging(as.matrix(x), y,
                                  ntrees = param$ntrees,
                                  nleaves = param$nleaves,
                                  ...)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(modelFit$problemType == "Classification")
                    {
                      if(length(modelFit$obsLevels) == 2)
                      {
                        as.character(modelFit$obsLevels[predict(modelFit, newData = newdata) + 1])
                      } else {
                        as.character(predict(modelFit, newData = newdata))
                      }
                    } else predict(modelFit, newData = newdata)
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    if(length(modelFit$obsLevels) == 2)
                    {
                      out <- predict(modelFit, newData = newdata, type = "prob")
                      out <- as.data.frame(cbind(out, 1 - out), stringsAsFactors = TRUE)
                      colnames(out) <- modelFit$obsLevels
                    } else {
                      out <- predict(modelFit, newData = newdata, type = "prob")
                    }
                    out
                  },
                  predictors = function(x, ...) {
                    varNums <- lapply(x$logreg.model,
                                      function(y) lapply(y$trees,
                                                         function(z) z$trees$knot))
                    varNums <- sort(unique(unlist(varNums)))
                    varNums <- varNums[varNums > 0]
                    if(length(varNums) > 0) colnames(x$data)[varNums] else NA    
                  },
                  levels = function(x) x$obsLevels,
                  notes = paste(
                    "Unlike other packages used by `train`, the `logicFS`",
                    "package is fully loaded when this model is used."
                  ),
                  tags = c("Logic Regression", "Linear Classifier", "Linear Regression", "Logistic Regression",
                           "Bagging", "Ensemble Model", "Two Class Only", "Binary Predictors Only"),
                  sort = function(x) x[order(x$ntrees, x$nleaves),])
