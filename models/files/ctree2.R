modelInfo <- list(label = "Conditional Inference Tree",
                  library = "party",
                  loop = NULL,
                  type = c('Regression', 'Classification'),
                  parameters = data.frame(parameter = c('maxdepth', 'mincriterion'),
                                          class = c('numeric', 'numeric'),
                                          label = c('Max Tree Depth', '1 - P-Value Threshold')),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(maxdepth = 1:len, mincriterion = seq(from = .99, to = 0.01, length = len))
                    } else {
                      out <- data.frame(maxdepth = sample(1:15, replace = TRUE, size = len),
                                        mincriterion = runif(len, min = 0, max = 1))
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
                    dat$.outcome <- y
                    theDots <- list(...)
                    if(any(names(theDots) == "controls"))
                    {
                      theDots$controls@tgctrl@maxdepth <- param$maxdepth
                      theDots$controls@gtctrl@mincriterion <- param$mincriterion
                      ctl <- theDots$controls
                      theDots$controls <- NULL
                      
                    } else ctl <- do.call(party::ctree_control, 
                                          list(maxdepth = param$maxdepth,
                                               mincriterion = param$mincriterion))
                    ## pass in any model weights
                    if(!is.null(wts)) theDots$weights <- wts
                    modelArgs <- c(
                                   list(
                                        formula = as.formula(".outcome ~ ."),
                                        data = dat,
                                        controls = ctl),
                                   theDots)
                    out <- do.call(party::ctree, modelArgs)
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    out <- predict(modelFit, newdata)
                    if(!is.null(modelFit@responses@levels$.outcome)) out <- as.character(out)
                    if(is.matrix(out)) out <- out[,1]
                    out
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    obsLevels <- levels(modelFit@data@get("response")[,1])
                    rawProbs <- party::treeresponse(modelFit, newdata)
                    probMatrix <- matrix(unlist(rawProbs), ncol = length(obsLevels), byrow = TRUE)
                    out <- data.frame(probMatrix)
                    colnames(out) <- obsLevels
                    rownames(out) <- NULL
                    out
                  },
                  predictors = function(x, surrogate = TRUE, ...) {
                    treeObj <- unlist(nodes(x, 1))
                    target <- "psplit\\.variableName"
                    vars <- treeObj[grep(target, names(treeObj))]
                    if(surrogate)
                    {
                      target2 <- "ssplits\\.variableName"
                      svars <- treeObj[grep(target, names(treeObj))]
                      vars <- c(vars, svars)
                      
                    }
                    unique(vars)
                  },
                  tags = c('Tree-Based Model', "Implicit Feature Selection", "Accepts Case Weights"),
                  levels = function(x) levels(x@data@get("response")[,1]),
                  sort = function(x) x[order(x[,1]),])
