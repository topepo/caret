modelInfo <- list(label = "Conditional Inference Random Forest",
                  library = "party",
                  loop = NULL,
                  type = c("Classification", "Regression"),
                  parameters = data.frame(parameter = 'mtry',
                                          class = 'numeric',
                                          label = "#Randomly Selected Predictors"),
                  grid = function(x, y, len = NULL, search = "grid"){
                    if(search == "grid") {
                      out <- data.frame(mtry = caret::var_seq(p = ncol(x), 
                                                              classification = is.factor(y), 
                                                              len = len))
                    } else {
                      out <- data.frame(mtry = unique(sample(1:ncol(x), replace = TRUE, size = len)))
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
                    dat$.outcome <- y
                    theDots <- list(...)
                    
                    if(any(names(theDots) == "controls"))
                    {
                      theDots$controls@gtctrl@mtry <- as.integer(param$mtry) 
                      ctl <- theDots$controls
                      theDots$controls <- NULL

                    } else ctl <- party::cforest_control(mtry = min(param$mtry, ncol(x)))

                    ## pass in any model weights
                    if(!is.null(wts)) theDots$weights <- wts
                    
                    modelArgs <- c(list(formula = as.formula(.outcome ~ .),
                                        data = dat,
                                        controls = ctl),
                                   theDots)

                    out <- do.call(party::cforest, modelArgs)
                    out
                  },
                  predict = function(modelFit, newdata = NULL, submodels = NULL) {
                    if(!is.null(newdata) && !is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    ## party builds the levels into the model object, so I'm
                    ## going to assume that all the levels will be passed to
                    ## the output
                    out <- party:::predict.RandomForest(modelFit, newdata = newdata, OOB = TRUE)
                    if(is.matrix(out)) out <- out[,1]
                    if(!is.null(modelFit@responses@levels$.outcome)) out <- as.character(out)
                    
                    out
                  },
                  prob = function(modelFit, newdata = NULL, submodels = NULL) {
                    if(!is.null(newdata) && !is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    obsLevels <- levels(modelFit@data@get("response")[,1])
                    rawProbs <- party::treeresponse(modelFit, newdata = newdata, OOB = TRUE)
                    probMatrix <- matrix(unlist(rawProbs), ncol = length(obsLevels), byrow = TRUE)
                    out <- data.frame(probMatrix)
                    colnames(out) <- obsLevels
                    rownames(out) <- NULL
                    out
                  },
                  predictors = function(x, ...) {
                    vi <- party::varimp(x, ...)
                    names(vi)[vi != 0]
                  },
                  varImp = function(object, ...) {
                    variableImp <- party::varimp(object, ...)
                    out <- data.frame(Overall = variableImp)
                    out
                  },
                  tags = c("Random Forest", "Ensemble Model", "Bagging", "Implicit Feature Selection", "Accepts Case Weights"),
                  levels = function(x) levels(x@data@get("response")[,1]),
                  sort = function(x) x[order(x[,1]),],
                  oob = function(x) {
                    obs <- x@data@get("response")[,1]
                    pred <- party:::predict.RandomForest(x, OOB = TRUE)
                    postResample(pred, obs)
                  })
