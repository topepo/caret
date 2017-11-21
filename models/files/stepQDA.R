modelInfo <- list(label = "Quadratic Discriminant Analysis with Stepwise Feature Selection",
                  library = c("klaR", "MASS"),
                  loop = NULL,
                  type = c("Classification"),
                  parameters = data.frame(parameter = c("maxvar", "direction"),
                                          class = c("numeric", "character"),
                                          label = c('Maximum #Variables', 'Search Direction')),
                  grid = function(x, y, len = NULL, search = "grid") 
                    data.frame(maxvar = Inf, direction = "both"),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...){
                    out <- klaR::stepclass(x, y,
                                           method = "qda",
                                           maxvar = param$maxvar,
                                           direction = as.character(param$direction),
                                           ...)
                    out$fit <- MASS::qda(x[, out$model$name, drop = FALSE], y, ...)
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    code <- getModelInfo("qda", regex = FALSE)[[1]]$predictors
                    predict(modelFit$fit, newdata[,  code(modelFit$fit), drop = FALSE])$class
                    },
                  prob = function(modelFit, newdata, submodels = NULL){
                    code <- getModelInfo("qda", regex = FALSE)[[1]]$predictors
                    predict(modelFit$fit, newdata[, code(modelFit$fit), drop = FALSE])$posterior
                  },
                  predictors = function(x, ...) {
                    form <- x$formula
                    form[[2]] <- NULL
                    all.vars(form)
                  },
                  levels = function(x) x$obsLevels,
                  tags = c("Discriminant Analysis", "Feature Selection Wrapper", "Polynomial Model"),
                  sort = function(x) x)
