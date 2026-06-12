modelInfo <- list(label = "Linear Discriminant Analysis",
                  library = "MASS",
                  loop = function(grid) {
                    grid <- grid[order(grid$dimen, decreasing = TRUE),, drop = FALSE]
                    loop <- grid[1,,drop = FALSE]
                    submodels <- list(grid[-1,,drop = FALSE])
                    list(loop = loop, submodels = submodels)
                  },
                  type = c('Classification'),
                  parameters = data.frame(parameter = c('dimen'),
                                          class = c('numeric'),
                                          label = c('#Discriminant Functions')),
                  grid = function(x, y, len = NULL, search = "grid") data.frame(dimen = 1:min(ncol(x), length(levels(y)) - 1)),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) MASS::lda(x, y, ...)  ,
                  predict = function(modelFit, newdata, submodels = NULL) {
                    out <- as.character(predict(modelFit, newdata, dimen = modelFit$tuneValue$dimen)$class)
                    if(!is.null(submodels))
                    {
                      tmp <- vector(mode = "list", length = nrow(submodels) + 1)
                      tmp[[1]] <- out
                      for(j in seq(along = submodels$dimen))
                      {
                        tmp[[j+1]] <- as.character(predict(modelFit, newdata, dimen = submodels$dimen[j])$class)
                      }
                      out <- tmp
                    }                        
                    out
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    out <- predict(modelFit, newdata, dimen = modelFit$tuneValue$dimen)$posterior
                    if(!is.null(submodels))
                    {
                      tmp <- vector(mode = "list", length = nrow(submodels) + 1)
                      tmp[[1]] <- out
                      
                      for(j in seq(along = submodels$dimen))
                      {
                        tmpProb <- predict(modelFit, newdata, dimen = submodels$dimen[j])$posterior
                        tmp[[j+1]] <- as.data.frame(tmpProb[, modelFit$obsLevels, drop = FALSE], stringsAsFactors = TRUE)
                      }
                      out <- tmp
                    }                        
                    out
                  },
                  predictors = function(x, ...) if(hasTerms(x)) predictors(x$terms) else colnames(x$means),
                  tags = c("Discriminant Analysis", "Linear Classifier"),
                  levels = function(x) names(x$prior),
                  sort = function(x) x[order(x[,1]),])
