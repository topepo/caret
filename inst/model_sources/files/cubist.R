modelInfo <- list(label = "Cubist",
                  library = "Cubist",
                  loop = function(grid) {
                    ## Here, we want `loop` to be a data frame with the unique values
                    ## of `committees`. We don't need `neighbors` until `predit.cubist`
                    ## is used.

                    grid <- grid[order(-grid$committees,
                                       grid$neighbors,
                                       decreasing = TRUE),,
                                 drop = FALSE]

                    uniqueCom <- unique(grid$committees)

                    loop <- data.frame(committees = uniqueCom)
                    loop$neighbors <- NA

                    submodels <- vector(mode = "list", length = length(uniqueCom))
                    ## For each value of committees, find the largest
                    ## value of `neighbors` and assign it to `loop`.
                    ## Then save the rest to a data frame and add it to
                    ## `submodels`.
                    for(i in seq(along = uniqueCom)) {
                      subK <- grid[grid$committees == uniqueCom[i],"neighbors"]
                      loop$neighbors[loop$committees == uniqueCom[i]] <- subK[which.max(subK)]
                      submodels[[i]] <- data.frame(neighbors = subK[-which.max(subK)])
                    }
                    list(loop = loop, submodels = submodels)
                  },
                  type = "Regression",
                  parameters = data.frame(parameter = c('committees', 'neighbors'),
                                          class = rep('numeric', 2),
                                          label = c('#Committees', '#Instances')),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(neighbors = c(0, 5, 9), committees = c(1, 10, 20))
                    } else {
                      out <- data.frame(neighbors = sample(0:9, replace = TRUE, size = len),
                                        committees = sample(1:100, replace = TRUE, size = len))
                    }

                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    out <- Cubist::cubist(x, y, committees =  param$committees,  ...)
                    if(last) out$tuneValue$neighbors <- param$neighbors
                    out
                    },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    out <- predict(modelFit, newdata, neighbors = modelFit$tuneValue$neighbors)
                    if(!is.null(submodels)) {
                      tmp <- vector(mode = "list", length = nrow(submodels) + 1)
                      tmp[[1]] <- out

                      for(j in seq(along = submodels$neighbors))
                        tmp[[j+1]] <- predict(modelFit, newdata, neighbors = submodels$neighbors[j])

                      out <- tmp
                    }
                    out
                  },
                  varImp = function(object, weights = c(0.5, 0.5), ...) {
                    if(length(weights) != 2) stop("two weights must be given")
                    weights <- weights/sum(weights)
                    out <- data.frame(Overall =
                                        object$usage$Conditions*weights[1] +
                                        object$usage$Model*weights[2])
                    rownames(out) <- object$usage$Variable
                    out
                  },
                  predictors = function(x, ...) {
                    subset(x$usage, Conditions > 0 | Model > 0)$Variable
                  },
                  tags = c("Rule-Based Model", "Boosting", "Ensemble Model",
                           "Prototype Models", "Model Tree", "Linear Regression",
                           "Implicit Feature Selection"),
                  prob = NULL,
                  sort = function(x) x[order(x$committees,  x$neighbors),])
