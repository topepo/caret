modelInfo <- list(label = "Self-Organizing Maps",
                  library = "kohonen",
                  check = function(pkg) {
                    requireNamespace("kohonen")
                    current <- packageDescription("kohonen")$Version
                    expected <- "3.0.0"
                    if(compareVersion(current, expected) < 0)
                      stop("This modeling workflow requires kohonen version ",
                           expected, "or greater.", call. = FALSE)
                  },
                  loop = NULL,
                  type = c("Classification", "Regression"),
                  parameters = data.frame(parameter = c("xdim", "ydim", "user.weights", "topo"),
                                          class = c(rep("numeric", 3), "character"),
                                          label = c("Rows", "Columns", "Layer Weight", "Topology")),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(xdim = 1:len, ydim = 2:(len+1),
                                         user.weights = seq(.2, .8, length = len),
                                         topo = "hexagonal")
                      out <- subset(out, xdim <= ydim)
                    } else {
                      out <- data.frame(xdim = sample(1:20, size = len*10, replace = TRUE),
                                        ydim = sample(1:20, size = len*10, replace = TRUE),
                                        topo = sample(c("rectangular", "hexagonal"), size = len*10, replace = TRUE),
                                        user.weights = runif(len*10, min = .01, max = .99))
                      out <- subset(out, xdim <= ydim & xdim*ydim < nrow(x))
                      out <- out[1:min(nrow(out), len),]
                    }
                    out
                  }, 
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    layer_wts <- c(1 - param$user.weights, param$user.weights)
                    layer_wts <- layer_wts/sum(layer_wts)
                    if(is.numeric(y))
                      y <- as.matrix(y, ncol = 1)
                    kohonen::supersom(list(X = as.matrix(x), Y = y),
                             user.weights = layer_wts,
                             grid = kohonen::somgrid(param$xdim, param$ydim, as.character(param$topo)),
                             ...)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    out <- predict(modelFit, list(X = as.matrix(newdata)), whatmap = "X")$predictions$Y
                    if(is.factor(out))
                      out <- as.character(out)
                    out
                  },
                  prob = function(modelFit, newdata, submodels = NULL){
                    preds <- predict(modelFit, list(X = as.matrix(newdata)), whatmap = "X")
                    preds <- preds$unit.predictions$Y[preds$unit.classif,]
                    as.data.frame(preds)
                  },
                  levels = function(x) x$obsLevels,
                  tags = c("Self-Organising Maps"),
                  sort = function(x) x[order(x$xdim,  x$ydim),],
                  notes = paste("As of version 3.0.0 of the kohonen package, the argument `user.weights`",
                                "replaces the old `alpha` parameter. `user.weights` is usually a vector of",
                                "relative weights such as `c(1, 3)` but is parameterized here as a proportion",
                                "such as `c(1-.75, .75)` where the .75 is the value of the tuning parameter",
                                "passed to `train` and indicates that the outcome layer has 3 times the weight",
                                "as the predictor layer."))
