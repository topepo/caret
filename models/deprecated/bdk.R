modelInfo <- list(label = "Self-Organizing Map",
                  library = c("kohonen", "class"),
                  loop = NULL,
                  type = c("Classification", "Regression"),
                  parameters = data.frame(parameter = c("xdim", "ydim", "xweight", "topo"),
                                          class = c(rep("numeric", 3), "character"),
                                          label = c("Row", "Columns", "X Weight", "Topology")),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(xdim = 1:len, ydim = 2:(len+1),
                                         xweight = seq(.5, .9, length = len),
                                         topo = "hexagonal")
                      out <- subset(out, xdim <= ydim)
                    } else {
                      out <- data.frame(xdim = sample(1:len, size = len*10, replace = TRUE),
                                        ydim = sample(1:len, size = len*10, replace = TRUE),
                                        topo = sample(c("rectangular", "hexagonal"), size = len*10, replace = TRUE),
                                        xweight = runif(len*10, min = .5, max = 1))
                      out <- subset(out, xdim <= ydim & xdim*ydim < nrow(x))
                      out <- out[1:min(nrow(out), len),]
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...)
                    kohonen::bdk(as.matrix(x),
                        Y = if(is.factor(y)) kohonen::classvec2classmat(y) else y,
                        xweight = param$xweight,
                        contin = !is.factor(y),
                        grid = class::somgrid(param$xdim, param$ydim, as.character(param$topo)),
                        ...),
                  predict = function(modelFit, newdata, submodels = NULL) {
                    out <- predict(modelFit, as.matrix(newdata))$prediction
                    if(modelFit$problemType == "Classification") {
                      out <- as.character(out)
                    } else {
                      if(is.matrix(out)) out <- out[,1]
                    }
                    out
                  },
                  prob = function(modelFit, newdata, submodels = NULL){
                    preds <- predict(modelFit, as.matrix(newdata))
                    preds$unit.predictions[preds$unit.classif,,drop = FALSE]
                  },
                  levels = function(x) x$obsLevels,
                  tags = c("Self-Organising Maps"),
                  sort = function(x) x[order(x$xdim,  x$ydim),])
