modelInfo <- list(label = "Self-Organizing Map", 
                  library = "kohonen",
                  loop = NULL,
                  type = c("Classification", "Regression"),
                  parameters = data.frame(parameter = c("xdim", "ydim", "xweight", "topo"),
                                          class = c(rep("numeric", 3), "character"),
                                          label = c("Row", "Columns", "X Weight", "Topology")),
                  grid = function(x, y, len = NULL) {
                    out <- expand.grid(xdim = 1:len, ydim = 2:(len+1),
                                       xweight = seq(.5, .9, length = len))
                    out$topo <- "hexagonal"
                    subset(out, xdim <= ydim)
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) 
                    bdk(as.matrix(x),
                        Y = if(is.factor(y)) classvec2classmat(y) else y,
                        xweight = param$xweight,
                        contin = !is.factor(y),
                        grid = somgrid(param$xdim, param$ydim, param$topo),
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
                  tags = c("Self-Organising Maps"),
                  sort = function(x) x[order(x$xdim,  x$ydim),])
