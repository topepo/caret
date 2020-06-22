modelInfo <- list(label = "Neural Network",
                  library = "neuralnet",
                  loop = NULL,
                  type = c('Regression'),
                  parameters = data.frame(parameter = c('layer1', 'layer2', 'layer3'),
                                          class = c('numeric', 'numeric', 'numeric'),
                                          label = c('#Hidden Units in Layer 1', '#Hidden Units in Layer 2', '#Hidden Units in Layer 3')),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(layer1 = ((1:len) * 2) - 1, layer2 = 0, layer3 = 0)
                    } else {
                      out <- data.frame(layer1 = sample(2:20, replace = TRUE, size = len),
                                        layer2 = sample(c(0, 2:20), replace = TRUE, size = len),
                                        layer3 = sample(c(0, 2:20), replace = TRUE, size = len))
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    colNames <- colnames(x)
                    dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
                    dat$.outcome <- y
                    form <- as.formula(paste(".outcome ~",paste(colNames, collapse = "+")))
                    if(param$layer1 == 0) stop("the first layer must have at least one hidden unit")
                    if(param$layer2 == 0 & param$layer2 > 0) stop("the second layer must have at least one hidden unit if a third layer is specified")
                    nodes <- c(param$layer1)
                    if(param$layer2 > 0) {
                      nodes <- c(nodes, param$layer2)
                      if(param$layer3 > 0) nodes <- c(nodes, param$layer3)
                    }
                    neuralnet::neuralnet(form, data = dat, hidden = nodes, ...)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    newdata <- newdata[, modelFit$model.list$variables, drop = FALSE]
                    neuralnet::compute(modelFit, covariate = newdata)$net.result[,1]
                  },
                  prob = NULL,
                  tags = c("Neural Network"),
                  sort = function(x) x[order(x$layer1, x$layer2, x$layer3),])
