modelInfo <- list(label = "Stacked AutoEncoder Deep Neural Network",
                  library = "deepnet",
                  loop = NULL,
                  type = c("Classification", "Regression"),
                  parameters = data.frame(parameter = c("layer1", "layer2", "layer3", "hidden_dropout", "visible_dropout"),
                                          class = rep("numeric", 5),
                                          label = c("Hidden Layer 1", "Hidden Layer 2", "Hidden Layer 3", 
                                                    "Hidden Dropouts", "Visible Dropout")),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(layer1 = 1:len, layer2 = 0:(len -1), layer3 = 0:(len -1),
                                         hidden_dropout = seq(0, .7, length = len), 
                                         visible_dropout = seq(0, .7, length = len))
                    } else {
                      out <- data.frame(layer1 = sample(2:20, replace = TRUE, size = len),
                                        layer2 = sample(2:20, replace = TRUE, size = len),
                                        layer3 = sample(2:20, replace = TRUE, size = len),
                                        hidden_dropout = runif(len, min = 0, max = .7),
                                        visible_dropout = runif(len, min = 0, max = .7))
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    if(!is.matrix(x)) x <- as.matrix(x)
                    is_class <- is.factor(y)
                    if (is_class) y <- caret:::class2ind(y)
                    layers <- c(param$layer1, param$layer2, param$layer3)
                    layers <- layers[layers > 0]
                    deepnet::sae.dnn.train(x, y, hidden = layers,
                                           output = if(is_class) "sigm" else "linear",
                                           hidden_dropout = param$hidden_dropout,
                                           visible_dropout = param$visible_dropout,
                                           ...)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    pred <- deepnet::nn.predict(modelFit, as.matrix(newdata))
                    if(ncol(pred) > 1)
                      pred <- modelFit$obsLevels[apply(pred, 1, which.max)]
                    pred
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    out <- exp(deepnet::nn.predict(modelFit, as.matrix(newdata)))
                    out <- apply(out, 1, function(x) x/sum(x))
                    t(out)
                  },
                  predictors = function(x, ...) {
                    NULL
                  },
                  varImp = NULL,
                  levels = function(x) x$classes,
                  tags = c("Neural Network"),
                  sort = function(x) x[order(x[,1]),])
