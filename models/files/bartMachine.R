modelInfo <- list(label = "Bayesian Additive Regression Trees",
                  library = "bartMachine",
                  loop = NULL,
                  type = c("Classification", "Regression"),
                  parameters = data.frame(parameter = c("num_trees", "k", "alpha", "beta", "nu"),
                                          class = rep("numeric", 5),
                                          label = c("#Trees",
                                                    "Prior Boundary",
                                                    "Base Terminal Node Hyperparameter",
                                                    "Power Terminal Node Hyperparameter",
                                                    "Degrees of Freedom")),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(num_trees = 50,
                                         k = (1:len)+ 1,
                                         alpha = seq(.9, .99, length = len),
                                         beta = seq(1, 3, length = len),
                                         nu =  (1:len)+ 1)
                    } else {
                      out <- data.frame(num_trees = sample(10:100, replace = TRUE, size = len),
                                        k = runif(len, min = 0, max = 5),
                                        alpha = runif(len, min = .9, max = 1),
                                        beta = runif(len, min = 0, max = 4),
                                        nu = runif(len, min = 0, max = 5))
                    }
                    if(is.factor(y)) {
                      out$k <- NA
                      out$nu <- NA
                    }
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    if(!is.data.frame(x) | inherits(x, "tbl_df"))
                      x <- as.data.frame(x, stringsAsFactors = TRUE)
                    out <- if(is.factor(y)) {
                      bartMachine::bartMachine(X = x, y = y,
                                               num_trees = param$num_trees,
                                               alpha = param$alpha,
                                               beta = param$beta,
                                               ...)
                    } else {
                      bartMachine::bartMachine(X = x, y = y,
                                               num_trees = param$num_trees,
                                               k = param$k,
                                               alpha = param$alpha,
                                               beta = param$beta,
                                               nu = param$nu,
                                               ...)
                    }
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata) | inherits(newdata, "tbl_df"))
                      newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    out <- if(is.factor(modelFit$y))
                      predict(modelFit, newdata, type = "class") else
                        predict(modelFit, newdata)
                    },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata) | inherits(newdata, "tbl_df"))
                      newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    out <- predict(modelFit, newdata, type = "prob")
                    out <- data.frame(y1 = 1- out, y2 = out)
                    colnames(out) <- modelFit$y_levels
                    out
                    },
                  predictors = function(x, ...)  colnames(x$X),
                  varImp = function(object, ...){
                    imps <- bartMachine::investigate_var_importance(object, plot = FALSE)
                    imps <- imps$avg_var_props - 1.96*imps$sd_var_props
                    missing_x <- !(colnames(object$X) %in% names(imps))
                    if(any(missing_x)) {
                      imps2 <- rep(0, sum(missing_x))
                      names(imps2) <- colnames(object$X)[missing_x]
                      imps <- c(imps, imps2)
                    }
                    out <- data.frame(Overall = as.vector(imps))
                    rownames(out) <- names(imps)
                    out
                  },
                  levels = function(x) x$y_levels,
                  tags = c("Tree-Based Model", "Implicit Feature Selection", "Bayesian Model", "Two Class Only"),
                  sort = function(x) x[order(-x[,"num_trees"]),])
