modelInfo <- list(label = "DeepBoost",
                  library = "deepboost",
                  loop = NULL,
                  type = 'Classification',
                  parameters = data.frame(
                    parameter = c('num_iter', "tree_depth", 'beta', 'lambda', "loss_type"),
                    class = c(rep('numeric', 4), "character"),
                    label = c('# Boosting Iterations', 'Tree Depth', 'L1 Regularization',
                              'Tree Depth Regularization', "Loss")
                    ),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(
                        tree_depth = seq(1, len),
                        num_iter = floor((1:len) * 50),
                        beta = 2^seq(-8, -4, length = len),
                        lambda = 2^seq(-6, -2, length = len),
                        loss_type = "l"
                        )
                    } else {
                      out <- data.frame(
                        num_iter = floor(runif(len, min = 1, max = 500)),
                        tree_depth = sample(1:20, replace = TRUE, size = len),         
                        beta = runif(len, max = .25),
                        lambda = runif(len, max = .25),
                        loss_type = sample(c("l"), replace = TRUE, size = len) 
                        )
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    if(!is.data.frame(x)) x <- as.data.frame(x, stringsAsFactors = TRUE)
                    if(is.null(wts)) {
                      dat <- x
                      dat$.outcome <- y
                      out <- deepboost::deepboost(
                        .outcome ~ ., data = dat,
                        tree_depth = param$tree_depth,
                        num_iter = param$num_iter,
                        beta = param$beta,
                        lambda = param$lambda,
                        loss_type = as.character(param$loss_type), 
                        ...
                        )
                    } else {
                      out <- deepboost::deepboost(
                        .outcome ~ ., data = dat,
                        tree_depth = param$tree_depth,
                        instance_weights = wts,
                        num_iter = param$num_iter,
                        beta = param$beta,
                        lambda = param$lambda,
                        loss_type = as.character(param$loss_type), 
                        ...
                      )             
                    }
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) 
                      newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    deepboost:::predict(modelFit, newdata)
                  },
                  levels = function(x) x@classes,
                  prob =  NULL,
                  tags = c("Tree-Based Model", "Boosting", "Ensemble Model", "Implicit Feature Selection", 
                           "Accepts Case Weights", "L1 Regularization", "Two Class Only"),
                  sort = function(x) x[order(x$num_iter, x$tree_depth, x$beta),] )
