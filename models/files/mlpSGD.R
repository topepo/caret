modelInfo <- list(label = "Multilayer Perceptron Network by Stochastic Gradient Descent",
                  library = "FCNN4R",
                  loop = NULL,
                  type = c('Regression'),
                  parameters = data.frame(parameter = c('size', 'l2reg', 'lambda', "learn_rate", 
                                                        "momentum", "gamma", "minibatchsz", "repeats"),
                                          class = rep('numeric', 8),
                                          label = c('#Hidden Units', 'L2 Regularization', 
                                                    'RMSE Gradient Scaling', "Learning Rate", 
                                                    "Momentum", "Decay", "Batch Size",
                                                    "#Models")),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(size = ((1:len) * 2) - 1, 
                                         l2reg = c(0, 10 ^ seq(-1, -4, length = len - 1)), 
                                         lambda = 0,
                                         learn_rate = 2e-6, 
                                         momentum = 0.9, 
                                         gamma = 10 ^ seq(-3, -1, length = len - 1),
                                         minibatchsz = floor(nrow(x)/3),
                                         repeats = 1)
                    } else {
                      out <- data.frame(size = sample(2:20, replace = TRUE, size = len),
                                        l2reg = 10^runif(len, min = -5, 1),
                                        lambda = runif(len, max = .4),
                                        learn_rate = runif(len),
                                        momentum = runif(len, min = .5),
                                        gamma = 10^runif(len, min = -3, 1),
                                        minibatchsz = sample(1:(floor(100*2/3)+ 1), 
                                                             replace = TRUE, size = len),
                                        repeats = sample(1:10, replace = TRUE, size = len))
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    if(!is.matrix(x)) x <- as.matrix(x)
                    y <- matrix(y, ncol = 1)
                    obj <- function(net) return(mlp_mse(net, inp, outp))
                    net <- mlp_net(c(ncol(x), param$size, 1))
                    net <- mlp_set_activation(net, layer = "h", activation = "sigmoid")
                    net <- mlp_set_activation(net, layer = "o", activation = "linear")
                    
                    args <- list(net = net, 
                                 input = x, output = y, 
                                 learn_rate = param$learn_rate,
                                 minibatchsz = param$minibatchsz,
                                 l2reg = param$l2reg,
                                 lambda = param$lambda,
                                 gamma = param$gamma,
                                 momentum = param$momentum)
                    the_dots <- list(...) 
                    if(!any(names(the_dots) == "tol_level")) 
                      args$tol_level <- sd(y[,1])/sqrt(nrow(y))
                    if(!any(names(the_dots) == "max_epochs")) 
                      args$max_epochs <- 1000
                    args <- c(args, the_dots)
                    out <- list(models = vector(mode = "list", length = param$repeats))
                    for(i in 1:param$repeats) {
                      args$net <- mlp_rnd_weights(args$net)
                      out$models[[i]] <- do.call("mlp_teach_sgd", args)
                    }
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
                    out <- lapply(modelFit$models, 
                                  function(obj, newdata)
                                    mlp_eval(obj$net, input = newdata), 
                                  newdata = newdata)
                    out <- if(length(out) == 1) 
                      out[[1]][,1]  else {
                        out <- do.call("rbind", out)
                        out <- apply(out, 1, mean)
                      }
                    out
                  },
                  prob =  NULL,
                  tags = c("Neural Network", "L2 Regularization"),
                  sort = function(x) x[order(x$size, -x$l2reg, -x$gamma),])
