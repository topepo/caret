modelInfo <- list(label = "Multilayer Perceptron Network by Stochastic Gradient Descent",
                  library = "FCNN4R",
                  loop = NULL,
                  type = c('Regression'),
                  parameters = data.frame(parameter = c('size', 'l2reg', 'lambda', "learn_rate", 
                                                        "momentum", "gamma", "minibatchsz"),
                                          class = rep('numeric', 7),
                                          label = c('#Hidden Units', 'L2 Regularization', 
                                                    'RMSE Gradient Scaling', "Learning Rate", 
                                                    "Momentum", "Weight Decay", "Batch Size")),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(size = ((1:len) * 2) - 1, 
                                         l2reg = c(0, 10 ^ seq(-1, -4, length = len - 1)), 
                                         lambda = 0,
                                         learn_rate = 2e-6, 
                                         momentum = 0.9, 
                                         gamma = c(0, 10 ^ seq(-1, -4, length = len - 1)),
                                         minibatchsz = floor(nrow(x)/3))
                    } else {
                      out <- data.frame(size = sample(2:20, replace = TRUE, size = len),
                                        l2reg = 10^runif(len, min = -5, 1),
                                        lambda = runif(len, max = .4),
                                        learn_rate = runif(len),
                                        momentum = runif(len, min = .5),
                                        gamma = 10^runif(len, min = -5, 1),
                                        minibatchsz = floor(rbeta(len, 1.5,3)*nrow(x))+ 1)
                      out$minibatchsz[out$minibatchsz == nrow(x)] <- floor(nrow(x))/3
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
                    net <- mlp_rnd_weights(net)

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
                    
                    netobj2 <- do.call("mlp_teach_sgd", args)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
                    mlp_eval(modelFit$net, newdata)[,1]
                  },
                  prob =  NULL,
                  tags = c("Neural Network", "L2 Regularization"),
                  sort = function(x) x[order(x$size, -x$l2reg, -x$gamma),])
