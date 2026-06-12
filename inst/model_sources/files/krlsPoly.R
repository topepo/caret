modelInfo <- list(label ="Polynomial Kernel Regularized Least Squares",
                  library = c("KRLS"),
                  loop = NULL,
                  type = c('Regression'),
                  parameters = data.frame(parameter = c('lambda', 'degree'),
                                          class = c('numeric', 'numeric'),
                                          label = c('Regularization Parameter', 'Polynomial Degree')),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(lambda = NA, degree = 1:3)
                    } else {
                      out <- data.frame(lambda = 10^runif(len, min = -5, 0),
                                        degree = sample(1:3, size = len, replace = TRUE))
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    if(!(param$degree %in% 1:4)) stop("Degree should be either 1, 2, 3 or 4")
                    krn <- switch(param$degree,
                                  '1' = "linear",
                                  '2' = "poly2",
                                  '3' = "poly3",
                                  '4' = "poly4")
                    KRLS::krls(x, y, lambda = if(is.na(param$lambda)) NULL else param$lambda,
                               derivative = FALSE,
                               whichkernel = krn, ...)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    KRLS:::predict.krls(modelFit, newdata)$fit[,1]
                  },
                  tags = c("Kernel Method", "L2 Regularization", "Polynomial Model"),
                  prob = NULL,
                  sort = function(x) x[order(x$degree, x$lambda),])
