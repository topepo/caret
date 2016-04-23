modelInfo <- list(label = "Penalized Ordinal Regression",
                  library = c("ordinalNet", "plyr"),
                  type = c("Regression", "Classification"),
                  parameters = data.frame(parameter = c('alpha', 'criteria', 'link'),
                                          class = c("numeric", "character", "character"),
                                          label = c('Mixing Percentage', 'Selection Criterion', "Link Function")),
                  grid = function(x, y, len = NULL, search = "grid") {
                    links <- c("logit", "probit", "cloglog", "cauchit")
                    if(search == "grid") {
                      out <- expand.grid(alpha = seq(0.1, 1, length = len),
                                         criteria = "aic",
                                         link = links)
                    } else {
                      out <- data.frame(alpha = runif(len, min = 0, 1),
                                        criteria = sample(c("aic", "bic"), size = len, replace = TRUE),
                                        links = sample(links, size = len, replace = TRUE))
                    }
                    out
                  },
                  #                   loop = function(grid) {
                  #                     loop <- ddply(grid, c("alpha", "link"),
                  #                                   function(x) c(criteria = as.character(x$criteria[1])))
                  #                     submodels <- vector(mode = "list", length = nrow(loop))
                  #                     for(i in seq(along = loop$criteria)) {
                  #                       index <- which(grid$link == loop$link[i] & grid$alpha == loop$alpha[i])
                  #                       trees <- grid[index, "criteria"] 
                  #                       submodels[[i]] <- data.frame(criteria = trees[trees != loop$criteria[i]])
                  #                     }    
                  #                     list(loop = loop, submodels = submodels)
                  #                   },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    if(!is.matrix(x)) x <- as.matrix(x)
                    out <- ordinalNet(x = x, y = y, alpha = param$alpha,
                                      link = as.character(param$link),
                                      ...)
                    out$.criteria <- as.character(param$criteria)
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
                    out <- modelFit$obsLevels[predict(modelFit, newdata, criteria = modelFit$.criteria)]
                    
                    #                     if(!is.null(submodels)) {
                    #                       out <- modelFit$obsLevels[predict(modelFit, newdata, criteria = as.character(submodels$criteria))]
                    #                       out <- c(list(out), tmp)
                    #                     } 
                    out
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
                    out <- predict(modelFit, newdata, criteria = modelFit$.criteria, type = "prob")
                    colnames(out) <- modelFit$obsLevels
                    out
                  },
                  predictors = function(x, lambda = NULL, ...) {
                    betas <- coef(x, criteria = x$.criteria)
                    out <- names(betas)[betas != 0]
                    out[!grepl("^Intercept", out)]
                  },
                  varImp = function(object, lambda = NULL, ...) {
                    betas <- coef(object, criteria = object$.criteria)
                    betas <- betas[!grepl("^Intercept", names(betas))]
                    out <- data.frame(Overall = abs(betas))
                    rownames(out) <- names(betas)
                    out
                  },
                  levels = function(x) if(any(names(x) == "obsLevels")) x$obsLevels else NULL,
                  tags = c("Generalized Linear Model", "Implicit Feature Selection", 
                           "L1 Regularization", "L2 Regularization", "Linear Classifier",
                           "Linear Regression"),
                  sort = function(x) x[order(x$alpha),])
