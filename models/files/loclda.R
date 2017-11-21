modelInfo <- list(label = "Localized Linear Discriminant Analysis",
                  library = "klaR",
                  loop = NULL,
                  type = "Classification",
                  parameters = data.frame(parameter = "k",
                                          class = "numeric",
                                          label = "#Nearest Neighbors"),
                  grid = function(x, y, len = NULL, search = "grid") {
                    min_p <- ncol(x)/nrow(x) + .05
                    p_seq <- seq(min_p , min(.9, min_p + 1/3), length = len)
                    if(search == "grid") {
                      out <- data.frame(k = floor(p_seq*nrow(x)))
                    } else {
                      by_val <- if(is.factor(y)) length(levels(y)) else 1
                      out <- data.frame(k = floor(runif(len, min = nrow(x)*min_p, max = nrow(x)*min(.9, min_p + 1/3))))
                    }
                    out
                    },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...)
                    klaR::loclda(x, y, k = floor(param$k), ...)  ,
                  predict = function(modelFit, newdata, submodels = NULL)
                    predict(modelFit, newdata)$class,
                  prob = function(modelFit, newdata, submodels = NULL) 
                    predict(modelFit, newdata)$posterior,
                  predictors = function(x, ...) if(hasTerms(x)) predictors(x$terms) else colnames(x$means),
                  tags = c("Discriminant Analysis", "Linear Classifier"),
                  levels = function(x) names(x$prior),
                  sort = function(x) x)
