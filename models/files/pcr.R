modelInfo <- list(label = "Principal Component Analysis",
                  library = "pls",
                  type = "Regression",
                  parameters = data.frame(parameter = 'ncomp',
                                          class = "numeric",
                                          label = '#Components'),
                  grid = function(x, y, len = NULL, search = "grid")  {
                    if(search == "grid") {
                      out <- data.frame(ncomp = seq(1, min(ncol(x) - 1, len), by = 1))
                    } else {
                      out <- data.frame(ncomp = unique(sample(1:(ncol(x)-1), size = len, replace = TRUE)))
                    }
                    out
                  },
                  loop = function(grid) {
                    grid <- grid[order(grid$ncomp, decreasing = TRUE),, drop = FALSE]
                    loop <- grid[1,,drop = FALSE]
                    submodels <- list(grid[-1,,drop = FALSE])
                    list(loop = loop, submodels = submodels)
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    ncomp <- min(ncol(x), param$ncomp)
                    dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
                    dat$.outcome <- y
                    pls::pcr(.outcome ~ ., data = dat, ncomp = ncomp, ...)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    out <- as.vector(pls:::predict.mvr(modelFit, newdata, ncomp = max(modelFit$ncomp)))

                    if(!is.null(submodels))
                    {
                      tmp <- apply(predict(modelFit, newdata, ncomp = submodels$ncomp), 3, function(x) list(x))
                      tmp <-  as.data.frame(tmp, stringsAsFactors = TRUE)
                      out <- c(list(out), as.list(tmp))
                    }
                    out
                  },
                  predictors = function(x, ...) rownames(x$projection),
                  tags = c("Linear Regression", "Feature Extraction"),
                  prob = NULL,
                  sort = function(x) x[order(x[,1]),])
