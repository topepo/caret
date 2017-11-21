modelInfo <- list(label = "Independent Component Regression",
                  library = "fastICA",
                  loop = NULL,
                  type = c('Regression'),
                  parameters = data.frame(parameter = c('n.comp'),
                                          class = c('numeric'),
                                          label = c('#Components')),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- data.frame(n.comp = 1:len)
                    } else {
                      out <- data.frame(n.comp = unique(sample(1:ncol(x), size = len, replace = TRUE)))
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    caret::icr(x, y, n.comp = param$n.comp, ...)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) predict(modelFit, newdata),
                  prob = NULL,
                  tags = c("Linear Regression", "Feature Extraction"),
                  sort = function(x) x[order(x[,1]),])
