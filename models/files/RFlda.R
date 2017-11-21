modelInfo <- list(label = "Factor-Based Linear Discriminant Analysis",
                  library = "HiDimDA",
                  loop = NULL,
                  type = c('Classification'),
                  parameters = data.frame(parameter = c('q'),
                                          class = c('numeric'),
                                          label = c('# Factors')),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- data.frame(q = 1:len)
                    } else {
                      out <- data.frame(q = unique(sample(1:10, size = len, replace = TRUE)))
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) 
                    HiDimDA::RFlda(x, y, q = param$q, maxq = param$q, ...),
                  predict = function(modelFit, newdata, submodels = NULL) {
                    out <- predict(modelFit, newdata)$class
                    out <- modelFit$obsLevels[as.numeric(out)]
                    out
                  },
                  levels = function(x) x$obsLevels,
                  prob = NULL,
                  tags = c("Discriminant Analysis", "Linear Classifier"),
                  sort = function(x) x[order(x[,1]),])
