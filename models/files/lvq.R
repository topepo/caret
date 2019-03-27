modelInfo <- list(label = "Learning Vector Quantization",
                  library = "class",
                  loop = NULL,
                  type = "Classification",
                  parameters = data.frame(parameter = c("size", "k"),
                                          class = c("numeric", "numeric"),
                                          label = c('Codebook Size', '#Prototypes')),
                  grid = function(x, y, len = NULL, search = "grid"){
                    p <- ncol(x)
                    ng <- length(levels(y))
                    n <- nrow(x)
                    tmp <- min(round(0.4*ng*(ng-1 + p/2),0), n)
                    if(search == "grid") {
                      out <- expand.grid(size = floor(seq(tmp, 2*tmp, length = len)),
                                         k = -4 + (1:len)*5)
                      out$size <- floor(out$size)
                    } else {
                      out <- data.frame(size = sample(tmp:(2*tmp), size = len, replace = TRUE),
                                        k = sample(1:(nrow(x) - 2), size = len, replace = TRUE))
                    }
                    out <- subset(out, size < n & k < n)
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    class::lvq3(x, y, class::lvqinit(x, y, size = param$size, k = min(param$k, nrow(x)-length(levels(y)))), ...)
                    },
                  predict = function(modelFit, newdata, submodels = NULL)
                    class::lvqtest(modelFit , newdata),
                  levels = function(x) x$obsLevels,
                  prob = NULL,
                  tags = "Prototype Models",
                  sort = function(x) x[order(-x$k, -x$size),])
