modelInfo <- list(label = "Learning Vector Quantization",
                  library = "class",
                  loop = NULL,
                  type = "Classification",
                  parameters = data.frame(parameter = c("size", "k"),
                                          class = c("numeric", "numeric"),
                                          label = c('Codebook Size', '#Prototypes')),
                  grid = function(x, y, len = NULL) {
                    p <- ncol(x) 
                    ng <- length(levels(y))
                    n <- nrow(x)
                    tmp <- min(round(0.4*ng*(ng-1 + p/2),0), n)
                    out <- expand.grid(size = unique(floor(seq(tmp, 2*tmp, length = len))),
                                       k = -4 + (1:len)*5)
                    out <- subset(out, k <= size & size < n)
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) 
                    lvq3(x, y, lvqinit(x, y, size = param$size, k = param$k), ...),
                  predict = function(modelFit, newdata, submodels = NULL) 
                    lvqtest(modelFit , newdata),
                  prob = NULL,
                  tags = "Prototype Models",
                  sort = function(x) x[order(-x$k, -x$size),])
