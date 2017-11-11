modelInfo <- list(label = "Robust Quadratic Discriminant Analysis",
                  library = "rrcov",
                  loop = NULL,
                  type = "Classification",
                  parameters = data.frame(parameter = "parameter",
                                          class = "character",
                                          label = "parameter"),
                  grid = function(x, y, len = NULL, search = "grid") data.frame(parameter = "none"),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) 
                    rrcov:::QdaCov(x, y, ...),
                  predict = function(modelFit, newdata, submodels = NULL) 
                    rrcov::predict(modelFit, newdata)@classification,
                  prob = function(modelFit, newdata, submodels = NULL) {
                    probs <- rrcov::predict(modelFit, newdata)@posterior
                    colnames(probs) <- names(modelFit@prior)
                    probs
                  },
                  tags = c("Discriminant Analysis", "Polynomial Model"),
                  levels = function(x) names(x@prior),
                  sort = function(x) x)
