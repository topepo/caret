modelInfo <- list(label = "Patient Rule Induction Method",
                  library = "supervisedPRIM",
                  loop = NULL,
                  type = "Classification",
                  parameters = data.frame(parameter = c("peel.alpha", "paste.alpha", "mass.min"),
                                          class = c("numeric", "numeric", "numeric"),
                                          label = c("peeling quantile", 'pasting quantile', "minimum mass")),
                  grid = function(x, y, len = NULL, search = "grid"){
                    lowerlimit <- 1 / nrow(x)
                    if(search == "grid") {
                      out <- expand.grid(
                        peel.alpha = seq(max(lowerlimit, 0.01),
                                         0.25, length.out = len),
                        paste.alpha = seq(max(lowerlimit, 0.01),
                                          0.25, length.out = len),
                        mass.min = seq(max(lowerlimit, 0.01),
                                       0.25, length.out = len)
                        )
                      if(len == 1){
                        out <- data.frame(
                          peel.alpha = 0.05,
                          paste.alpha = 0.01,
                          mass.min = 0.05
                          )
                      }
                    } else {
                      out <- data.frame(
                        peel.alpha = runif(len,
                                           min = lowerlimit,
                                           max = 0.25),
                        paste.alpha = runif(len,
                                            min = lowerlimit,
                                            max = 0.25),
                        mass.min = runif(len, min = lowerlimit,
                                         max = 0.25)
                        )
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    if(!is.data.frame(x) | inherits(x, "tbl_df")) 
                      x <- as.data.frame(x, stringsAsFactors = TRUE)
                    out <- supervisedPRIM::supervisedPRIM(
                      x = x, y = y,
                      peel.alpha = param$peel.alpha,
                      paste.alpha = param$paste.alpha,
                      mass.min = param$mass.min,
                      ...
                      )
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL){
                    if(!is.data.frame(newdata) | inherits(newdata, "tbl_df")) 
                      newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    supervisedPRIM:::predict.supervisedPRIM(
                      modelFit, 
                      newdata = newdata, 
                      classProb = FALSE
                      )
                  },
                  prob = function(modelFit, newdata, submodels = NULL){
                    if(!is.data.frame(newdata) | inherits(newdata, "tbl_df")) 
                      newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    out <- supervisedPRIM:::predict.supervisedPRIM(
                      modelFit, 
                      newdata = newdata, 
                      classProb = TRUE
                    )
                    out <- data.frame(out, 1 - out)
                    names(out) <- modelFit$levels[1:2]
                    return(out)
                  },
                  predictors = function(x, ...) if(hasTerms(x)) predictors(x$terms) else NA,
                  tags = c("Rule-Based Model", "Patient Rule Induction Method"),
                  levels = function(x) x$lev,
                  sort = function(x) x[order(x$peel.alpha, x$paste.alpha, x$mass.min),])
