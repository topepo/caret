modelInfo <- list(label = "Robust Linear Model",
                  library = "MASS",
                  loop = NULL,
                  type = "Regression",
                  parameters = data.frame(parameter = c("intercept", "psi"),
                                          class = c("logical", "character"),
                                          label = c("intercept", "psi")),
                  grid = function(x, y, len = NULL, search = "grid") 
                    expand.grid(intercept = c(TRUE, FALSE),
                                psi = c("psi.huber", "psi.hampel", "psi.bisquare")),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
                    dat$.outcome <- y

                    psi <- MASS::psi.huber # default
                    if (param$psi == "psi.bisquare")
                      psi <- MASS::psi.bisquare else
                    if (param$psi == "psi.hampel")
                      psi <- MASS::psi.hampel

                    if(!is.null(wts))
                    {
                      if (param$intercept)
                        out <- MASS::rlm(.outcome ~ ., data = dat, weights = wts, psi = psi, ...)
                      else
                        out <- MASS::rlm(.outcome ~ 0 + ., data = dat, weights = wts, psi = psi,  ...)
                    } else
                    {
                      if (param$intercept)
                        out <- MASS::rlm(.outcome ~ ., data = dat, psi = psi,...)
                      else
                        out <- MASS::rlm(.outcome ~ 0 + ., data = dat, psi = psi, ...)
                    }
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    predict(modelFit, newdata)
                  },
                  prob = NULL,
                  varImp = function(object, ...) {
                    values <- summary(object)$coef
                    varImps <-  abs(values[ !grepl( rownames(values), pattern = 'Intercept' ), 
                                             grep("value$", colnames(values)), drop = FALSE])
                    out <- data.frame(varImps)
                    colnames(out) <- "Overall"
                    if(!is.null(names(varImps))) rownames(out) <- names(varImps)
                    out   
                  },
                  tags = c("Linear Regression", "Robust Model", "Accepts Case Weights"),
                  sort = function(x) x)
