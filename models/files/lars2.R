modelInfo <- list(label = "Least Angle Regression",
                  library = "lars",
                  type = "Regression",
                  parameters = data.frame(parameter = 'step',
                                          class = "numeric",
                                          label = '#Steps'),
                  grid = function(x, y, len = NULL) {
                    p <- ncol(x) 
                    if(p <= len)
                    { 
                      tuneSeq <- floor(seq(2, to = p, length = p))
                    } else {
                      if(p < 500 ) tuneSeq <- floor(seq(2, to = p, length = len))
                      else tuneSeq <- floor(2^seq(1, to = log(p, base = 2), length = len))
                    }
                    if(any(table(tuneSeq) > 1))
                    {
                      tuneSeq <- unique(tuneSeq)
                      cat("note: only",
                          length(tuneSeq),
                          "unique complexity parameters in default grid.",
                          "Truncating the grid to",
                          length(tuneSeq), ".\n\n")      
                    }
                    data.frame(step = tuneSeq)
                  },
                  loop = function(grid) {   
                    grid <- grid[order(grid$step, decreasing = TRUE),, drop = FALSE]
                    loop <- grid[1,,drop = FALSE]
                    submodels <- list(grid[-1,,drop = FALSE])     
                    list(loop = loop, submodels = submodels)
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) 
                    lars(as.matrix(x), y, ...),
                  predict = function(modelFit, newdata, submodels = NULL) {
                    out <- predict(modelFit,
                                   as.matrix(newdata),
                                   type = "fit",
                                   mode = "step",
                                   s = modelFit$tuneValue$step)$fit
                    
                    if(!is.null(submodels))
                    {
                      tmp <- vector(mode = "list", length = nrow(submodels) + 1)
                      tmp[[1]] <- out
                      
                      for(j in seq(along = submodels$step))
                      {
                        tmp[[j+1]] <- predict(modelFit,
                                              as.matrix(newdata),
                                              type = "fit",
                                              mode = "step",
                                              s = submodels$step[j])$fit
                      }
                      out <- tmp
                    }
                    out       
                  },
                  predictors = function(x, s = NULL, ...) {
                    if(is.null(s))
                    {
                      if(!is.null(x$tuneValue))
                      {
                        s <- x$tuneValue$.fraction
                      } else stop("must supply a vaue of s")
                      out <- predict(x, s = s,
                                     type = "coefficients",
                                     mode = "fraction")$coefficients
                      
                    } else {
                      out <- predict(x, s = s, ...)$coefficients
                      
                    }
                    names(out)[out != 0]
                  },
                  tags = c("Linear Regression", "Implicit Feature Selection", "L1 Regularization"),
                  prob = NULL,
                  sort = function(x) x[order(x[,1]),])
