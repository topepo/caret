modelInfo <- list(label = "Random Forest Rule-Based Model",
                  library = c("randomForest", "inTrees", "plyr"),
                  type = c("Classification", "Regression"),
                  parameters = data.frame(parameter = c("mtry","maxdepth"),
                                          class = rep("numeric",2),
                                          label = c("#Randomly Selected Predictors","Maximum Rule Depth")),
                  grid = function(x, y, len = NULL, search = "grid"){
                    if(search == "grid") {
                      out <- data.frame(mtry = caret::var_seq(p = ncol(x), 
                                                              classification = is.factor(y), 
                                                              len = len), 
                                        maxdepth = (1:len)+1)
                    } else {
                      out <- data.frame(mtry = sample(1:ncol(x), size = len, replace = TRUE),
                                        maxdepth = sample(1:15, size = len, replace = TRUE))
                    }
                    out[!duplicated(out),]
                  },
                  loop = function(grid) {   
                    loop <- ddply(grid, c("mtry"),
                                  function(x) c(maxdepth = max(x$maxdepth)))
                    submodels <- vector(mode = "list", length = nrow(loop))
                    for(i in seq(along = loop$maxdepth)) {
                      index <- which(grid$mtry == loop$mtry[i])
                      trees <- grid[index, "maxdepth"] 
                      submodels[[i]] <- data.frame(maxdepth = trees[trees != loop$maxdepth[i]])
                    }    
                    list(loop = loop, submodels = submodels)
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    RFor <- randomForest(x, y, mtry = param$mtry, ...)
                    treeList <- RF2List(RFor)  
                    exec <- extractRules(treeList,x, maxdepth=param$maxdepth, ntree = RFor$ntree) 
                    ruleMetric <- getRuleMetric(exec,x,y) 
                    ruleMetric <- pruneRule(ruleMetric,x,y) 
                    ruleMetric <- selectRuleRRF(ruleMetric,x,y) 
                    out <- list(model = buildLearner(ruleMetric,x,y)) 
                    if(!last) {
                      out$rf <- treeList
                      out$x <- x
                      out$y <- y
                      out$trees <- RFor$ntree
                    }
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    out <- applyLearner(modelFit$model, newdata)
                    if(modelFit$problemType == "Regression") out <- as.numeric(out)
                    if(!is.null(submodels)) {
                      tmp <- vector(mode = "list", length = nrow(submodels) + 1)
                      tmp[[1]] <- if(is.matrix(out)) out[,1] else out
                      for(i in seq(along = submodels$maxdepth)) {
                        exec <- extractRules(modelFit$rf,
                                             modelFit$x, 
                                             maxdepth=submodels$maxdepth[i], 
                                             ntree = modelFit$trees) 
                        ruleMetric <- getRuleMetric(exec,modelFit$x,modelFit$y) 
                        ruleMetric <- pruneRule(ruleMetric,modelFit$x,modelFit$y) 
                        ruleMetric <- selectRuleRRF(ruleMetric,modelFit$x,modelFit$y) 
                        mod <- buildLearner(ruleMetric,modelFit$x,modelFit$y)
                        tmp[[i+1]] <- applyLearner(mod, newdata)
                        if(modelFit$problemType == "Regression") tmp[[i+1]] <- as.numeric(tmp[[i+1]])
                      }
                      out <- tmp
                    }
                    out
                  },
                  prob = NULL,
                  predictors = function(x, ...) {
                    split_up <- strsplit(x$model[,"condition"], "&")
                    
                    isolate <- function(x) {
                      index <- gregexpr("]", x, fixed = TRUE)
                      out <- NULL
                      for(i in seq_along(index)) {
                        if(all(index[[i]] > 0)) {
                          tmp <- substring(x[i], 1, index[[i]][1])
                          tmp <- gsub("(X)|(\\[)|(\\])|(,)|( )", "", tmp)
                          tmp <- tmp[tmp != ""]
                          out <- c(out, as.numeric(tmp))
                        }
                      }
                      as.numeric(unique(out))
                    }
                    
                    var_index <- unique(unlist(lapply(split_up, isolate)))
                    if(length(var_index) > 0) x$xNames[var_index] else NULL
                  },
                  varImp = function(object, ...) {
                    split_up <- strsplit(object$model[,"condition"], "&")
                    
                    isolate <- function(x) {
                      index <- gregexpr("]", x, fixed = TRUE)
                      out <- NULL
                      for(i in seq_along(index)) {
                        if(all(index[[i]] > 0)) {
                          tmp <- substring(x[i], 1, index[[i]][1])
                          tmp <- gsub("(X)|(\\[)|(\\])|(,)|( )", "", tmp)
                          tmp <- tmp[tmp != ""]
                          out <- c(out, as.numeric(tmp))
                        }
                      }
                      as.numeric(unique(out))
                    }
                    
                    var_index <- lapply(split_up, isolate)
                    
                    vars_dat <- lapply(var_index, 
                                       function(x, p) {
                                         out <- rep(0, p)
                                         if(length(x) > 0) out[x] <- 1
                                         out
                                       },
                                       p = length(object$xNames))
                    vars_dat <- do.call("rbind", vars_dat)
                    colnames(vars_dat) <- object$xNames
                    freqs <- as.numeric(object$model[,"freq"])
                    vars_dat <- vars_dat * freqs
                    var_imp <- apply(vars_dat, 2, sum)
                    out <- data.frame(Overall = as.vector(var_imp))
                    rownames(out) <- names(var_imp)
                    out
                  },
                  levels = function(x) x$obsLevels,
                  tags = c("Random Forest", "Ensemble Model", "Bagging", 
                           "Implicit Feature Selection", "Rule-Based Model"),
                  sort = function(x) x[order(x[,"maxdepth"]),])