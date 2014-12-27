ga_internal_names <- c('Iter','Size','Similarity','Similarity_M','Resample')
ga_external_names <- c('Iter','Resample')

check_ga_pop <- function(x) {
  no_vars <- apply(x, 1, sum) == 0
  if(any(no_vars)) {
    for(i in which(no_vars)) {
      here <- sample(1:ncol(x), 1)
      x[i,here] <- 1 
    }
  }
  x
}

ga_func_check <- function(x) {
  fnames <- names(x) 
  required <- c('fit', 'fitness_intern', 'pred', 'fitness_extern', 
                'initial', 'selection', 'crossover', 'mutation', 
                'selectIter')
  missing <- !(required %in% fnames)
  if(any(missing)) 
    stop(paste("The following functions are missing from the 'func' argument:",
               paste(required[missing], sep = "", collapse = ",")))
  invisible(x)
  args <- lapply(x, function(x) names(formals(x)))
  expected <- list(fit = c('x', 'y', 'lev', 'last', '...'),                   
                   fitness_intern = c('object', 'x', 'y', 'maximize', 'p'),
                   pred = c('object', 'x'),
                   fitness_extern = c('data', 'lev', 'model'),
                   initial = c('vars', 'popSize', '...'),
                   selection = c('population', 'fitness', 'r', 'q', '...'),
                   crossover = c('population', 'fitness', 'parents', '...'),
                   mutation = c('population', 'parent', '...'),
                   selectIter = c('x', 'metric', 'maximize'))
  
  for(i in names(x)) {
    .args <- names(formals(x[[i]]))
    .check <- same_args(.args, expected[[i]])
    if(!.check) {
      stop(paste("Arguments to function", i, "should be {",
                 paste(expected[[i]], sep = "", collapse = ", "),
                 "}  and these were given {",
                 paste(.args, sep = "", collapse = ", "), "}\n"))
    }
  }
}

gafs_initial <- function (vars, popSize, ...)  {
  x <- matrix(NA, nrow = popSize, ncol = vars)
  probs <- seq(.9, .1, length = popSize)
  for(i in 1:popSize){
    x[i,] <- sample(0:1, replace = TRUE, 
                    size = vars, 
                    prob = c(probs[i], 1-probs[i]))
  }
  var_count <- apply(x, 1, sum)
  if(any(var_count == 0)) {
    for(i in which(var_count == 0)) {
      x[i, ] <- sample(0:1, replace = TRUE, size = vars)
    }
  }
  x
}

gafs_lrSelection <-  function (population, fitness, 
                               r = NULL, 
                               q = NULL, ...) {
  popSize = nrow(population)
  
  if(is.null(r)) r <- 2/(popSize * (popSize - 1))
  if(is.null(q)) q <- 2/popSize
  rank <- (popSize + 1) - rank(fitness, ties.method = "random")
  prob <- q - (rank - 1) * r
  sel <- sample(1:popSize, 
                size = popSize, 
                prob = pmin(pmax(0, prob), 1, na.rm = TRUE), 
                replace = TRUE)
  out <- list(population = population[sel, , drop = FALSE], 
              fitness = fitness[sel])
  out
}

gafs_spCrossover <- function (population, fitness, parents, ...)  {
  fitness <- fitness[parents]
  parents <- population[parents, , drop = FALSE]
  n <- ncol(parents)
  children <- matrix(as.double(NA), nrow = 2, ncol = n)
  fitnessChildren <- rep(NA, 2)
  crossOverPoint <- sample(0:n, size = 1)
  if (crossOverPoint == 0) {
    children[1:2, ] <- parents[2:1, ]
    fitnessChildren[1:2] <- fitness[2:1]
  }
  else if (crossOverPoint == n) {
    children <- parents
    fitnessChildren <- fitness
  }
  else {
    children[1, ] <- c(parents[1, 1:crossOverPoint], parents[2, (crossOverPoint + 1):n])
    children[2, ] <- c(parents[2, 1:crossOverPoint], parents[1, (crossOverPoint + 1):n])
  }
  out <- list(children = children, fitness = fitnessChildren)
  out
}

gafs_raMutation <- function (population, parent, ...)  {
  mutate <- parent <- as.vector(population[parent, ])
  n <- length(parent)
  j <- sample(1:n, size = 1)
  mutate[j] <- abs(mutate[j] - 1)
  mutate
}

gafs_nlrSelection <- function (population, fitness, q = 0.25, ...) {
  popSize <- nrow(population)
  rank <- (popSize + 1) - rank(fitness, ties.method = "random")
  prob <- q * (1 - q)^(rank - 1)
  sel <- sample(1:popSize, size = popSize, 
                prob = pmin(pmax(0, prob), 1, na.rm = TRUE), replace = TRUE)
  out <- list(population = population[sel, , drop = FALSE], 
              fitness = fitness[sel])
  return(out)
}

gafs_rwSelection <- function (population, fitness, ...) {
  popSize <- nrow(population)
  prob <- abs(fitness)/sum(abs(fitness))
  sel <- sample(1:popSize, size = popSize, 
                prob = pmin(pmax(0, prob), 1, na.rm = TRUE), replace = TRUE)
  out <- list(population = population[sel, , drop = FALSE], 
              fitness = fitness[sel])
  return(out)
}

gafs_tourSelection <- function (population, fitness, k = 3, ...) {
  popSize <- nrow(population)
  sel <- rep(NA, popSize)
  for (i in 1:popSize) {
    s <- sample(1:popSize, size = k)
    sel[i] <- s[which.max(fitness[s])]
  }
  out <- list(population = population[sel, , drop = FALSE], 
              fitness = fitness[sel])
  return(out)
}

gafs_uCrossover <- function (population, parents, ...) {
  parents <- population[parents, , drop = FALSE]
  n <- ncol(parents)
  u <- runif(n)
  children <- parents
  children[1:2, u > 0.5] <- children[2:1, u > 0.5]
  out <- list(children = children, fitness = rep(NA, 2))
  return(out)
}

###################################################################
##


gafsControl <- function(functions = NULL,
                        method = "repeatedcv",
                        metric = NULL,
                        maximize = NULL,
                        number = ifelse(grepl("cv", method), 10, 25),
                        repeats = ifelse(grepl("cv", method), 1, 5),
                        verbose = FALSE,
                        returnResamp = "final",
                        p = .75,
                        index = NULL,
                        indexOut = NULL,
                        seeds = NULL,
                        holdout = 0,
                        genParallel = FALSE,
                        allowParallel = TRUE) {
  if(!(method %in% c("cv", "boot", "repeatedcv", "LGOCV", "LOOCV")))
    stop('method should be one of: "cv", "boot", "repeatedcv", "LGOCV" or "LOOCV"')
  if(holdout < 0 | holdout >= 1) stop("'holdout' should be in [0, 1)")
  
  if(!is.null(metric)) {
    if(length(metric)  != 2)
      stop("'metric' should be a two-element named vector. See ?gafsControl")
    if(is.null(names(metric)) || any(sort(names(metric)) != c("external", "internal")))
      stop("'metric' should have names 'internal' and 'external' See ?gafsControl")
  }
  if(!is.null(maximize)) {
    if(length(maximize)  != 2)
      stop("'maximize' should be a two-element named vector. See ?gafsControl")
    if(is.null(names(maximize)) || any(sort(names(maximize)) != c("external", "internal")))
      stop("'maximize' should have names 'internal' and 'external' See ?gafsControl")
  }  
  
  list(functions = if(is.null(functions)) caretFuncs else functions,
       method = method,
       metric = metric,
       maximize = maximize,
       number = number,
       repeats = repeats,
       returnResamp = returnResamp,
       verbose = verbose,
       p = p,
       index = index,
       indexOut = indexOut,
       seeds = seeds,
       holdout = holdout,
       genParallel = genParallel,
       allowParallel = allowParallel)
}

###################################################################
##

ga_wrapper <- function(ind, x, y, funcs, holdoutX, holdoutY, testX, testY, 
                       ga_metric, ga_maximize, lvl = lvl, last = FALSE, indiv = 0, ...) {
  mod <- funcs$fit(x[, ind, drop=FALSE], y, lev = lvl, last = last,...)
  internal <- funcs$fitness_intern(mod, 
                                   x = if(!is.null(holdoutX)) holdoutX[, ind, drop=FALSE] else x[, ind, drop=FALSE], 
                                   y = if(!is.null(holdoutY)) holdoutY else y, 
                                   p = ncol(x))
  if(!is.null(testX)) {
    modelPred <- funcs$pred(mod, testX[, ind, drop=FALSE])
    if(is.data.frame(modelPred) | is.matrix(modelPred)) {
      if(is.matrix(modelPred)) modelPred <- as.data.frame(modelPred)
      modelPred$obs <- testY
      modelPred$Size <- length(ind)
    } else modelPred <- data.frame(pred = modelPred, obs = testY, Size = sum(ind == 1))
    external <- funcs$fitness_extern(modelPred, lev = levels(testY))  
    if(is.null(names(external))) {
      names(external) <- paste0("external", 1:length(external))
    }
  } else external <- NULL
  
  if(!ga_maximize["internal"]) internal[ga_metric["internal"]] <- -internal[ga_metric["internal"]]
  
  list(internal = c(internal, .indiv = indiv),
       external = c(external, .indiv = indiv))
}

###################################################################
##


ga_select <- function(x, y,  
                      
                      testX = NULL, testY = NULL, 
                      
                      iters = 20,
                      funcs = NULL, 
                      ga_metric = NULL,
                      ga_maximize = TRUE, 
                      ga_verbose = TRUE, 
                      
                      holdout = 0,
                      ga_seed = NULL,
                      lvl = NULL,
                      
                      popSize = 50, 
                      pcrossover = 0.8, 
                      pmutation = 0.1, 
                      elite = base::max(1, round(popSize*0.05)), 
                      maxfitness = Inf,
                      suggestions = NULL, 
                      genParallel = FALSE,
                      Resample = "",
                      ...) {
  ga_func_check(funcs)
  nvars <- ncol(x)
  if(!is.null(ga_seed)) set.seed(ga_seed[1])
  dig <- options()$digits
  
  if(holdout > 0) {
    in_holdout <- createDataPartition(y, 
                                      p = holdout, 
                                      list = FALSE)
    holdout_x <- x[in_holdout,,drop = FALSE]
    holdout_y <- y[in_holdout]
    x <- x[-in_holdout,,drop = FALSE]
    y <- y[-in_holdout]    
  } else {
    holdout_x <- NULL
    holdout_y <- NULL
  }  
  
  ###################################################################
  ##
  
  subsets <- vector(mode = "list", length = iters)
  internal <- data.frame(Iter = 1:(iters),
                         Size = rep(0*NA, iters),
                         Similarity = rep(0*NA, iters),
                         Similarity_M= rep(0*NA, iters),
                         stringsAsFactors = FALSE)
  external <- if(!is.null(testX)) data.frame(Iter = 1:(iters)) else NULL
  
  ## add GA package warnings
  
  ###################################################################
  ## From GA package:
  
  ## TODO make input a vector of indicies
  
  if(is.null(suggestions)) { 
    suggestions <- matrix(nrow = 0, ncol = nvars) 
  } else { 
    if(is.vector(suggestions)) { 
      if(nvars > 1) suggestions <- matrix(suggestions, nrow = 1)
      else suggestions <- matrix(suggestions, ncol = 1) 
    } else suggestions <- as.matrix(suggestions) 
    if(nvars != ncol(suggestions))
      stop("Provided suggestions (ncol) matrix do not match number of variables of the problem!")
  }
  
  ###################################################################
  ## From GA package:
  
  Pop <- matrix(as.double(NA), nrow = popSize, ncol = nvars)
  ng <- min(nrow(suggestions), popSize)
  if(ng > 0)  { # use suggestion if provided
    Pop[1:ng,] <- suggestions 
  }
  # fill the rest with a random population
  if(popSize > ng) { 
    Pop[(ng+1):popSize,] <- funcs$initial(vars = nvars, popSize = popSize)[1:(popSize-ng),] 
  }
  colnames(Pop) <- colnames(x)
  
  .Pop <- Pop
  .Fit <- rep(NA, nrow(Pop))
  
  ###################################################################
  ##
  
  `%op%` <- getOper(genParallel && getDoParWorkers() > 1)
  
  for(generation in 1:iters) {
    Pop <- check_ga_pop(Pop)
    currennt_results <- foreach(i = seq_len(popSize), 
                                .combine = "c", 
                                .verbose = FALSE, 
                                .errorhandling = "stop") %op% { 
                                  ga_wrapper(ind = which(Pop[i,] == 1), 
                                             x = x, y = y, 
                                             funcs, 
                                             holdoutX = holdout_x, holdoutY = holdout_y,
                                             testX = testX, testY = testY, 
                                             ga_metric = ga_metric,
                                             ga_maximize = ga_maximize, 
                                             lvl = lvl,
                                             last = Resample == "",
                                             indiv = i,
                                             ...) 
                                } ## loop over chromosomes
    
    ## TODO save only the parts you need inside of loop
    if(!is.null(testX)) {
      current_ext <- currennt_results[names(currennt_results) == "external"]
      current_ext <- do.call("rbind", current_ext)
      current_ext <- current_ext[order(current_ext[,".indiv"]),]
      current_ext <- current_ext[, -ncol(current_ext), drop = FALSE]
      rownames(current_ext) <- NULL
    } else current_ext <- NULL
    current_int <- currennt_results[names(currennt_results) == "internal"]
    current_int <- do.call("rbind", current_int)
    current_int <- current_int[order(current_int[,".indiv"]),]
    current_int <- current_int[, -ncol(current_int), drop = FALSE]
    rownames(current_int) <- NULL
    rm(currennt_results)
    
    Fitness <- if(is.matrix(current_int)) current_int[,ga_metric["internal"]]
    best_index <- which.max(Fitness)
    best_internal <- current_int[best_index,]
    if(!is.null(testX)) best_external <- current_ext[best_index,]
    subsets[[generation]] <- which(Pop[best_index,] == 1)
    internal$Size[generation] <- sum(Pop[best_index,] == 1)
    
    if(generation > 1) {
      hist_best <- which.max(internal[1:(generation-1), ga_metric["internal"]])
      internal$Similarity[generation] <- jack_sim(index2vec(subsets[[hist_best]], ncol(Pop)), 
                                                  index2vec(subsets[[generation]], ncol(Pop)))
      tmp_sim <- apply(Pop, 1, function(x, y) jack_sim(x, y),
                       y = index2vec(subsets[[hist_best]], ncol(Pop))) 
      
      internal$Similarity_M[generation] <- mean(tmp_sim, na.rm = TRUE)
    }
    
    .Pop <- Pop
    .Fit <- Fitness
    
    if(generation == 1) {
      k <- length(best_internal)
      perf_names <- names(best_internal)
      for(new_var in perf_names) internal[,new_var] <- NA
      nr <- ncol(internal)
      internal[1, (nr-k+1):nr] <- best_internal
      if(!is.null(testX)) {
        for(new_var in names(best_external)) external[,new_var] <- NA
        external[1, -1] <- best_external
      }
    } else {
      internal[generation, (nr-k+1):nr] <- best_internal
      if(!is.null(testX)) external[generation, -1] <- best_external
    }
    
    if(ga_verbose){
      if(generation > 1) {
        imp <- internal[hist_best, ga_metric["internal"]] < max(Fitness)
        cat(Resample, " ", format(1:iters)[generation], " ",
            if(ga_maximize["internal"]) 
              signif( internal[hist_best, ga_metric["internal"]], digits = dig) else 
                signif(-internal[hist_best, ga_metric["internal"]], digits = dig),
            "->" , 
            if(ga_maximize["internal"]) 
              signif(max(Fitness), digits = dig) else 
                signif( min(-Fitness), digits = dig), 
            change_text(subsets[[hist_best]], subsets[[generation]], nvars, show_diff = FALSE), 
            if(imp) " *" else "",
            "\n",
            sep = "") 
      } else {
        cat(Resample, " ", format(1:iters)[generation], " ",
            if(ga_maximize["internal"]) 
              signif(internal[1, ga_metric["internal"]], digits = dig) else 
               signif(-internal[1, ga_metric["internal"]], digits = dig),
            " (", length(subsets[[1]]), ")\n",
            sep = "") 
      }
    }
    
    ###################################################################
    ## From GA package
    
    ord <- order(Fitness, decreasing = TRUE)
    PopSorted <- Pop[ord,,drop = FALSE]
    FitnessSorted <- Fitness[ord]
    
    # selection
    if(is.function(funcs$selection)) { 
      sel <- funcs$selection(population = .Pop, fitness = .Fit)
      Pop <- sel$population
      Fitness <- sel$fitness
    } else { 
      sel <- sample(1:popSize, size = popSize, replace = TRUE)
      Pop <- .Pop[sel,]
      Fitness <- .Fit[sel]
    }
    .Pop <- Pop
    .Fit <- Fitness
    
    # crossover
    if(is.function(funcs$crossover) & pcrossover > 0) { 
      nmating <- floor(popSize/2)
      mating <- matrix(sample(1:(2*nmating), size = (2*nmating)), ncol = 2)
      for(i in seq_len(nmating)) { 
        if(pcrossover > runif(1)){ 
          parents <- mating[i,]
          Crossover <- funcs$crossover(population = .Pop, 
                                       fitness = .Fit, 
                                       parents = parents)
          Pop[parents,] <- Crossover$children
          Fitness[parents] <- Crossover$fitness
        }
      }             
      .Pop <- Pop
      .Fit <- Fitness
    }
    
    # mutation
    pm <- if(is.function(pmutation)) pmutation(object) else pmutation
    if(is.function(funcs$mutation) & pm > 0) { 
      for(i in seq_len(popSize))  { 
        if(pm > runif(1)) { 
          Mutation <- funcs$mutation(population = .Pop, parent = i)
          Pop[i,] <- Mutation
          Fitness[i] <- NA
        }
      }
      .Pop <- Pop
      .Fit <- Fitness
    }
    
    # elite
    if(elite > 0)  { 
      ord <- order(.Fit, na.last = TRUE)
      u <- which(!duplicated(PopSorted, margin = 1))
      Pop[ord[1:elite],] <- PopSorted[u[1:elite],]
      Fitness[ord[1:elite]] <- FitnessSorted[u[1:elite]]
      .Pop <- Pop
      .Fit <- Fitness
    } 
    
  } ## search iterations
  
  
  best_index <- which.max(internal[, ga_metric["internal"]])
  best_subset <- colnames(x)[subsets[[best_index]]]
  
  
  if(!ga_maximize["internal"]) {
    internal[, ga_metric["internal"]] <- -internal[, ga_metric["internal"]]
  }
  mod <- funcs$fit(x[, best_subset, drop=FALSE], y, lev = lvl, last = TRUE, ...)
  if(Resample != "") internal$Resample <- Resample
  if(Resample != "" && !is.null(testX)) external$Resample <- Resample
  
  diffs <- try(get_fitness_differences(colnames(x), 
                                       subsets, 
                                       external[, !(names(external) %in% ga_external_names), drop = FALSE]),
               silent = TRUE)
  if(class(diffs)[1] == "try-error") diffs <- NULL
  
  list(internal = internal,
       subsets = subsets, 
       external = external,
       final = best_subset,
       fit = mod,
       diffs = diffs)
}

###################################################################
##

print.gafs <- function (x, top = 5, 
                        digits = max(3, getOption("digits") - 3), 
                        ...) {
  cat("\nGenetic Algorithm Feature Selection\n\n")
  
  cat(x$dims[1], 
      " samples\n", 
      x$dims[2],
      " predictor", ifelse(x$dims[2] > 1, "s\n", "\n"),
      sep = "")
  if(!is.null(x$levels)){
    cat(length(x$levels), 
        "classes:",
        paste("'", x$levels, "'", sep = "", collapse = ", "),
        "\n")
  }
  cat("\n")
  
  cat("Maximum generations:", max(x$iters), "\n")
  cat("Population per generation:", x$ga_param$popSize, "\n")
  cat("Crossover probability:", x$ga_param$pcrossover, "\n")
  if(is.function(x$ga_param$pmutation)) {
    cat("Mutation probability: variable\n")    
  } else     cat("Mutation probability:", x$ga_param$pmutation, "\n")    
  cat("Elitism:", x$ga_param$elite, "\n\n")
  
  inames <- names(x$internal)
  inames <- inames[!(inames %in% ga_internal_names)]
  enames <- names(x$external)
  enames <- enames[!(enames %in% ga_external_names)]
  cat("Internal performance value", ifelse(length(inames) > 1, "s: ", ": "),
      paste(inames, sep = "", collapse = ", "), "\n", sep = "")
  cat("Subset selection driven to", 
      if(x$control$maximize["internal"]) "maximize internal" else "minimize internal", 
      x$control$metric["internal"],  "\n")  
  cat("\n")
  cat("External performance value", ifelse(length(enames) > 1, "s: ", ": "),
      paste(enames, sep = "", collapse = ", "), "\n", sep = "")  
  if(x$auto) {
  cat("Best iteration chose by", 
      if(x$control$maximize["external"]) "maximizing external" else "minimizing external", 
      x$control$metric["external"],  "\n")
  } else {
    cat("Best iteration chosen manually\n")
  }
  resampleN <- unlist(lapply(x$control$index, length))
  numResamp <- length(resampleN)
  resampText <- getFromNamespace("resampName", "caret")(x)
  cat("External resampling method:", resampText, "\n")
  
  if(x$control$holdout > 0)
    cat("Subsampling for internal fitness calculation: ", 
        round(x$control$holdout*100, digits), "%\n", sep = "")
  
  cat("\n")
  
  vars <- sort(table(unlist(x$resampled_vars)), decreasing = TRUE)
  
  top <- min(top, length(vars))
  
  smallVars <- vars[1:top]
  smallVars <- round(smallVars/length(x$control$index)*100, 1)
  
  varText <- paste0(names(smallVars), " (", smallVars, "%)")
  varText <- paste(varText, collapse = ", ")
  
  if(!all(is.na(smallVars))) {
    cat("During resampling:\n  * the top ",
        top,
        " selected variables (out of a possible ",
        x$dims[2],
        "):\n    ",
        varText,
        "\n",
        sep = "")
    cat("  * on average, ",
        round(mean(unlist(lapply(x$resampled_vars, length))), 1),
        " variables were selected (min = ",
        round(min(unlist(lapply(x$resampled_vars, length))), 1),
        ", max = ",
        round(max(unlist(lapply(x$resampled_vars, length))), 1),
        ")\n\n",
        sep = "")
  } else {
    cat("During resampling, no variables were selected.\n\n")
  }
  
  cat("In the final search using the entire training set:\n",
      "  *", length(x$optVariables), "features selected at iteration",
      x$optIter, "including:\n    ",
      paste(x$optVariables[1:min(length(x$optVariables), top)], 
            sep = "", collapse = ", "),
      if(length(x$optVariables) > top) "..." else "", 
      "\n")
  perf_dat <- subset(x$external, Iter == x$optIter)
  perf_dat <- perf_dat[!(names(perf_dat) %in% c("Iter", "Resample"))]
  perf <- colMeans(perf_dat)
  cat("   * external performance at this iteration is\n\n")
  ch_perf  <- format(perf, digits = digits, row.names = FALSE)
  ch_perf[1] <- paste("    ", ch_perf[1])
  print(ch_perf, quote = FALSE)
  cat("\n")  
  
  invisible(x)
}

predict.gafs <- function (object, newdata, ...) {
  newdata <- newdata[, object$optVariables, drop = FALSE]
  object$control$functions$pred(object$fit, newdata)  
}

###################################################################
##

gafs <- function (x, ...) UseMethod("gafs")

"gafs.default" <-
  function(x, y,
           iters = 10,
           popSize = 50, 
           pcrossover = 0.8, 
           pmutation = 0.1, 
           elite = 0, 
           suggestions = NULL, 
           differences = TRUE,
           gafsControl = gafsControl(), 
           ...) {
    startTime <- proc.time()
    funcCall <- match.call(expand.dots = TRUE)
    
    if(is.null(gafsControl$metric)) 
      gafsControl$metric <- rep(ifelse(is.factor(y), "Accuracy", "RMSE"), 2)
    if(is.null(gafsControl$maximize)) 
      gafsControl$maximize <- rep(ifelse(gafsControl$metric == "RMSE", FALSE, TRUE), 2)
    if(is.null(names(gafsControl$metric)))
      names(gafsControl$metric) <- c("internal", "external")
    if(is.null(names(gafsControl$maximize)))
      names(gafsControl$maximize) <- c("internal", "external")
    
    if(nrow(x) != length(y)) stop("there should be the same number of samples in x and y")
    numFeat <- ncol(x)
    classLevels <- levels(y)
    
    if(is.null(gafsControl$index)) 
      gafsControl$index <- switch(tolower(gafsControl$method),
                                  cv = createFolds(y, gafsControl$number, returnTrain = TRUE),
                                  repeatedcv = createMultiFolds(y, gafsControl$number, gafsControl$repeats),
                                  loocv = createFolds(y, length(y), returnTrain = TRUE),
                                  boot =, boot632 = createResample(y, gafsControl$number),
                                  test = createDataPartition(y, 1, gafsControl$p),
                                  lgocv = createDataPartition(y, gafsControl$number, gafsControl$p))
    
    if(is.null(names(gafsControl$index))) 
      names(gafsControl$index) <- getFromNamespace("prettySeq", "caret")(gafsControl$index)
    
    ## Create hold--out indicies
    if(is.null(gafsControl$indexOut)){
      gafsControl$indexOut <- lapply(gafsControl$index,
                                     function(training, allSamples) allSamples[-unique(training)],
                                     allSamples = seq(along = y))
      names(gafsControl$indexOut) <- getFromNamespace("prettySeq", "caret")(gafsControl$indexOut)
    }
    
    if(!is.null(gafsControl$seeds)) {
      if(length(gafsControl$seeds) < length(gafsControl$index) + 1)
        stop(paste("There must be at least", 
                   length(gafsControl$index) + 1,
                   "random number seeds passed to gafsControl"))
    } else {
      gafsControl$seeds <- sample.int(100000, length(gafsControl$index) + 1)
    }
    
    ## check summary function and metric
    testOutput <- data.frame(pred = sample(y, min(10, length(y))),
                             obs = sample(y, min(10, length(y))))
    
    if(is.factor(y))
      for(i in seq(along = classLevels)) 
        testOutput[, classLevels[i]] <- runif(nrow(testOutput))
    
    test <- gafsControl$functions$fitness_extern(testOutput, lev = classLevels)
    
    perfNames <- names(test)
    if(is.null(perfNames)) {
      warning(paste("The external fitness results should be a *named* vector;",
                    "new name(s) are",
                    paste(paste0("external", 1:length(test)), sep = "", collapse = ", ")),
              immediate. = TRUE)
      perfNames <- paste0("external", 1:length(test))
    }
    if(!(gafsControl$metric["external"] %in% perfNames)) {
      warning(paste("The metric '", gafsControl$metric["external"], 
                    "' is not created by the summary function; '",
                    perfNames[1], "' will be used instead", sep = ""))
      gafsControl$metric["external"] <- perfNames[1]
    }
    
    `%op%` <- getOper(gafsControl$allowParallel && getDoParWorkers() > 1)
    
    result <- foreach(i = seq(along = gafsControl$index), .combine = "c", .verbose = FALSE, .errorhandling = "stop") %op% {   
      ga_select(x[gafsControl$index[[i]],,drop=FALSE], 
                y[gafsControl$index[[i]]], 
                funcs = gafsControl$functions,
                ga_maximize = gafsControl$maximize,
                ga_metric = gafsControl$metric,
                iters = iters,
                popSize = popSize, 
                pcrossover = pcrossover, 
                pmutation = pmutation, 
                elite = elite, 
                suggestions = suggestions, 
                ga_verbose = gafsControl$verbose,
                testX = x[gafsControl$indexOut[[i]],,drop=FALSE],
                testY = y[gafsControl$indexOut[[i]]],
                ga_seed = gafsControl$seeds[i],
                Resample = names(gafsControl$index)[i],
                holdout = gafsControl$holdout,
                lvl = classLevels,
                genParallel = gafsControl$genParallel,
                ...)
    }
    ## TODO save only the parts you need inside of loop
    external <- result[names(result) == "external"]
    external <- do.call("rbind", external)
    rownames(external) <- NULL
    internal <- result[names(result) == "internal"]
    internal <- do.call("rbind", internal)
    rownames(internal) <- NULL
    selected_vars <- result[names(result) == "final"]
    names(selected_vars) <- names(gafsControl$index)
    
    if(differences) {
      diffs <- try(process_diffs(result[names(result) == "diffs"],
                                 colnames(x)),
                   silent = TRUE)
      if(class(diffs)[1] == "try-error") {
        diffs <- NULL
        # warning("An error occured when computing the variable differences")
      }
    } else diffs <- NULL
    rm(result)
    
    if(gafsControl$verbose) cat("+ final GA\n")
    
    if(gafsControl$holdout > 0) {
      in_holdout <- createDataPartition(y, 
                                        p = gafsControl$holdout, 
                                        list = FALSE)
      in_model <- seq(along = y)[-unique(in_holdout)]
    } else {
      in_model <- seq(along = y)
      in_holdout <- NULL
    }
    final_ga <- ga_select(x[in_model,,drop=FALSE], 
                          y[in_model], 
                          funcs = gafsControl$functions,
                          ga_maximize = gafsControl$maximize,
                          ga_metric = gafsControl$metric,
                          iters = iters,
                          popSize = popSize, 
                          pcrossover = pcrossover, 
                          pmutation = pmutation, 
                          elite = elite, 
                          suggestions = suggestions, 
                          ga_verbose = gafsControl$verbose,
                          testX = if(!is.null(in_holdout)) x[in_holdout,,drop=FALSE] else NULL,
                          testY = if(!is.null(in_holdout)) y[in_holdout] else NULL,
                          ga_seed = gafsControl$seeds[length(gafsControl$seeds)],
                          lvl = classLevels,
                          genParallel = gafsControl$genParallel,
                          ...)
    averages <- ddply(external, .(Iter), 
                      function(x, nms) {
                        apply(x[, perfNames, drop = FALSE], 2, mean)
                      },
                      nms = perfNames)
    if(!is.null(gafsControl$functions$selectIter)) {
      best_index <- gafsControl$functions$selectIter(averages, 
                                                     metric = gafsControl$metric["external"], 
                                                     maximize = gafsControl$maximize["external"])
      best_iter <- averages$Iter[best_index]
      best_vars <- colnames(x)[final_ga$subsets[[best_index]]]
    } else { 
      best_index <- if(gafsControl$maximize["external"]) 
        which.max(averages[,gafsControl$metric["external"]]) else 
          which.min(averages[,gafsControl$metric["external"]])
      best_iter <- averages$Iter[best_index]
      best_vars <- colnames(x)[final_ga$subsets[[best_index]]]
    }
    if(gafsControl$verbose) cat("+ final model\n")
    
    fit <- gafsControl$functions$fit(x[, best_vars, drop=FALSE], y, lev = lvls, last = TRUE, ...)
    endTime <- proc.time()
    res <- list(fit = fit,
                ga = final_ga,
                ga_param = list(popSize = popSize, 
                                pcrossover = pcrossover, 
                                pmutation = pmutation, 
                                elite = elite),
                external = external, 
                internal = internal,
                resampled_vars = selected_vars,
                averages = averages,
                iters = iters,
                optVariables = best_vars,
                optIter = best_iter,
                control = gafsControl,
                dims = dim(x),
                differences = diffs,
                perfNames = perfNames,
                auto = TRUE,
                the_dots = list(...),
                call = funcCall,
                times = list(everything = endTime - startTime),
                levels = if(is.factor(y)) classLevels else NULL)
    
    ## now do analysis for whole dataset, plus make update method
    class(res) <- "gafs"
    res
  }


###################################################################
##

plot.gafs <- function(x, 
                      metric = x$control$metric["external"], 
                      estimate = c("internal", "external"), 
                      output = "ggplot",
                      ...) {
  int_names <- names(x$internal)[!(names(x$internal) %in% ga_internal_names)]
  ext_names <- names(x$external)[!(names(x$external) %in% ga_external_names)]
  common <- intersect(int_names, ext_names)
  both_estimates <- length(estimate) == 2  && all(sort(estimate) == c("external", "internal"))
  
  if(both_estimates){
    if(!metric %in% common) stop(paste("'", metric, "' not computed in both estimates"))
    tmp_e <- x$external[, c("Iter", "Resample", common)]
    tmp_e$Estimate <- "External"
    tmp_i <- x$internal[, c("Iter", "Resample", common)]
    tmp_i$Estimate <- "Internal"
    plot_dat <- rbind(tmp_e, tmp_i)
  } else {
    if("internal" %in% estimate) {
      if(!metric %in% int_names) stop(paste("'", metric, "' not computed internally"))
      plot_dat <- x$internal[, c("Iter", "Resample", int_names)]
    }
    if("external" %in% estimate) {
      if(!metric %in% int_names) stop(paste("'", metric, "' not computed externally"))
      plot_dat <- x$external[, c("Iter", "Resample", ext_names)]
    }
  }
  if(output == "data") out <- plot_dat
  plot_dat <- if(both_estimates) 
    ddply(plot_dat, c("Iter", "Estimate"),
          function(x) c(Mean = mean(x[, metric]))) else 
            ddply(plot_dat, c("Iter"),
                  function(x) c(Mean = mean(x[, metric])))                  
  
  if(output == "ggplot") {
    out <- if(both_estimates) 
      ggplot(plot_dat, aes(x = Iter, y = Mean, color = Estimate)) + geom_point() else 
        ggplot(plot_dat, aes(x = Iter, y = Mean)) + geom_point() 
    out <- out + xlab("Generation")
    
  } 
  if(output == "lattice") {
    out <- if(both_estimates) 
      xyplot(Mean ~ Iter, data = plot_dat, groups = Estimate, ...) else 
        xyplot(Mean ~ Iter, data = plot_dat, ...)
    out <- update(out, xlab = "Generation")
  }
  out
}

###################################################################
##

caretGA <- list(fit = function(x, y, lev = NULL, last = FALSE, ...) train(x, y, ...),
                pred = function(object, x) {
                  tmp <- predict(object, x)
                  if(object$control$classProbs) {
                    out <- cbind(data.frame(pred = tmp),
                                 as.data.frame(predict(object, x, type = "prob")))
                  } else out <- tmp
                  out
                },
                fitness_intern = function(object, x, y, maximize, p){
                  perf_val <- getTrainPerf(object)
                  perf_val <- perf_val[names(perf_val) != "method"]
                  perf_val <- unlist(perf_val)
                  names(perf_val) <- gsub("Train", "", names(perf_val))
                  perf_val
                },
                fitness_extern = defaultSummary,
                initial = gafs_initial,
                selection = gafs_lrSelection,
                crossover = gafs_spCrossover,
                mutation = gafs_raMutation,
                selectIter = best)

treebagGA <- list(fit = function(x, y, lev = NULL, last = FALSE, ...) {
  library(ipred)
  ipredbagg(y, x, ...)
},
pred = function(object, x) {
  tmp <- predict(object, x)
  if(is.factor(object$y)) {
    out <- cbind(data.frame(pred = tmp),
                 as.data.frame(predict(object, x, type = "prob")))
  } else out <- tmp
  out
},
fitness_intern = function(object, x, y, maximize, p)
  ipredStats(object)[1:2],
fitness_extern = defaultSummary,
initial = gafs_initial,
selection = gafs_lrSelection,
crossover = gafs_spCrossover,
mutation = gafs_raMutation,
selectIter = best)

rfGA <-  list(fit = function(x, y, lev = NULL, last = FALSE, ...) {
  library(randomForest)
  randomForest(x, y, ...)
},
pred = function(object, x) {
  tmp <- predict(object, x)
  if(is.factor(object$y)) {
    out <- cbind(data.frame(pred = tmp),
                 as.data.frame(predict(object, x, type = "prob")))
  } else out <- tmp
  out
},
fitness_intern = function(object, x, y, maximize, p) rfStats(object),
fitness_extern = defaultSummary,
initial = gafs_initial,
selection = gafs_lrSelection,
crossover = gafs_spCrossover,
mutation = gafs_raMutation,
selectIter = best)

update.gafs <- function(object, iter, x, y, ...) {
  iter <- iter[1]
  if(iter > length(object$ga$subsets))
    stop(paste("iter must be less than", length(object$ga$subsets)))
  if(is.null(x) | is.null(y))
    stop("the original training data is needed to refit the model")
  args <- list(x = x[, object$ga$subsets[[iter]], drop=FALSE], 
               y = y, lev = object$levels, last = TRUE)
  if(length(object$the_dots) > 0) args <- c(args, object$the_dots)
  if(object$control$verbose)
    cat("Refitting model to use", length(object$ga$subsets[[iter]]),
        "predictors from generation", iter, "\n")
  object$fit <- do.call(object$control$functions$fit, args)
  object$auto <- FALSE
  object$optVariables <- colnames(x)[object$ga$subsets[[iter]]]
  object$optIter <- iter
  object  
}

"varImp.gafs" <- function(object, 
                          metric = object$control$metric["external"], 
                          maximize = object$control$maximize["external"],
                          ...) {
  
  if(is.null(object$differences)) 
    stop("must have used `differences = TRUE`")
  out <- object$differences[,metric, drop = FALSE]
  rownames(out) <- as.character(object$differences$Variable)
  if(!maximize) out[, metric, drop = FALSE] <- -out[, metric, drop = FALSE]
  out <- out[order(-out[, metric]),, drop = FALSE]
  out
}




