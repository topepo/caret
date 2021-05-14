
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
                   initial = c('vars', 'popSize', '...'),
                   selection = c('population', 'fitness', 'r', 'q', '...'),
                   crossover = c('population', 'fitness', 'parents', '...'),
                   mutation = c('population', 'parent', '...'),
                   selectIter = c('x', 'metric', 'maximize'))

  check_names <- names(x)
  check_names <- check_names[check_names != "fitness_extern"]
  for(i in check_names) {
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




#' Ancillary genetic algorithm functions
#'
#' @description
#' Built-in functions related to genetic algorithms
#'
#' These functions are used with the \code{functions} argument of the
#' \code{\link{gafsControl}} function. More information on the details of these
#' functions are at \url{http://topepo.github.io/caret/feature-selection-using-genetic-algorithms.html}.
#'
#' Most of the \code{gafs_*} functions are based on those from the GA package
#' by Luca Scrucca. These functions here are small re-writes to work outside of
#' the GA package.
#'
#' The objects \code{caretGA}, \code{rfGA} and \code{treebagGA} are example
#' lists that can be used with the \code{functions} argument of
#' \code{\link{gafsControl}}.
#'
#' In the case of \code{caretGA}, the \code{...} structure of
#' \code{\link{gafs}} passes through to the model fitting routine. As a
#' consequence, the \code{\link{train}} function can easily be accessed by
#' passing important arguments belonging to \code{\link{train}} to
#' \code{\link{gafs}}. See the examples below. By default, using \code{caretGA}
#' will used the resampled performance estimates produced by
#' \code{\link{train}} as the internal estimate of fitness.
#'
#' For \code{rfGA} and \code{treebagGA}, the \code{randomForest} and
#' \code{bagging} functions are used directly (i.e. \code{\link{train}} is not
#' used). Arguments to either of these functions can also be passed to them
#' though the \code{\link{gafs}} call (see examples below). For these two
#' functions, the internal fitness is estimated using the out-of-bag estimates
#' naturally produced by those functions. While faster, this limits the user to
#' accuracy or Kappa (for classification) and RMSE and R-squared (for
#' regression).
#'
#' @aliases gafs_initial gafs_lrSelection gafs_rwSelection gafs_tourSelection
#' gafs_uCrossover gafs_spCrossover gafs_raMutation caretGA rfGA treebagGA
#' @param vars number of possible predictors
#' @param popSize the population size passed into \code{\link{gafs}}
#' @param population a binary matrix of the current subsets with predictors in
#' columns and individuals in rows
#' @param fitness a vector of fitness values
#' @param parent,parents integer(s) for which chromosomes are altered
#' @param r,q,k tuning parameters for the specific selection operator
#' @param \dots not currently used
#' @return The return value depends on the function.
#' @author Luca Scrucca, \code{gafs_initial}, \code{caretGA}, \code{rfGA} and
#' \code{treebagGA} by Max Kuhn
#' @seealso \code{\link{gafs}}, \code{\link{gafsControl}}
#' @references Scrucca L (2013). GA: A Package for Genetic Algorithms in R.
#' Journal of Statistical Software, 53(4), 1-37.
#'
#' \url{https://cran.r-project.org/package=GA}
#'
#' \url{http://topepo.github.io/caret/feature-selection-using-genetic-algorithms.html}
#' @examples
#'
#' pop <- gafs_initial(vars = 10, popSize = 10)
#' pop
#'
#' gafs_lrSelection(population = pop, fitness = 1:10)
#'
#' gafs_spCrossover(population = pop, fitness = 1:10, parents = 1:2)
#'
#'
#' \dontrun{
#' ## Hypothetical examples
#' lda_ga <- gafs(x = predictors,
#'                y = classes,
#'                gafsControl = gafsControl(functions = caretGA),
#'                ## now pass arguments to `train`
#'                method = "lda",
#'                metric = "Accuracy"
#'                trControl = trainControl(method = "cv", classProbs = TRUE))
#'
#' rf_ga <- gafs(x = predictors,
#'               y = classes,
#'               gafsControl = gafsControl(functions = rfGA),
#'               ## these are arguments to `randomForest`
#'               ntree = 1000,
#'               importance = TRUE)
#' 	}
#'
#'
#' @export gafs_initial
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

#' @rdname gafs_initial
#' @export
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

#' @rdname gafs_initial
#' @export
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

#' @rdname gafs_initial
#' @export
gafs_raMutation <- function (population, parent, ...)  {
  mutate <- parent <- as.vector(population[parent, ])
  n <- length(parent)
  j <- sample(1:n, size = 1)
  mutate[j] <- abs(mutate[j] - 1)
  mutate
}

#' @rdname gafs_initial
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

#' @rdname gafs_initial
#' @export
gafs_rwSelection <- function (population, fitness, ...) {
  popSize <- nrow(population)
  prob <- abs(fitness)/sum(abs(fitness))
  sel <- sample(1:popSize, size = popSize,
                prob = pmin(pmax(0, prob), 1, na.rm = TRUE), replace = TRUE)
  out <- list(population = population[sel, , drop = FALSE],
              fitness = fitness[sel])
  return(out)
}

#' @rdname gafs_initial
#' @export
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

#' @rdname gafs_initial
#' @importFrom stats runif
#' @export
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

#' @rdname safsControl
#' @export
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
                       perf, holdoutPerf, testPerf,
                       ga_metric, ga_maximize, lvl = lvl, last = FALSE, indiv = 0, ...) {
  mod <- funcs$fit(x[, ind, drop=FALSE], y, lev = lvl, last = last,...)

  if (!is.null(holdoutX)) {
    intern_x <- holdoutX[, ind, drop = FALSE]
    if(!is.null(holdoutPerf))
      intern_x <- cbind(intern_x, holdoutPerf)
  } else {
    intern_x <- x[, ind, drop = FALSE]
    if(!is.null(perf))
      intern_x <- cbind(intern_x, perf)
  }
  internal <-
    funcs$fitness_intern(
      mod,
      x = intern_x,
      y = if(!is.null(holdoutY)) holdoutY else y,
      p = ncol(x)
      )
  if(!is.null(testX)) {
    modelPred <- funcs$pred(mod, testX[, ind, drop=FALSE])
    if(is.data.frame(modelPred) | is.matrix(modelPred)) {
      if(is.matrix(modelPred)) modelPred <- as.data.frame(modelPred, stringsAsFactors = TRUE)
      modelPred$obs <- testY
      modelPred$Size <- length(ind)
    } else modelPred <- data.frame(pred = modelPred, obs = testY, Size = sum(ind == 1))
    if(!is.null(testPerf))
      modelPred <- cbind(modelPred, testPerf)

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


#' @importFrom stats runif
#' @import foreach
ga_select <- function(x, y, perf = NULL,

                      testX = NULL, testY = NULL, testPerf = NULL,

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
    holdout_perf <- perf[in_holdout,,drop = FALSE]
    x <- x[-in_holdout,,drop = FALSE]
    y <- y[-in_holdout]
    perf <- perf[-in_holdout,,drop = FALSE]

  } else {
    holdout_x <- NULL
    holdout_y <- NULL
    holdout_perf <- NULL
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
    currennt_results <-
      foreach(
        i = seq_len(popSize),
        .combine = "c",
        .verbose = FALSE,
        .errorhandling = "stop",
        .packages = "recipes") %op% {
          ga_wrapper(ind = which(Pop[i,] == 1),
                     x = x, y = y, perf = perf,
                     funcs,
                     holdoutX = holdout_x, holdoutY = holdout_y,
                     holdoutPerf = holdout_perf,
                     testX = testX, testY = testY,
                     testPerf = testPerf,
                     ga_metric = ga_metric,
                     ga_maximize = ga_maximize,
                     lvl = lvl,
                     last = Resample == "",
                     indiv = i,
                     ...
          )
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
  if (inherits(diffs, "try-error")) diffs <- NULL

  list(internal = internal,
       subsets = subsets,
       external = external,
       final = best_subset,
       fit = mod,
       diffs = diffs)
}

###################################################################
##

#' @importFrom utils getFromNamespace
#' @export
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



#' Predict new samples
#'
#' Predict new samples using \code{\link{safs}} and \code{\link{gafs}} objects.
#'
#' Only the predictors listed in \code{object$optVariables} are required.
#'
#' @aliases predict.gafs predict.safs
#' @param object an object of class \code{\link{safs}} or \code{\link{gafs}}
#' @param newdata a data frame or matrix of predictors.
#' @param \dots not currently used
#' @return The type of result depends on what was specified in
#' \code{object$control$functions$predict}.
#' @author Max Kuhn
#' @seealso \code{\link{safs}}, \code{\link{gafs}}
#' @keywords multivariate
#' @method predict gafs
#' @export
#' @examples
#'
#' \dontrun{
#'
#' set.seed(1)
#' train_data <- twoClassSim(100, noiseVars = 10)
#' test_data  <- twoClassSim(10,  noiseVars = 10)
#'
#' ## A short example
#' ctrl <- safsControl(functions = rfSA,
#'                     method = "cv",
#'                     number = 3)
#'
#' rf_search <- safs(x = train_data[, -ncol(train_data)],
#'                   y = train_data$Class,
#'                   iters = 3,
#'                   safsControl = ctrl)
#'
#' rf_search
#'
#' predict(rf_search, train_data)
#' }
#'
#' @export predict.gafs
predict.gafs <- function (object, newdata, ...) {
  if (any(names(object) == "recipe") && !is.null(object$recipe)) {
    newdata <-
      bake(object$recipe, newdata, all_predictors(), composition = "data.frame")
  } else {
    newdata <- newdata[, object$optVariables, drop = FALSE]
  }
  object$control$functions$pred(object$fit, newdata)
}

###################################################################
##

#' @export
gafs <- function (x, ...) UseMethod("gafs")



#' Genetic algorithm feature selection
#'
#' Supervised feature selection using genetic algorithms
#'
#' \code{\link{gafs}} conducts a supervised binary search of the predictor
#' space using a genetic algorithm. See Mitchell (1996) and Scrucca (2013) for
#' more details on genetic algorithms.
#'
#' This function conducts the search of the feature space repeatedly within
#' resampling iterations. First, the training data are split be whatever
#' resampling method was specified in the control function. For example, if
#' 10-fold cross-validation is selected, the entire genetic algorithm is
#' conducted 10 separate times. For the first fold, nine tenths of the data are
#' used in the search while the remaining tenth is used to estimate the
#' external performance since these data points were not used in the search.
#'
#' During the genetic algorithm, a measure of fitness is needed to guide the
#' search. This is the internal measure of performance. During the search, the
#' data that are available are the instances selected by the top-level
#' resampling (e.g. the nine tenths mentioned above). A common approach is to
#' conduct another resampling procedure. Another option is to use a holdout set
#' of samples to determine the internal estimate of performance (see the
#' holdout argument of the control function). While this is faster, it is more
#' likely to cause overfitting of the features and should only be used when a
#' large amount of training data are available. Yet another idea is to use a
#' penalized metric (such as the AIC statistic) but this may not exist for some
#' metrics (e.g. the area under the ROC curve).
#'
#' The internal estimates of performance will eventually overfit the subsets to
#' the data. However, since the external estimate is not used by the search, it
#' is able to make better assessments of overfitting. After resampling, this
#' function determines the optimal number of generations for the GA.
#'
#' Finally, the entire data set is used in the last execution of the genetic
#' algorithm search and the final model is built on the predictor subset that
#' is associated with the optimal number of generations determined by
#' resampling (although the update function can be used to manually set the
#' number of generations).
#'
#' This is an example of the output produced when \code{gafsControl(verbose =
#' TRUE)} is used:
#'
#' \preformatted{
#' Fold2 1 0.715 (13)
#' Fold2 2 0.715->0.737 (13->17, 30.4\%) *
#' Fold2 3 0.737->0.732 (17->14, 24.0\%)
#' Fold2 4 0.737->0.769 (17->23, 25.0\%) *
#' }
#'
#' For the second resample (e.g. fold 2), the best subset across all
#' individuals tested in the first generation contained 13 predictors and was
#' associated with a fitness value of 0.715. The second generation produced a
#' better subset containing 17 samples with an associated fitness values of
#' 0.737 (and improvement is symbolized by the \code{*}. The percentage listed
#' is the Jaccard similarity between the previous best individual (with 13
#' predictors) and the new best. The third generation did not produce a better
#' fitness value but the fourth generation did.
#'
#' The search algorithm can be parallelized in several places: \enumerate{
#' \item each externally resampled GA can be run independently (controlled by
#' the \code{allowParallel} option of \code{\link{gafsControl}}) \item within a
#' GA, the fitness calculations at a particular generation can be run in
#' parallel over the current set of individuals (see the \code{genParallel}
#' option in \code{\link{gafsControl}}) \item if inner resampling is used,
#' these can be run in parallel (controls depend on the function used. See, for
#' example, \code{\link[caret]{trainControl}}) \item any parallelization of the
#' individual model fits. This is also specific to the modeling function.  }
#'
#' It is probably best to pick one of these areas for parallelization and the
#' first is likely to produces the largest decrease in run-time since it is the
#' least likely to incur multiple re-starting of the worker processes. Keep in
#' mind that if multiple levels of parallelization occur, this can effect the
#' number of workers and the amount of memory required exponentially.
#'
#' @inheritParams train
#' @aliases gafs.default gafs
#' @param x An object where samples are in rows and features are in columns.
#' This could be a simple matrix, data frame or other type (e.g. sparse
#' matrix). For the recipes method, \code{x} is a recipe object. See Details below
#' @param y a numeric or factor vector containing the outcome for each sample
#' @param iters number of search iterations
#' @param popSize number of subsets evaluated at each iteration
#' @param pcrossover the crossover probability
#' @param pmutation the mutation probability
#' @param elite the number of best subsets to survive at each generation
#' @param suggestions a binary matrix of subsets strings to be included in the
#' initial population. If provided the number of columns must match the number
#' of columns in \code{x}
#' @param differences a logical: should the difference in fitness values with
#' and without each predictor be calculated?
#' @param gafsControl a list of values that define how this function acts. See
#' \code{\link{gafsControl}} and URL.
#' @param ... additional arguments to be passed to other methods
#' @return an object of class \code{gafs}
#' @author Max Kuhn, Luca Scrucca (for GA internals)
#' @seealso \code{\link{gafsControl}}, \code{\link{predict.gafs}},
#' \code{\link{caretGA}}, \code{\link{rfGA}} \code{\link{treebagGA}}
#' @references Kuhn M and Johnson K (2013), Applied Predictive Modeling,
#' Springer, Chapter 19 \url{http://appliedpredictivemodeling.com}
#'
#' Scrucca L (2013). GA: A Package for Genetic Algorithms in R. Journal of
#' Statistical Software, 53(4), 1-37. \url{https://www.jstatsoft.org/article/view/v053i04}
#'
#' Mitchell M (1996), An Introduction to Genetic Algorithms, MIT Press.
#'
#' \url{https://en.wikipedia.org/wiki/Jaccard_index}
#' @keywords models
#' @method gafs default
#' @export
#' @examples
#'
#' \dontrun{
#' set.seed(1)
#' train_data <- twoClassSim(100, noiseVars = 10)
#' test_data  <- twoClassSim(10,  noiseVars = 10)
#'
#' ## A short example
#' ctrl <- gafsControl(functions = rfGA,
#'                     method = "cv",
#'                     number = 3)
#'
#' rf_search <- gafs(x = train_data[, -ncol(train_data)],
#'                   y = train_data$Class,
#'                   iters = 3,
#'                   gafsControl = ctrl)
#'
#' rf_search
#'   }
#'
#' @export gafs.default
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
      gafsControl$maximize <- rep(ifelse(gafsControl$metric %in% c("RMSE", "MAE", "logLoss"), FALSE, TRUE), 2)
    if(is.null(names(gafsControl$metric)))
      names(gafsControl$metric) <- c("internal", "external")
    if(is.null(names(gafsControl$maximize)))
      names(gafsControl$maximize) <- c("internal", "external")

    if(nrow(x) != length(y)) stop("there should be the same number of samples in x and y")
    numFeat <- ncol(x)
    classLevels <- levels(y)

    if(is.null(gafsControl$index))
      gafsControl$index <- switch(
        tolower(gafsControl$method),
        cv = createFolds(y, gafsControl$number, returnTrain = TRUE),
        repeatedcv = createMultiFolds(y, gafsControl$number, gafsControl$repeats),
        loocv = createFolds(y, length(y), returnTrain = TRUE),
        boot =, boot632 = createResample(y, gafsControl$number),
        test = createDataPartition(y, 1, gafsControl$p),
        lgocv = createDataPartition(y, gafsControl$number, gafsControl$p)
        )

    if(is.null(names(gafsControl$index)))
      names(gafsControl$index) <- getFromNamespace("prettySeq", "caret")(gafsControl$index)

    ## Create hold-out indicies
    if(is.null(gafsControl$indexOut)){
      gafsControl$indexOut <-
        lapply(gafsControl$index,
               function(training, allSamples) allSamples[-unique(training)],
               allSamples = seq(along = y)
               )
      names(gafsControl$indexOut) <-
        getFromNamespace("prettySeq", "caret")(gafsControl$indexOut)
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

    result <-
      foreach(
        i = seq(along = gafsControl$index),
        .combine = "c", .verbose = FALSE,
        .errorhandling = "stop") %op% {
      ga_select(
        x[gafsControl$index[[i]],,drop=FALSE],
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
        ...
        )
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
    final_ga <- ga_select(
      x[in_model,,drop=FALSE],
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
      ...
      )
    averages <- ddply(external, .(Iter),
                      function(x, nms) {
                        apply(x[, perfNames, drop = FALSE], 2, mean)
                      },
                      nms = perfNames)
    if(!is.null(gafsControl$functions$selectIter)) {
      best_index <-
        gafsControl$functions$selectIter(
          averages,
          metric = gafsControl$metric["external"],
          maximize = gafsControl$maximize["external"]
        )
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


#' Plot Method for the gafs and safs Classes
#'
#' Plot the performance values versus search iteration
#'
#' The mean (averaged over the resamples) is plotted against the search
#' iteration using a scatter plot.
#'
#' When \code{output = "data"}, the unaveraged data are returned with columns
#' for all the performance metrics and the resample indicator.
#'
#' @aliases plot.safs plot.gafs
#' @param x an object of class \code{\link{gafs}} or \code{\link{safs}}
#' @param metric the measure of performance to plot (e.g. RMSE, accuracy, etc)
#' @param estimate the type of estimate: either "internal" or "external"
#' @param output either "data", "ggplot" or "lattice"
#' @param data,mapping,environment kept for consistency with
#'  \code{ggplot} and are not used here.
#' @param \dots For \code{plot} methods, these are options passed
#'  to \code{\link[lattice]{xyplot}}. For \code{ggplot} methods,
#'  they are not used.
#' @return Either a data frame, ggplot object or lattice object
#' @author Max Kuhn
#' @seealso \code{\link{gafs}}, \code{\link{safs}},
#' \code{\link[ggplot2]{ggplot}}, \code{\link[lattice]{xyplot}}
#' @keywords hplot
#' @method plot gafs
#' @export
#' @examples
#'
#' \dontrun{
#' set.seed(1)
#' train_data <- twoClassSim(100, noiseVars = 10)
#' test_data  <- twoClassSim(10,  noiseVars = 10)
#'
#' ## A short example
#' ctrl <- safsControl(functions = rfSA,
#'                     method = "cv",
#'                     number = 3)
#'
#' rf_search <- safs(x = train_data[, -ncol(train_data)],
#'                   y = train_data$Class,
#'                   iters = 50,
#'                   safsControl = ctrl)
#'
#' plot(rf_search)
#' plot(rf_search,
#' 	 output = "lattice",
#' 	 auto.key = list(columns = 2))
#'
#' plot_data <- plot(rf_search, output = "data")
#' summary(plot_data)
#'     }
#'
#' @export plot.gafs
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


#' @method ggplot gafs
#' @export ggplot.gafs
#' @export
#' @rdname plot.gafs
ggplot.gafs <-
  function (data = NULL, mapping = NULL, ..., environment = NULL) {
    plot.gafs(x = data, ...)
  }

###################################################################
##

#' @importFrom stats predict
#' @export
caretGA <- list(fit = function(x, y, lev = NULL, last = FALSE, ...) train(x, y, ...),
                pred = function(object, x) {
                  tmp <- predict(object, x)
                  if(object$control$classProbs) {
                    out <- cbind(data.frame(pred = tmp),
                                 as.data.frame(predict(object, x, type = "prob"), stringsAsFactors = TRUE))
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

#' @importFrom stats predict
#' @export
treebagGA <- list(fit = function(x, y, lev = NULL, last = FALSE, ...) {
  loadNamespace("ipred")
  ipred::ipredbagg(y, x, ...)
},
pred = function(object, x) {
  tmp <- predict(object, x)
  if(is.factor(object$y)) {
    out <- cbind(data.frame(pred = tmp),
                 as.data.frame(predict(object, x, type = "prob"), stringsAsFactors = TRUE))
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

#' @export
rfGA <-  list(fit = function(x, y, lev = NULL, last = FALSE, ...) {
  loadNamespace("randomForest")
  randomForest::randomForest(x, y, ...)
},
pred = function(object, x) {
  tmp <- predict(object, x)
  if(is.factor(object$y)) {
    out <- cbind(data.frame(pred = tmp),
                 as.data.frame(predict(object, x, type = "prob"), stringsAsFactors = TRUE))
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

#' @method update gafs
#' @export
update.gafs <- function(object, iter, x, y, ...) {
  iter <- iter[1]
  if (iter > length(object$ga$subsets))
    stop(paste("iter must be less than", length(object$ga$subsets)))

  if (!is.null(object$recipe)) {
    if (is.null(object$recipe$template))
      stop("Recipe is missing data to be juiced.", call. = FALSE)
    args <-
      list(x = juice(object$recipe, all_predictors(), composition = "data.frame"),
           y = juice(object$recipe, all_outcomes(), composition = "data.frame")[[1]],
           lev = object$levels,
           last = TRUE)
  } else {
    if (is.null(x) | is.null(y))
      stop("the original training data is needed to refit the model")
    args <- list(x = x[, object$ga$subsets[[iter]], drop=FALSE],
                 y = y, lev = object$levels, last = TRUE)
  }

  if (length(object$the_dots) > 0)
    args <- c(args, object$the_dots)
  if (object$control$verbose)
    cat("Refitting model to use", length(object$ga$subsets[[iter]]),
        "predictors from generation", iter, "\n")
  object$fit <- do.call(object$control$functions$fit, args)
  object$auto <- FALSE
  object$optVariables <- colnames(args$x)[object$ga$subsets[[iter]]]
  object$optIter <- iter
  object
}



#' Variable importances for GAs and SAs
#'
#' Variable importance scores for \code{\link{safs}} and \code{\link{gafs}}
#' objects.
#'
#' A crude measure of importance is computed for thee two search procedures. At
#' the end of a search process, the difference in the fitness values is
#' computed for models with and without each feature (based on the search
#' history). If a predictor has at least two subsets that include and did not
#' include the predictor, a t-statistic is computed (otherwise a value of
#' \code{NA} is assigned to the predictor).
#'
#' This computation is done separately for each resample and the t-statistics
#' are averaged (\code{NA} values are ignored) and this average is reported as
#' the importance. If the fitness value should be minimized, the negative value
#' of the t-statistic is used in the average.
#'
#' As such, the importance score reflects the standardized increase in fitness
#' that occurs when the predict is included in the subset. Values near zero (or
#' negative) indicate that the predictor may not be important to the model.
#'
#' @aliases varImp.gafs varImp.safs
#' @param object an \code{\link{safs}} or \code{\link{gafs}} object
#' @param metric a metric to compute importance (see Details below)
#' @param maximize are larger values of the metric better?
#' @param \dots not currently uses
#' @return a data frame where the rownames are the predictor names and the
#' column is the average t-statistic
#' @author Max Kuhn
#' @seealso \code{\link{safs}}, \code{\link{gafs}}
#' @export
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

#' @rdname gafs.default
#' @method gafs recipe
#' @export
"gafs.recipe" <-
  function(x, data,
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

    if(gafsControl$verbose)
      cat("Preparing recipe\n")

    orig_rec <- x
    trained_rec <- prep(
      x, training = data,
      fresh = TRUE,
      retain = TRUE,
      verbose = FALSE,
      stringsAsFactors = TRUE
    )
    x <- juice(trained_rec, all_predictors(), composition = "data.frame")
    y <- juice(trained_rec, all_outcomes(), composition = "data.frame")
    if(ncol(y) > 1)
      stop("`safs` doesn't support multivariate outcomes", call. = FALSE)
    y <- y[[1]]
    is_weight <- summary(trained_rec)$role == "case weight"
    if(any(is_weight))
      stop("`safs` does not allow for weights.", call. = FALSE)

    is_perf <- summary(trained_rec)$role == "performance var"
    if(any(is_perf)) {
      perf_data <- juice(trained_rec, has_role("performance var"))
    } else perf_data <- NULL

    if(is.null(gafsControl$metric))
      gafsControl$metric <- rep(ifelse(is.factor(y), "Accuracy", "RMSE"), 2)
    if(is.null(gafsControl$maximize))
      gafsControl$maximize <- rep(ifelse(gafsControl$metric %in% c("RMSE", "MAE", "logLoss"), FALSE, TRUE), 2)
    if(is.null(names(gafsControl$metric)))
      names(gafsControl$metric) <- c("internal", "external")
    if(is.null(names(gafsControl$maximize)))
      names(gafsControl$maximize) <- c("internal", "external")

    if(nrow(x) != length(y)) stop("there should be the same number of samples in x and y")
    numFeat <- ncol(x)
    classLevels <- levels(y)

    if(is.null(gafsControl$index))
      gafsControl$index <- switch(
        tolower(gafsControl$method),
        cv = createFolds(y, gafsControl$number, returnTrain = TRUE),
        repeatedcv = createMultiFolds(y, gafsControl$number, gafsControl$repeats),
        loocv = createFolds(y, length(y), returnTrain = TRUE),
        boot =, boot632 = createResample(y, gafsControl$number),
        test = createDataPartition(y, 1, gafsControl$p),
        lgocv = createDataPartition(y, gafsControl$number, gafsControl$p)
      )

    if(is.null(names(gafsControl$index)))
      names(gafsControl$index) <- getFromNamespace("prettySeq", "caret")(gafsControl$index)

    ## Create hold-out indicies
    if(is.null(gafsControl$indexOut)){
      gafsControl$indexOut <-
        lapply(gafsControl$index,
               function(training, allSamples) allSamples[-unique(training)],
               allSamples = seq(along = y)
        )
      names(gafsControl$indexOut) <-
        getFromNamespace("prettySeq", "caret")(gafsControl$indexOut)
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
    if(!is.null(perf_data))
      testOutput <- cbind(
        testOutput,
        perf_data[sample(1:nrow(perf_data), nrow(testOutput)),, drop = FALSE]
      )

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

    result <-
      foreach(
        i = seq(along = gafsControl$index),
        .combine = "c", .verbose = FALSE,
        .errorhandling = "stop") %op% {
          ga_select(
            x = x[gafsControl$index[[i]], , drop=FALSE],
            y = y[gafsControl$index[[i]]],
            perf = perf_data[gafsControl$index[[i]], , drop=FALSE],
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
            testX = x[ gafsControl$indexOut[[i]], , drop=FALSE],
            testY = y[ gafsControl$indexOut[[i]] ],
            testPerf = perf_data[ gafsControl$indexOut[[i]], , drop=FALSE],
            ga_seed = gafsControl$seeds[i],
            Resample = names(gafsControl$index)[i],
            holdout = gafsControl$holdout,
            lvl = classLevels,
            genParallel = gafsControl$genParallel,
            ...
          )
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
      if (inherits(diffs, "try-error"))  {
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
    final_ga <- ga_select(
      x = x[in_model, , drop=FALSE],
      y = y[in_model],
      perf = perf_data[in_model, , drop=FALSE],
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
      testPerf = if(!is.null(in_holdout)) perf_data[in_holdout,,drop=FALSE] else NULL,
      ga_seed = gafsControl$seeds[length(gafsControl$seeds)],
      lvl = classLevels,
      genParallel = gafsControl$genParallel,
      ...
    )
    averages <- ddply(external, .(Iter),
                      function(x, nms) {
                        apply(x[, perfNames, drop = FALSE], 2, mean)
                      },
                      nms = perfNames)
    if(!is.null(gafsControl$functions$selectIter)) {
      best_index <-
        gafsControl$functions$selectIter(
          averages,
          metric = gafsControl$metric["external"],
          maximize = gafsControl$maximize["external"]
        )
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

    # remove some items that won't be used again
    final_ga$sa$fit <- NULL
    final_ga$sa$final <- NULL
    final_ga$sa$diffs <- NULL

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
                recipe = trained_rec,
                call = funcCall,
                times = list(everything = endTime - startTime),
                levels = if(is.factor(y)) classLevels else NULL)

    ## now do analysis for whole dataset, plus make update method
    class(res) <- "gafs"
    res
  }



