modelInfo <- list(label = "Penalized Ordinal Regression",
                  library = c("ordinalNet", "plyr"),
                  check = function(pkg) {
                    requireNamespace("ordinalNet")
                    current <- packageDescription("ordinalNet")$Version
                    expected <- "2.0"
                    if(compareVersion(current, expected) < 0)
                      stop("This modeling workflow requires ordinalNet version ",
                           expected, "or greater.", call. = FALSE)
                  },
                  type = "Classification",
                  parameters = data.frame(
                    parameter = c('alpha', 'criteria', 'link', 'lambda', 'modeltype', 'family'),
                    class = c('numeric', 'character', 'character', 'numeric', 'character', 'character'),
                    label = c('Mixing Percentage', 'Selection criterion', 'Link Function','Penalty Parameter', 'Model Form', 'Model Family')
                  ),
                  grid = function(x, y, len = NULL, search = 'grid') {
                    links <- c("logit", "probit", "cloglog", "cauchit")
                    modeltypes <- c('parallel', 'nonparallel', 'semiparallel')
                    families <- c("cumulative", "sratio", "cratio", "acat")
                    max_lambda <- max(ordinalNet::ordinalNet(x = x, y = y, alpha = 0.01, nLambda = 2)$lambdaVals)
                    min_lambda <- 0.005 * max_lambda
                    if (search == "grid") {
                      out <- expand.grid(
                        alpha = seq(0.1, 1, length = len),
                        criteria = "aic",
                        link = links,
                        lambda = seq(from = min_lambda, to = max_lambda, length.out = len),
                        modeltype = modeltypes,
                        family = families
                      )
                    } else {
                      out <- data.frame(
                        alpha = runif(len, min = 0, 1),
                        criteria = sample(c("aic", "bic"), size = len, replace = TRUE),
                        link = sample(links, size = len, replace = TRUE),
                        lambda = runif(len, min = min_lambda, max = max_lambda),
                        modeltype = sample(modeltypes, size = len, replace = TRUE),
                        family = sample(families, size = len, replace = T)
                      )
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    if (!is.matrix(x)) x <- as.matrix(x)
                    out <- ordinalNet::ordinalNet(
                      x = x,
                      y = y,
                      alpha = param$alpha,
                      link = as.character(param$link),
                      lambdaVals = param$lambda,
                      family = as.character(param$family),
                      parallelTerms = (as.character(param$modeltype) %in% c('parallel', 'semiparallel')),
                      nonparallelTerms = (as.character(param$modeltype) %in% c('semiparallel', 'nonparallel')),
                      ...
                    )
                    out$.criteria <- as.character(param$criteria)
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if (!is.matrix(newdata)) newdata <- as.matrix(newdata)
                    out <- predict(modelFit, newdata, criteria = modelFit$.criteria, type = "class")
                    out <- modelFit$obsLevels[out]
                    out
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    if (!is.matrix(newdata)) newdata <- as.matrix(newdata)
                    out <- predict(modelFit, newdata, criteria = modelFit$.criteria, type = "response")
                    colnames(out) <- modelFit$obsLevels
                    out
                  },
                  predictors = function(x, lambda = NULL, ...) {
                    betas <- coef(x, criteria = x$.criteria)
                    out <- names(betas)[betas != 0]
                    out[!grepl("^Intercept", out)]
                  },
                  varImp = function(object, lambda = NULL, ...) {
                    betas <- coef(object, criteria = object$.criteria)
                    betas <- betas[!grepl("^Intercept", names(betas))]
                    out <- data.frame(Overall = abs(betas))
                    rownames(out) <- names(betas)
                    out
                  },
                  levels = function(x) if(any(names(x) == "obsLevels")) x$obsLevels else NULL,
                  tags = c("Generalized Linear Model", "Implicit Feature Selection",
                           "L1 Regularization", "L2 Regularization", "Linear Classifier",
                           "Linear Regression", "Ordinal Outcomes"),
                  sort = function(x) {
                    model_type_order <- c('parallel' = 1, 'semiparallel' = 2, 'nonparallel' = 3)
                    mt <- model_type_order[x$modeltype]
                    a <- x$alpha
                    l <- x$lambda
                    family_order <- c("cratio" = 1, "sratio" = 2, "acat" = 3, "cumulative" = 4)
                    f <- family_order[x$family]
                    x[order(mt, a, l, f), ]
                  },
                  notes = "Requires ordinalNet package version >= 2.0")
