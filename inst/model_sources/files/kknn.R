modelInfo <- list(
  label = "k-Nearest Neighbors",
  library = "kknn",
  loop = NULL,
  type = c('Regression', 'Classification'),
  parameters = data.frame(
    parameter = c('kmax', 'distance', 'kernel'),
    class = c('numeric', 'numeric', 'character'),
    label = c('Max. #Neighbors', 'Distance', 'Kernel')
  ),
  grid = function(x, y, len = NULL, search = "grid") {
    if (search == "grid") {
      out <- data.frame(
        kmax = (5:((2 * len) + 4))[(5:((2 * len) + 4)) %% 2 > 0],
        distance = 2,
        kernel = "optimal"
      )
    } else {
      if (is.factor(y)) {
        by_val <- length(levels(y))
      } else {
        by_val <- 1
      }
      kerns <- c(
        "rectangular",
        "triangular",
        "epanechnikov",
        "biweight",
        "triweight",
        "cos",
        "inv",
        "gaussian"
      )
      out <- data.frame(
        kmax = sample(
          seq(1, floor(nrow(x) / 3), by = by_val),
          size = len,
          replace = TRUE
        ),
        distance = runif(len, min = 0, max = 3),
        kernel = sample(kerns, size = len, replace = TRUE)
      )
    }
    out
  },
  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
    if (is.data.frame(x)) {
      dat <- x
    } else {
      dat <- as.data.frame(x, stringsAsFactors = TRUE)
    }
    dat$.outcome <- y
    kknn::train.kknn(
      .outcome ~ .,
      data = dat,
      kmax = param$kmax,
      distance = param$distance,
      kernel = as.character(param$kernel),
      ...
    )
  },
  predict = function(modelFit, newdata, submodels = NULL) {
    if (!is.data.frame(newdata)) {
      newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
    }
    predict(modelFit, newdata)
  },
  levels = function(x) x$obsLevels,
  tags = "Prototype Models",
  prob = function(modelFit, newdata, submodels = NULL) {
    if (!is.data.frame(newdata)) {
      newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
    }
    predict(modelFit, newdata, type = "prob")
  },
  sort = function(x) x[order(-x[, 1]), ]
)
