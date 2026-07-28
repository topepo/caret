sampling_methods <- list(
  down = function(x, y) {
    downSample(x, y, list = TRUE)
  },
  up = function(x, y) {
    upSample(x, y, list = TRUE)
  },
  smote = function(x, y) {
    checkInstall("themis")
    library(themis)
    if (is.data.frame(x)) {
      if (inherits(x, "tbl_df")) {
        dat <- as.data.frame(x)
      } else {
        dat <- x
      }
    } else {
      dat <- as.data.frame(x)
    }
    dat$.y <- y
    dat <- themis::smote(dat, var = ".y")
    list(
      x = dat[, !grepl(".y", colnames(dat), fixed = TRUE), drop = FALSE],
      y = dat$.y
    )
  },
  rose = function(x, y) {
    checkInstall("ROSE")
    library(ROSE)
    if (is.data.frame(x)) {
      dat <- x
    } else {
      dat <- as.data.frame(x)
    }
    dat$.y <- y
    dat <- ROSE(.y ~ ., data = dat)$data
    list(
      x = dat[, !grepl(".y", colnames(dat), fixed = TRUE), drop = FALSE],
      y = dat$.y
    )
  }
)
save(sampling_methods, file = "inst/models/sampling.RData", version = 2)
