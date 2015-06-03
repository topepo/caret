sampling_methods <- list(down = function(x, y) downSample(x, y, list = TRUE),
                         up = function(x, y) upSample(x, y, list = TRUE),
                         smote = function(x, y) {
                           checkInstall("DMwR")
                           library(DMwR)
                           dat <- if(is.data.frame(x)) x else as.data.frame(x)
                           dat$.y <- y
                           dat <- SMOTE(.y ~ ., data = dat)
                           list(x = dat[, !grepl(".y", colnames(dat), fixed = TRUE)],
                                y = dat$.y)
                         },
                         rose = function(x, y) {
                           checkInstall("ROSE")
                           library(ROSE)
                           dat <- if(is.data.frame(x)) x else as.data.frame(x)
                           dat$.y <- y
                           dat <- ROSE(.y ~ ., data = dat)$data
                           list(x = dat[, !grepl(".y", colnames(dat), fixed = TRUE)],
                                y = dat$.y)
                         })
save(sampling_methods, file = "../../pkg/caret/inst/models/sampling.RData")


