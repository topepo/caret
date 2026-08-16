# fit a small bagged classifier on iris with the given function set
fit_iris_bag <- function(funcs, ...) {
  set.seed(1)
  suppressWarnings(bag(
    iris[, 1:4],
    iris$Species,
    B = 3,
    bagControl = bagControl(
      fit = funcs$fit,
      predict = funcs$pred,
      aggregate = funcs$aggregate
    ),
    ...
  ))
}
