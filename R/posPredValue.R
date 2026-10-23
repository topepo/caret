#' @rdname sensitivity
#' @export
posPredValue <-
  function(data, ...) {
    UseMethod("posPredValue")
  }

#' @rdname sensitivity
#' @export
"posPredValue.default" <-
  function(
    data,
    reference,
    positive = levels(reference)[1],
    prevalence = NULL,
    ...
  ) {
    xtab <- factor_conf_table(data, reference, TRUE, "inputs must be factors")
    if (is.null(xtab)) {
      return(NA_real_)
    }
    posPredValue.table(xtab, positive = positive, prevalence = prevalence)
  }

#' @rdname sensitivity
#' @export
"posPredValue.table" <-
  function(data, positive = rownames(data)[1], prevalence = NULL, ...) {
    ## "truth" in columns, predictions in rows
    check_conf_table(data)
    table_metric(yardstick::ppv, data, positive, prevalence = prevalence)
  }

#' @rdname sensitivity
#' @export
"posPredValue.matrix" <-
  function(data, positive = rownames(data)[1], prevalence = NULL, ...) {
    posPredValue.table(
      as.table(data),
      positive = positive,
      prevalence = prevalence
    )
  }
