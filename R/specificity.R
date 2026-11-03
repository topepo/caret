#' @export
#' @rdname sensitivity
specificity <-
  function(data, ...) {
    UseMethod("specificity")
  }

#' @export
#' @rdname sensitivity
"specificity.default" <-
  function(
    data,
    reference,
    negative = levels(reference)[-1],
    na.rm = TRUE,
    ...
  ) {
    xtab <- factor_conf_table(
      data,
      reference,
      na.rm,
      "input data must be a factor"
    )
    if (is.null(xtab)) {
      return(NA_real_)
    }
    specificity.table(xtab, negative = negative)
  }

#' @export
#' @rdname sensitivity
"specificity.table" <-
  function(data, negative = rownames(data)[-1], ...) {
    ## "truth" in columns, predictions in rows
    check_conf_table(data)
    positive <- setdiff(colnames(data), negative)
    table_metric(yardstick::spec, data, positive)
  }

#' @export
"specificity.matrix" <-
  function(data, negative = rownames(data)[-1], ...) {
    specificity.table(as.table(data), negative = negative)
  }
