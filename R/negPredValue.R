#' @rdname sensitivity
#' @export
negPredValue <-
  function(data, ...) {
    UseMethod("negPredValue")
  }

#' @rdname sensitivity
#' @export
"negPredValue.default" <-
  function(
    data,
    reference,
    negative = levels(reference)[2],
    prevalence = NULL,
    ...
  ) {
    xtab <- factor_conf_table(
      data,
      reference,
      TRUE,
      "input data must be a factor"
    )
    if (is.null(xtab)) {
      return(NA_real_)
    }
    negPredValue.table(xtab, negative = negative, prevalence = prevalence)
  }

#' @rdname sensitivity
#' @export
"negPredValue.table" <-
  function(data, negative = rownames(data)[-1], prevalence = NULL, ...) {
    ## "truth" in columns, predictions in rows
    check_conf_table(data)
    positive <- setdiff(colnames(data), negative)
    table_metric(yardstick::npv, data, positive, prevalence = prevalence)
  }

#' @rdname sensitivity
#' @export
"negPredValue.matrix" <-
  function(data, negative = rownames(data)[-1], prevalence = NULL, ...) {
    negPredValue.table(
      as.table(data),
      negative = negative,
      prevalence = prevalence
    )
  }
