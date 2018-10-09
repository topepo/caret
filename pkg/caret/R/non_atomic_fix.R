# Fix sorting issues for non-atomic object by temporarely replacing them
# 
# Author: Willem Ligtenberg <willem.ligtenberg@cz.nl>
###############################################################################

#' Replace non-atomic columns with numerical values to allow sorting
#' @param tmp data.frame which might contain non-atomic columns 
#' @return list with result and replacement. 
#' result contains the adapted data.frame, 
#' replacement contains information to undo the replacement later
#' @import data.table
#' @author Willem Ligtenberg
replace_non_atomic_values <- function(tmp){
  # TODO We expect a data.frame test for it!
  non_atomic_columns <- colnames(tmp)[!sapply(tmp, is.atomic)]
  replacement <- list()
  for(column_name in non_atomic_columns){
    replacement_values <- as.list(unique(tmp[, column_name]))
    # Some magic to replace every value by its index
    tmp[, column_name] <- sapply(tmp[, column_name], 
        function(x, replacement_values){
          which(sapply(replacement_values, function(x, y){identical(x, y)}, x = x))}, replacement_values = replacement_values)
    replacement[[column_name]] <- replacement_values
  }
  return(
      list(
          "result" = tmp,
          "replacement" = replacement))
}

#' Undo replace non-atomic columsn with numerical values
#' @param tmp data.frame that had non-atomic columns replaced with numerical values
#' @param replacement the other result of replace_non_atomic_values to allow undo of that operation
#' @return data.frame with the numerical values replaced by their original values
#' 
#' @author Willem Ligtenberg
restore_non_atomic_values <- function(tmp, replacement){
  # TODO check expectations
  for(column_name in names(replacement)){
    # Roll back the changes we made (we do seem to lose some class types, but I think that is acceptable
    tmp_column <- tmp[, column_name, drop = FALSE]
    tmp[, column_name] <- NULL
    # Yuck, eval, but for some reason this works, and out[, column_name] not...
    eval(parse(text = paste0("tmp$", column_name, " <- apply(tmp_column, 1, function(x, replacement_values){replacement_values[[x]]}, replacement_values = replacement[[column_name]])")))
  }
  return(tmp)
}