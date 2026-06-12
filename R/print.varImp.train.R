#' @method print varImp.train
#' @export
"print.varImp.train" <-
function(x, top = min(20, dim(x$importance)[1]), digits = max(3, getOption("digits") - 3), ...)
{
   cat(x$model, "variable importance\n\n")

   printObj <- sortImp(x, top)

   if(dim(x$importance)[2] > 2)
     cat("  variables are sorted by maximum importance across the classes\n")
     
   if(top < dim(x$importance)[1]) 
      cat("  only ", top, " most important variables shown (out of ", dim(x$importance)[1], ")\n\n", sep = "")
    
   if(dim(printObj)[2] == 2)
   {
      printObj <- printObj[,1,drop = FALSE]
      names(printObj) <- "Importance"
   }
   print(printObj, digits = digits, ...)
   invisible(x)
}

