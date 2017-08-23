print_models <- function(x) {

  cat("\\strong{", x$label, "} (\\code{method = '", x$code, "'})\n\n", sep = "")
  cat("For", paste(as.character(tolower(sort(x$type))), collapse = " and "))
  if(length(x$library) > 0) {
    pkgs <- paste("\\pkg{", as.character(x$library), "}", sep = "")
    pkgs <- listString(pkgs)
    if(length(x$library) > 1) 
      cat(" using packages", pkgs)  else
        cat(" using package", pkgs)
  }
  if(all(x$parameters$parameter == "parameter")) {
    cat(" with no tuning parameters.\n\n")
  } else {
    params <- paste("\\item ", as.character(x$parameters$label), " (\\code{",
                    as.character(x$parameters$parameter), "}, ",
                    as.character(x$parameters$class), ")", 
                    sep = "")
    params <- gsub("#", "Number of ", params, fixed = TRUE)
    cat(" with tuning parameters:\n\\itemize{\n")
    cat(paste(params, collapse = "\n", sep = ""))
    cat("\n}\n\n")
  }
  if(x$note_text != "") 
    cat(x$note_text, "\n")
  cat("\n")
}

md2tex <- function(x) {
  txt <- system2('pandoc', '-t latex', input = x, stdout = TRUE)
  paste0(txt, collapse = " ")
}

library(caret)
library(odfWeave)

mods <- getModelInfo()

labs <- unlist(lapply(mods, function(x) x$label))
mods <- mods[order(labs)]
for(i in seq(along = mods)) {
  mods[[i]]$code <- names(mods)[i]
  if(any(names(mods[[i]]) == "notes")) {
    mods[[i]]$note_text <- md2tex(mods[[i]]$notes)
    mods[[i]]$note_text <- paste("Note:", mods[[i]]$note_text)
    mods[[i]]$note_text <- gsub("texttt", "code", mods[[i]]$note_text)
    mods[[i]]$note_text <- gsub("\\\\textgreater\\{\\}", ">", mods[[i]]$note_text)
    mods[[i]]$note_text <- gsub("\\ ", " ", mods[[i]]$note_text, fixed = TRUE)    
    mods[[i]]$note_text <- gsub("\\_", "_", mods[[i]]$note_text, fixed = TRUE)    
    mods[[i]]$note_text <- gsub("\\$", "$", mods[[i]]$note_text, fixed = TRUE)     
  } else mods[[i]]$note_text <- ""
}



sink("~/github/caret/pkg/caret/man/models.Rd")
cat("\\name{train_model_list}\n",
    "\\alias{train_model_list}\n",
    "\\alias{models}\n",    
    "\\title{A List of Available Models in train}\n",
    "\\description{", 
    "These models are included in the package via wrappers for ",
    "\\code{\\link{train}}. Custom models can also be created. See the URL below.\n\n",
    sep = "")
tt <- lapply(mods, print_models)
cat("}\n")

cat("\\references{``Using your own model in \\code{\\link{train}}'' (\\url{https://topepo.github.io/caret/using-your-own-model-in-train.html})}\n")
cat("\\keyword{models}\n")
sink()