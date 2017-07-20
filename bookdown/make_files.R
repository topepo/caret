library(bookdown)
library(doMC)
library(parallel)
registerDoMC(cores = detectCores() - 1)

render_book("index.Rmd", "bookdown::gitbook")

if(!interactive()) q("no")
