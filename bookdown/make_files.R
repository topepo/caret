library(bookdown)
library(parallel)
library(doParallel)
cl <- makePSOCKcluster(parallel::detectCores(logical = TRUE))
registerDoParallel(cl)

render_book("index.Rmd", "bookdown::gitbook")

if(!interactive()) q("no")
