library(bookdown)
# library(parallel)
# library(doParallel)
# cl <- makeForkCluster(parallel::detectCores(logical = TRUE))
# registerDoParallel(cl)

library(doMC)
registerDoMC(cores = parallel::detectCores(logical = TRUE))

render_book("index.Rmd", "bookdown::gitbook")

if (!interactive()) 
  q("no")
