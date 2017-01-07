start_time <- proc.time()[3]

library(bookdown)
library(doMC)
library(parallel)
registerDoMC(cores = detectCores() - 1)

render_book("index.Rmd", "bookdown::gitbook")

stop_time <- proc.time()[3]

cat(
  "Execution took", 
  round((stop_time-start_time)/60/60, 1),
  "hours\n"
)

if(!interactive()) q("no")
