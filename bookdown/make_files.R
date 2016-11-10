library(bookdown)

render_book("index.Rmd", "bookdown::gitbook")

if(!interactive()) q("no")
