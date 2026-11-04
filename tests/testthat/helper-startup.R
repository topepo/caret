# Quietly attach packages that are imported/depended on by caret and used
# across many tests (so individual test files don't need to load them).
suppressPackageStartupMessages({
  library(recipes)
  library(ggplot2)
  library(lattice)
})
