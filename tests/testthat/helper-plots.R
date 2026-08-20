# Helpers for the plotting tests.
#
# Lattice panel and prepanel functions do not run when a trellis object is
# built - only when it is drawn. Printing to a null device is what exercises
# them. The device is opened and closed inside this call so nothing is left
# open for the rest of the test file (an open device makes testthat try to
# snapshot recorded plots).
draw_trellis <- function(object) {
  grDevices::pdf(nullfile())
  on.exit(grDevices::dev.off(), add = TRUE)
  print(object)
  invisible(object)
}

# A minimal stand-in for a train() object fit with `search = "random"`, for
# random_search_plot(). That function reads only the tuning parameters, the
# results table and the metric, and it branches on how many of the varying
# parameters are numeric, so a fabricated object is the cheapest way to reach
# each branch: real random searches over that many parameter shapes would mean
# a fit per branch.
random_search_obj <- function(num = 1, other = 0, n = 8, constant = FALSE) {
  num_names <- if (num > 0) paste0("n", seq_len(num)) else character(0)
  other_names <- if (other > 0) paste0("o", seq_len(other)) else character(0)
  p_names <- c(num_names, other_names)

  res <- data.frame(RMSE = seq(1, 2, length.out = n))
  for (i in seq_along(num_names)) {
    res[[num_names[i]]] <- if (constant) 1 else seq_len(n) * i
  }
  for (i in seq_along(other_names)) {
    res[[other_names[i]]] <- if (constant) {
      "only"
    } else {
      rep(paste0("lev", seq_len(2)), length.out = n)
    }
  }

  structure(
    list(
      method = "fake",
      metric = "RMSE",
      maximize = FALSE,
      results = res,
      control = list(search = "random"),
      modelInfo = list(
        parameters = data.frame(
          parameter = p_names,
          class = c(rep("numeric", num), rep("character", other)),
          label = paste("Label", p_names),
          stringsAsFactors = FALSE
        )
      )
    ),
    class = "train"
  )
}
