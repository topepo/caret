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
