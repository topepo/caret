# Tests for caretTheme (R/caretTheme.R), the lattice theme caret uses for its
# own plots. It takes no arguments and returns a static settings list, so one
# call covers it.

test_that("caretTheme returns a lattice settings list", {
  theme <- caretTheme()
  expect_type(theme, "list")
  # the settings lattice needs for the plot types caret draws
  expect_contains(
    names(theme),
    c(
      "plot.polygon",
      "background",
      "box.rectangle",
      "box.umbrella",
      "dot.line",
      "dot.symbol",
      "plot.line",
      "plot.symbol",
      "regions",
      "strip.shingle"
    )
  )
  # the colour ramp used for level plots
  expect_type(theme$regions$col, "character")
  expect_gt(length(theme$regions$col), 1)
})

test_that("caretTheme can be handed straight to lattice", {
  # trellis.par.set() validates the shape of the settings it is given
  withr::local_pdf(nullfile())
  lattice::trellis.par.set(caretTheme())
  expect_identical(
    lattice::trellis.par.get("plot.line")$col,
    caretTheme()$plot.line$col
  )
})
