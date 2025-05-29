test_that("createDataPartition basic functionality with factor y", {
  withr::local_seed(123)
  y_factor <- factor(rep(c("A", "B", "C"), each = 10L))
  partitions <- createDataPartition(y_factor, times = 2, p = 0.2)
  expect_identical(
    partitions,
    list(
      Resample1 = c(3L, 10L, 12L, 20L, 25:26),
      Resample2 = c(4L, 6L, 15L, 19L, 23L, 29L)
    )
  )
})

test_that("createDataPartition with list = FALSE", {
  withr::local_seed(123)
  partitions_matrix <- createDataPartition(rnorm(30L), times = 3, p = 0.7, list = FALSE)
  expect_identical(dim(partitions_matrix), c(22L, 3L))
})

test_that("createDataPartition with numeric y", {
  withr::local_seed(123)
  partitions <- createDataPartition(rnorm(20L), times = 1, p = 0.1)
  expect_identical(partitions, list(Resample1 = c(3L, 5L, 8L, 20L)))
})

test_that("createDataPartition handles NA values in factor y", {
  withr::local_seed(123)
  y_factor_na <- factor(rep(c("A", "B", NA), c(10L, 10L, 5L)))

  partitions_na <- createDataPartition(y_factor_na, times = 1, p = 0.2)
  expect_identical(partitions_na, list(Resample1 = c(3L, 10L, 12L, 20L, 23L)))
})

test_that("createDataPartition handles character y with NA and string 'NA'", {
  withr::local_seed(456)
  y_string_mix <-
    c(rep("Apple", 5L), rep("Banana", 5L), NA, NA, "NA", "Cherry", "NA", "NA", "NA")

  expect_warning(
    expect_identical(
      createDataPartition(y_string_mix, times = 1, p = 0.5),
      list(Resample1 = c(1L, 3L, 5L, 8:11, 13:15))
    ),
    "Some classes have a single record ( Cherry ) and these will be selected for the sample",
    fixed = TRUE
  )
})

test_that("createDataPartition with y having very few data points", {
  expect_error(createDataPartition(c(1), times = 1, p = 0.5), "y must have at least 2 data points")
  
  withr::local_seed(987498)

  part_small <- createDataPartition(c(1, 2), times = 1, p = 0.5)
  expect_identical(part_small, list(Resample1 = 1L))

  expect_warning(
    expect_identical(
      createDataPartition(factor(c("A", "B")), times = 1, p = 0.5),
      list(Resample1 = 1:2)
    ),
    "Some classes have a single record ( A, B ) and these will be selected for the sample",
    fixed = TRUE
  )
})

test_that("createDataPartition warnings for no-record classes", {
  withr::local_seed(984)

  y_no_rec_factor <- factor(c("A", "A"), levels = c("A", "B", "C"))
  expect_warning(
    expect_identical(
      createDataPartition(y_no_rec_factor, p = 0.5),
      list(Resample1 = 2L)
    ),
    "Some classes have no records ( B, C ) and these will be ignored",
    fixed = TRUE
  )
})
