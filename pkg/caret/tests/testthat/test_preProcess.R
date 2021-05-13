context('Test preProcess')

# Helper function
check.medianImpute <- function(x) {
	# Reference column medians for checks
	med <- apply(x, 2, median, na.rm=TRUE)
	anyEmptyColumn <- any(is.na(med))
	med[is.na(med)] <- 0

	if(anyEmptyColumn) {
		expect_warning(
			pp <- preProcess(x, method = "medianImpute"),
			"never filled"
		)
	} else {
		pp <- preProcess(x, method = "medianImpute")
	}
	expect_equal(pp$median, med)

	x.filled <- predict(pp, newdata=x)
	expect_false(any(is.na(x.filled)))
	expect_equal(x[!is.na(x)], x.filled[!is.na(x)])

	med.filled <- apply(x.filled, 2, median)
	expect_equal(med.filled, med)
}

# Tested data matrix
set.seed(1)
x <- matrix(rnorm(20, mean=10,sd=5), nrow=4)
x[2,1] <- x[3,4] <- x[2,5] <- x[4,5] <- NA
x[,3] <- NA

colnames(x) <- paste0("Var.",1:ncol(x))

test_that("median Impute works for matrix with named columns", {
	check.medianImpute(x)
})

test_that("median Impute works for data.frames", {
	check.medianImpute(as.data.frame(x, stringsAsFactors = TRUE))
})

test_that("correlation filter", {
  expect_equal(
    preProcess(iris, "corr")$method,
    list(ignore = "Species", remove = "Petal.Length")
  )
})


