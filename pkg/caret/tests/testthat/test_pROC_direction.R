library(caret)

context('Testing pROC direction')

test_that('rocPerCol returns AUC < 0.5 with direction = "<"', {
	#skip_on_cran()
	skip_if_not_installed('pROC')
	
	set.seed(42) 
	dat <- twoClassSim(200, linearVars = 1)
	
	auto.auc <- as.numeric(pROC::roc(dat$Class, dat$Linear1, direction = "auto")$auc)
	fixed.auc <- as.numeric(pROC::roc(dat$Class, dat$Linear1, direction = "<")$auc)
	tested.auc <- as.numeric(caret:::rocPerCol(dat$Linear1, dat$Class))
	
	# tested.auc should equal tested.auc  now
	expect_equal(tested.auc, fixed.auc)
	
	# Also it has been hand-checked to be < 0.5 with this seed (0.4875)
	expect_lt(tested.auc, 0.5)
	
	# And it should be lower than the "auto" auc
	expect_lt(tested.auc, auto.auc)
})
