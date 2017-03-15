context('Minimal Tests')
stats <- caret:::basic2x2Stats(factor(0:1), factor(0:1), pos='1', neg='0')
expect_equal(stats[['Sensitivity']], 1)
expect_equal(stats[['Specificity']], 1)
expect_equal(stats[['Pos Pred Value']], 1)
expect_equal(stats[['Neg Pred Value']], 1)

test_that("resampling method 'none' doesn't conflict with default tuneLength", {
    
    data(BloodBrain)
    
    expect_error(train(bbbDescr, logBBB,
                       method = "earth",
                       tuneLength = 2,
                       trControl = trainControl(method = "none")))

    expect_error(train(bbbDescr, logBBB,
                       method = "earth",
                       tuneLength = 2, 
                       trControl = trainControl(method = "none")))

    expect_error(train(mpg ~ cyl + disp, data = mtcars,
                       method = "gam",
                       tuneLength = 2, 
                       trControl = trainControl(method = "none")))

})

