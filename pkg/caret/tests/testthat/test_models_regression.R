# This is an basic test that can catch serious kinds of errors
# such as functions not returning the right kind of object.
# It is fast, and it does not focus on assessing predictive accuracy.
test_that('regression models with train()', {
    data <- data.frame(X1 = 1:100, X2 = 1:100)
    data$Y <- with(data, X1 * 1.5 + X2 * 2.5)
    models <- unique(subset(modelLookup(), forReg)$model)
    writeLines(paste('number of models: ', length(models)))
    for (model in models)
    {
        writeLines(paste('testing regression model:', model))
        fit <- train(Y ~ X1 + X2, data=data, method=model, tuneLength=1)
        expect_is(fit, 'train')

        print_ <- print(fit)
        expect_is(print_, 'matrix')

        pred <- predict(fit, newdata=data)
        expect_is(pred, 'numeric')

        rm(fit)
        rm(pred)
    }
})
