# This is an basic test that can catch serious kinds of errors
# such as a traceback, change in an upstream package's API, or functions not
# returning the right kind of object. It is fast, and it does not focus on
# assessing predictive accuracy.
skip_on_cran()
data <- data.frame(X1 = 1:100, X2 = 1:100)
data$Y <- with(data, X1 * 1.5 + X2 * 2.5)
models <- unique(subset(modelLookup(), forReg)$model)
writeLines(paste('number of models: ', length(models)))
for (model in models)
{
    msg <- paste('regression model:', model)
    writeLines(msg)
    test_that(msg, {

        fit <- train(Y ~ X1 + X2, data=data, method=model, tuneLength=1)
        expect_is(fit, 'train')

        print_ <- print(fit)
        expect_is(print_, 'matrix')

        predict_ <- predict(fit, newdata=data)
        expect_is(predict_, 'numeric')

        rm(fit)
        rm(predict_)
    })
}

