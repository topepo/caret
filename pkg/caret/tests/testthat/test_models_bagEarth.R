# This is an extremely basic test that would catch serious kinds of errors
# such as the bagEarth() not returning the right kind of object, that one of
# the functions (bagEarth, format, predict) crash during normal usage, or that
# bagEarth cannot model a simplistic kind of linear equation.
test_that('bagEarth simple regression', {
    data <- data.frame(X = 1:100)
    data$Y <- data$X * 2
    data$training <- data$X %% 2
    fit <- bagEarth(Y ~ X, data=data[1==data$training,], B=3)
    expect_that(format(fit, cat=FALSE), is_a('character'))
    expect_that(fit, is_a('bagEarth'))
    data$pred <- predict(fit, newdata=data)
    data$resid <- with(data, Y - pred)
    mae <- mean(abs(data$resid))
    expect_equal(mae, 0)
})
