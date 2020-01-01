# This is an extremely basic test that would catch serious kinds of errors
# such as the bagEarth() not returning the right kind of object, that one of
# the functions (bagEarth, format, predict) crash during normal usage, or that
# bagEarth cannot model a simplistic kind of linear equation.
context("earth")
test_that('bagEarth simple regression', {
    skip_on_cran()
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

test_that('bagEarth simple classification', {
    skip_on_cran()
    data <- twoClassSim(n=1000)
    fit <- bagEarth(Class ~ ., data=data, B=3, glm=list(family=binomial))
    expect_that(format(fit, cat=FALSE), is_a('character'))
    expect_that(fit, is_a('bagEarth'))

    pred_response <- predict(fit, newdata=data)
    expect_is(pred_response, "factor")
    expect_equal(length(pred_response), nrow(data))

    pred_class <- predict(fit, newdata=data, type="class")
    expect_is(pred_class, "factor")
    expect_equal(length(pred_class), 1000)

    pred_prob <- predict(fit, newdata=data, type="prob")
    expect_is(pred_prob, "data.frame")
    expect_equal(ncol(pred_prob), 2)
    expect_equal(nrow(pred_prob), 1000)
    expect_true(0 <= min(pred_prob))
    expect_true(max(pred_prob) <= 1)
})
