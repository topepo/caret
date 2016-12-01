context("Data Spliting")

test_that("createTimeSlices works as expected", {
    
    s1 <- createTimeSlices(1:8, 5, horizon = 1)
    s2 <- createTimeSlices(1:8, 5, horizon = 1, skip = 3)
    s3 <- createTimeSlices(1:10, 5, horizon = 1, fixedWindow = FALSE, skip = 3)
    s4 <- createTimeSlices(1:10, 5, horizon = 2, skip = 2)

    expect_equal(s1, list(train = list(Training5 = 1:5, Training6 = 2:6, Training7 = 3:7),
                          test = list(Testing5 = 6L, Testing6 = 7L, Testing7 = 8L)))

    expect_equal(s2, list(train = structure(list(Training5 = 1:5)), 
                          test = structure(list(Testing5 = 6L))))

    expect_equal(s3, list(train = list(Training5 = 1:5, Training9 = 1:9),
                          test = list(Testing5 = 6L, Testing9 = 10L)))

    expect_equal(s4, list(train = list(Training5 = 1:5, Training8 = 4:8),
                          test = list(Testing5 = 6:7, Testing8 = 9:10)))
})
