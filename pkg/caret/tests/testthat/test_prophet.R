# test fixes to caret to allow data.frames in the tuneGrid
# 
# Author: Willem Ligtenberg
###############################################################################

context("prophet")

library(caret)
library(dplyr)
library(data.table)
library(prophet)
library(timeDate)

#' Caret wrapper function for a Prophet model
#' @return List that complies with caret requirements
#' @author Willem Ligtenberg <willem.ligtenberg@cz.nl>
#' @export
prophet_caret <- function() {
  # See: https://topepo.github.io/caret/using-your-own-model-in-train.html
  library <- c("prophet")
  type <- "Regression"
  parameters <- data.frame(
      parameter = c("growth", "changepoints", "n.changepoints", "changepoint.range", "yearly.seasonality", "weekly.seasonality", "daily.seasonality", "holidays", "seasonality.mode", "seasonality.prior.scale", "holidays.prior.scale", "changepoint.prior.scale", "mcmc.samples", "interval.width", "uncertainty.samples"), 
      class = c("character", "Date", "numeric", "numeric", "logical", "logical", "logical", "list", "character", "numeric", "numeric", "numeric", "integer", "numeric", "integer"), 
      label = c("growth", "changepoints", "n.changepoints", "changepoint.range", "yearly.seasonality", "weekly.seasonality", "daily.seasonality", "holidays", "seasonality.mode", "seasonality.prior.scale", "holidays.prior.scale", "changepoint.prior.scale", "mcmc.samples", "interval.width", "uncertainty.samples"))
  grid <- function(x, y, len = NULL, search = "grid") {
    data.frame(
        growth = "linear", 
        changepoints = NA, 
        n.changepoints = 25, 
        changepoint.range = 0.8,
        yearly.seasonality = "auto",
        weekly.seasonality = "auto",
        daily.seasonality = "auto",
        holidays = NA,
        seasonality.mode = "additive",
        seasonality.prior.scale = 10,
        holidays.prior.scale = 10, 
        changepoint.prior.scale = 0.05,
        mcmc.samples = 0,
        interval.width = 0.9,
        uncertainty.samples = 1000
    )
  }
  fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
    prophet_data <- as.data.frame(cbind(x, y))
    colnames(prophet_data) <- c("ds", "y")
    prophet_data$ds <- as.Date(prophet_data$ds)
    prophet(
        df = prophet_data, 
        growth = param$growth, 
        changepoints = if(is.na(param$changepoints)){NULL} else{param$changepoints},
        n.changepoints = param$n.changepoints,
#        changepoint.range = param$changepoint.range,
        yearly.seasonality = param$yearly.seasonality,
        weekly.seasonality = param$weekly.seasonality,
        daily.seasonality = param$daily.seasonality,
        holidays = if(is.na(param$holidays)){NULL} else{as.data.table(param$holidays)}, 
#        seasonality.mode = param$seasonality.mode,
        seasonality.prior.scale = param$seasonality.prior.scale,
        holidays.prior.scale = param$holidays.prior.scale,
        changepoint.prior.scale = param$changepoint.prior.scale,
        mcmc.samples = param$mcmc.samples,
        interval.width = param$interval.width,
        uncertainty.samples = param$uncertainty.samples, ...)
  }
  prophet_predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL) {
    prophet_data <- as.data.frame(newdata)
    colnames(prophet_data) <- c("ds")
#		return(predict(modelFit, prophet_data))
    return(pmax(predict(modelFit, prophet_data)$yhat, 0))
  }
  sort <- function(x){
    return(x)
  }
  label <- "Prophet"
  
  return(
      list(
          library = library, 
          type = type, 
          parameters = parameters, 
          grid = grid, 
          fit = fit, 
          predict = prophet_predict, 
          prob = NULL, 
          sort = sort,
          label = label))
}


holidays_list <- list(
    rbind(
        data.table(holiday = "New Year",
            ds = as.Date(timeDate::NewYearsDay(seq(1970, 2050))),
            lower_window = -5,
            upper_window = 0),
        data.table(holiday = "Easter",
            ds = as.Date(timeDate::Easter(seq(1970, 2050))),
            lower_window = 0,
            upper_window = 2),
        data.table(holiday = "Ascension",
            ds = as.Date(timeDate::Ascension(seq(1970, 2050))),
            lower_window = 0,
            upper_window = 0),
        data.table(holiday = "Pentecost",
            ds = as.Date(timeDate::Pentecost(seq(1970, 2050))),
            lower_window = 0,
            upper_window = 1),
        data.table(holiday = "Christmas",
            ds = as.Date(timeDate::ChristmasDay(seq(1970, 2050))),
            lower_window = 0,
            upper_window = 1)
    ),
    rbind(
        data.table(holiday = "New Year",
            ds = as.Date(timeDate::NewYearsDay(seq(1970, 2050))),
            lower_window = -5,
            upper_window = 1),
        data.table(holiday = "Easter",
            ds = as.Date(timeDate::Easter(seq(1970, 2050))),
            lower_window = 0,
            upper_window = 3),
        data.table(holiday = "Ascension",
            ds = as.Date(timeDate::Ascension(seq(1970, 2050))),
            lower_window = 0,
            upper_window = 1),
        data.table(holiday = "Pentecost",
            ds = as.Date(timeDate::Pentecost(seq(1970, 2050))),
            lower_window = 0,
            upper_window = 2),
        data.table(holiday = "Christmas",
            ds = as.Date(timeDate::ChristmasDay(seq(1970, 2050))),
            lower_window = 0,
            upper_window = 2)
    ))

changepoints_list <- list(
    NA,
    c(as.Date("2018-06-06"), as.Date("2018-05-05")))

grid <- CJ(
    growth = c("linear"), 
    changepoints = seq_along(changepoints_list), 
    n.changepoints = c(10), 
    changepoint.range = 0.8,
    yearly.seasonality = "auto",
    weekly.seasonality = "auto",
    daily.seasonality = FALSE,
    holidays = seq_along(holidays_list),
    seasonality.mode = "additive",
    seasonality.prior.scale = 10,
    holidays.prior.scale = c(10), 
    changepoint.prior.scale = 0.05,
    mcmc.samples = c(0),
    interval.width = 0.9,
    uncertainty.samples = 1000)

grid[, holidays := lapply(holidays, function(x) {holidays_list[[x]]})]
grid[, changepoints := lapply(changepoints, function(x) {changepoints_list[[x]]})]

tc <- trainControl(
    method = "timeslice",
    initialWindow = 730,
    horizon = 365,
    fixedWindow = FALSE)

time_series_data <- data.frame(
    date = seq(as.Date("2016-01-01"), as.Date("2018-12-31"), by = 1),
    y = rnorm((3*365)+1, 100, 10))

model <- caret::train(
    x = data.frame(x = time_series_data$date),
    y = time_series_data$y,
    method = prophet_caret(), 
    maximize = FALSE, 
    trControl = tc,
    tuneGrid = grid)


p <- ggplot(model)
print(p)
print(model)
update(model)
levels(model)
densityplot(model)
histogram(model)
stripplot(model)
predictors(model)
summary(model)
residuals(model)
fitted(model)
prediction_timeframe <- data.frame(ds = seq.Date(from = as.Date("2019-01-01"), to = as.Date("2019-03-01"), by = "day"))
predict(model$finalModel, prediction_timeframe)