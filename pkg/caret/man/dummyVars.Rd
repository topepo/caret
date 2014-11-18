\name{dummyVars}
\alias{dummyVars}
\alias{dummyVars.default}
\alias{predict.dummyVars}
\alias{contr.dummy}
\alias{contr.ltfr}


\title{
Create A Full Set of Dummy Variables
}
\description{
\code{dummyVars} creates a full set of dummy variables (i.e. less than full rank parameterization) 
}
\usage{
dummyVars(formula, ...)

\method{dummyVars}{default}(formula, data, sep = ".", levelsOnly = FALSE, 
          fullRank = FALSE, ...)

\method{predict}{dummyVars}(object, newdata, na.action = na.pass, ...)

contr.dummy(n, ...) ## DEPRECATED

contr.ltfr(n, contrasts = TRUE, sparse = FALSE)
}
\arguments{
  \item{formula}{
An appropriate R model formula, see References
}
  \item{data}{
A data frame with the predictors of interest
}
  \item{sep}{
An optional separator between factor variable names and their levels. Use \code{sep = NULL} for no separator (i.e. normal behavior of \code{\link[stats]{model.matrix}} as shown in the Details section)
}
  \item{levelsOnly}{
A logical; \code{TRUE} means to completely remove the variable names from the column names
}
  \item{fullRank}{
A logical; should a full rank or less than full rank parameterization be used? If \code{TRUE}, factors are encoded to be consistent with \code{\link[stats]{model.matrix}} and the resulting there are no linear dependencies induced between the columns.
}
  \item{object}{
An object of class \code{dummyVars}
}
  \item{newdata}{
A data frame with the required columns
}
  \item{na.action}{
A function determining what should be done with missing values in \code{newdata}. The default is to predict \code{NA}.
}
  \item{n}{
A vector of levels for a factor, or the number of levels.
}
  \item{contrasts}{
A logical indicating whether contrasts should be computed.
}
  \item{sparse}{
A logical indicating if the result should be sparse.
}
  \item{...}{additional arguments to be passed to other methods}
}
\details{
Most of the \code{\link[stats]{contrasts}} functions in R produce full rank parameterizations of the predictor data. For example, \code{\link[stats]{contr.treatment}} creates a reference cell in the data and defines dummy variables for all factor levels except those in the reference cell. For example, if a factor with 5 levels is used in a model formula alone,  \code{\link[stats]{contr.treatment}} creates columns for the intercept and all the factor levels except the first level of the factor. For the data in the Example section below, this would produce:
\preformatted{
  (Intercept) dayTue dayWed dayThu dayFri daySat daySun
1           1      1      0      0      0      0      0
2           1      1      0      0      0      0      0
3           1      1      0      0      0      0      0
4           1      0      0      1      0      0      0
5           1      0      0      1      0      0      0
6           1      0      0      0      0      0      0
7           1      0      1      0      0      0      0
8           1      0      1      0      0      0      0
9           1      0      0      0      0      0      0
} 

In some situations, there may be a need for dummy variables for all the levels of the factor. For the same example:
\preformatted{
  dayMon dayTue dayWed dayThu dayFri daySat daySun
1      0      1      0      0      0      0      0
2      0      1      0      0      0      0      0
3      0      1      0      0      0      0      0
4      0      0      0      1      0      0      0
5      0      0      0      1      0      0      0
6      1      0      0      0      0      0      0
7      0      0      1      0      0      0      0
8      0      0      1      0      0      0      0
9      1      0      0      0      0      0      0
}

Given a formula and initial data set, the class \code{dummyVars} gathers all the information needed to produce a full set of dummy variables for any data set. It uses \code{contr.ltfr} as the base function to do this.

}
\value{
The output of \code{dummyVars} is a list of class 'dummyVars' with elements
\item{call }{the function call}
\item{form }{the model formula}
\item{vars }{names of all the variables in the model}
\item{facVars }{names of all the factor variables in the model}
\item{lvls }{levels of any factor variables}
\item{sep }{\code{NULL} or a character separator}
\item{terms }{the \code{\link[stats]{terms.formula}} object}
\item{levelsOnly }{a logical}

The \code{predict} function produces a data frame.

\code{contr.ltfr} generates a design matrix.
}

\author{
\code{contr.ltfr} is a small modification of \code{\link[stats]{contr.treatment}} by Max Kuhn
}

\references{
\url{http://cran.r-project.org/doc/manuals/R-intro.html#Formulae-for-statistical-models}
}

\seealso{
\code{\link[stats]{model.matrix}}, \code{\link[stats]{contrasts}}, \code{\link[stats]{formula}}
}
\examples{

when <- data.frame(time = c("afternoon", "night", "afternoon",
                            "morning", "morning", "morning",
                            "morning", "afternoon", "afternoon"),
                   day = c("Mon", "Mon", "Mon",
                           "Wed", "Wed", "Fri",
                           "Sat", "Sat", "Fri"))

levels(when$time) <- c("morning", "afternoon", "night")
levels(when$day) <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

## Default behavior:
model.matrix(~day, when)

mainEffects <- dummyVars(~ day + time, data = when)
mainEffects
predict(mainEffects, when[1:3,])

when2 <- when
when2[1, 1] <- NA
predict(mainEffects, when2[1:3,])
predict(mainEffects, when2[1:3,], na.action = na.omit)


interactionModel <- dummyVars(~ day + time + day:time,
                              data = when,
                              sep = ".")
predict(interactionModel, when[1:3,])

noNames <- dummyVars(~ day + time + day:time,
                     data = when,
                     levelsOnly = TRUE)
predict(noNames, when)
}

\keyword{ models}

 
