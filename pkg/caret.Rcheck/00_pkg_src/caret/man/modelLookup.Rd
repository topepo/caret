\name{modelLookup}
\alias{modelLookup}
\alias{getModelInfo}
\alias{checkInstall}
\title{
Tools for Models Available in \code{train}
}
\description{
These function show information about models and packages that are accessible via \code{\link{train}}
}
\usage{
modelLookup(model = NULL)

getModelInfo(model = NULL, regex = TRUE, ...)

checkInstall(pkg)
}
\arguments{
  \item{model}{
a character string associated with the \code{method} argument of \code{\link{train}}. If no value is 
passed, all models are returned. For \code{getModelInfo}, regular expressions can be used.
}
  \item{regex}{
a logical: should a regular expressions be used? If \code{FALSE}, a simple match is conducted against the whole name of the model. 
}
  \item{pkg}{
a character string of package names. 
}
  \item{...}{
options to pass to \code{\link[base]{grepl}}
}

}
\details{
\code{modelLookup} is good for getting information related to the tuning parameters for a model. \code{getModelInfo} will return all the functions and metadata associated with a model. Both of these functions will only search within the models bundled in this package. 

\code{checkInstall} will check to see if packages are installed. If they are not and the session is interactive, an option is given to install the packages using \code{\link[utils]{install.packages}} using that functions default arguments (the missing packages are listed if you would like to install them with other options). If the session is not interactive, an error is thrown. 

}
\value{
\code{modelLookup} produces a data frame with columns
  \item{model }{a character string for the model code}
  \item{parameter }{the tuning parameter name}
  \item{label }{a tuning parameter label (used in plots)}
  \item{forReg }{a logical; can the model be used for regression?}
  \item{forClass }{a logical; can the model be used for classification?}
  \item{probModel }{a logical; does the model produce class probabilities?}

\code{getModelInfo} returns a list containing one or more lists of the standard model information. 

\code{checkInstall} returns not value. 

}

\note{The column \code{seq} is no longer included in the output of \code{modelLookup}.}

\author{
Max Kuhn
}


\seealso{
\code{\link{train}}, \code{\link[utils]{install.packages}}, \code{\link[base]{grepl}}
}
\examples{
modelLookup()
modelLookup("gbm")

getModelInfo("pls")
getModelInfo("^pls")
getModelInfo("pls", regex = FALSE)

\dontrun{
checkInstall(getModelInfo("pls")$library)
}
}

\keyword{ utilities }
