\name{caret-internal}
\title{Internal Functions}
\alias{createModel}
\alias{resampleWrapper}
\alias{sortImp}
\alias{caretTheme}
\alias{progress}
\alias{hasTerms}
\alias{predictionFunction}
\alias{probFunction}
\alias{expandParameters}
\alias{flatTable}
\alias{MeanSD}
\alias{sbfIter}
\alias{gamFormula}
\alias{bagEarthStats}
\alias{cforestStats}
\alias{ipredStats}
\alias{rfStats}
\alias{well_numbered}

\description{Internal functions}
\usage{
createModel(x, y, wts, method, tuneValue, obsLevels, 
	        pp = NULL, last = FALSE, classProbs, ...)
predictionFunction(method, modelFit, newdata, 
	               preProc = NULL, param = NULL)
probFunction(method, modelFit, newdata, 
	         preProc = NULL, param = NULL)
flatTable(pred, obs) 
expandParameters(fixed, seq) 
MeanSD(x, exclude = NULL) 
sortImp(object, top)
resampleWrapper(x, ind)
caretTheme()
rocPoint(cutoff, x, y, positive)
ipredStats(x)
rfStats(x)
bagEarthStats(x)
cforestStats(x)
progress(x, names, iter, start = TRUE)
hasTerms(x)
sbfIter(x, y, testX, testY, sbfControl = sbfControl(), ...)
gamFormula(data, smoother = "s", cut = 8, y = "y") 
well_numbered(prefix, items) 
}
\author{Max Kuhn, but \code{caretTheme} uses an expanded grid of the "Blues" palette designed by Cynthia Brewer and Mark Harrower}


\keyword{internal} 
