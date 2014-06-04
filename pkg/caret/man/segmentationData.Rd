\name{segmentationData}
\docType{data}
\alias{segmentationData}

\title{Cell Body Segmentation}
\description{
Hill, LaPan, Li and Haney (2007) develop models to predict which cells in a high content screen were well segmented. 
The data consists of 119 imaging measurements on 2019. The original analysis used 1009 for training and 1010 as a test set (see the column called \code{Case}). 

The outcome class is contained in a factor variable called \code{Class} with levels "PS" for poorly segmented and "WS" for well segmented.

The raw data used in the paper can be found at the Biomedcentral website. Versions of caret < 4.98 contained the original data. The version now contained in \code{segmentationData} is modified. First, several discrete versions of some of the predictors (with the suffix "Status") were removed. Second, there are several skewed predictors with minimum values of zero (that would benefit from some transformation, such as the log). A constant value of 1 was added to these fields: \code{AvgIntenCh2},  \code{FiberAlign2Ch3},  \code{FiberAlign2Ch4},  \code{SpotFiberCountCh4} and \code{TotalIntenCh2}.

A binary version of the original data is at \url{http://caret.r-forge.r-project.org/segmentationOriginal.RData}.

}

\usage{data(segmentationData)}
\value{
  \item{segmentationData}{data frame of cells}
  }

\source{Hill, LaPan, Li and Haney (2007). Impact of image segmentation on high-content screening data quality for SK-BR-3 cells, 
\emph{BMC Bioinformatics}, Vol. 8, pg. 340, \url{http://www.biomedcentral.com/1471-2105/8/340}.
}

\keyword{datasets}  

