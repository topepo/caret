\name{panel.needle}
\alias{panel.needle}
\title{Needle Plot Lattice Panel}
\description{
A variation of \code{panel.dotplot} that plots horizontal lines from zero to the data point.
}
\usage{
panel.needle(x, y, horizontal = TRUE,
              pch, col, lty, lwd, 
              col.line, levels.fos,
              groups = NULL,
              \dots)
}
\arguments{
  \item{x,y}{ variables to be plotted in the panel. Typically y is the
    'factor' }
  \item{horizontal}{ logical. If FALSE, the plot is `transposed' in the
    sense that the behaviours of x and y are switched. x is now
    the `factor'.  Interpretation of other arguments change
    accordingly. See documentation of \code{bwplot} for a fuller
    explanation.
  }
  \item{pch, col, lty, lwd, col.line}{ graphical parameters}
  \item{levels.fos}{ locations where reference lines will be drawn }
  \item{groups}{ grouping variable (affects graphical parameters)}
  \item{\dots}{
    extra parameters, passed to \code{panel.xyplot} which is responsible
    for drawing the foreground points (\code{panel.dotplot} only draws
    the background reference lines).
  }
}
\details{
  Creates (possibly grouped) needleplot of \code{x} against \code{y} or
  vice versa
}
\seealso{\code{\link{dotplot}}}
\author{Max Kuhn, based on \code{\link[lattice]{panel.dotplot}} by  Deepayan Sarkar}
\keyword{graphs}
