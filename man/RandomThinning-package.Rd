\name{RandomThinning-package}
\alias{RandomThinning-package}
\alias{RandomThinning}
\docType{package}
\title{
Random thinning tests and auxiliary functions
}
\description{
Monte Carlo tests for weak signals in time-series
}
\details{
  This package provides the functions \code{randomThinningTest} (which
  performs the test in question), \code{calculateTSstatistics} (which
  calculates relevant properties of the time-series),
  \code{calculatePower} (which estimates the power of the test for a
  given data-set) and \code{rangeTest} (the statistic underlying these
  tests). 
}
\author{
Jeremy Silver \email{jeremy.silver@unimelb.edu.au}
}
\references{
  These tests first appeared in Earl et al. 2015:

  Earl, N., Simmonds, I. & Tapper, N. (2015) Weekly cycles of global
  fires - Associations with religion, wealth and culture, and insights
  into anthropogenic influences on global climate. Geophysical Research
  Letters. 42 (21): 9579--9589
}
\keyword{ package }
\keyword{ htest }
\keyword{ ts }
\keyword{ nonparametric }
\examples{
## Generate a random time-series:
##  - length = 1000
##  - signal period = 8
##  - signal to noise ratio = 0.2
set.seed(42)

x <- rep(rnorm(8)*0.2,length.out = 1000) + rnorm(1000)
randomThinningTest(ts = x, p = 8, n = 1000,
                   fr = 0.025, returnExtraInfo = TRUE)

## compare with periods either too short or too long:
randomThinningTest(ts = x, p = 7, n = 1000, fr = 0.025)
randomThinningTest(ts = x, p = 9, n = 1000, fr = 0.025)
}
