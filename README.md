# RandomThinning

This is an R package that provides Monte Carlo tests for periodic components of known length in time-series. It is particularly useful in testing for the presence of short signals in noisy data. The R package provides functions that perform the test, estimate relevant statistical properties of a given time-series and that estimate the power of the test (based on properties of a given time-series).

It uses some c++ code (via the Rcpp interface) for some of the more CPU-intensive parts of the calculations. 
