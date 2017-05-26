# RandomThinning

This is an R package that provides Monte Carlo tests for periodic components of known length in time-series. It is particularly useful in testing for the presence of short signals in noisy data. The R package provides functions that perform the test, estimate relevant statistical properties of a given time-series and that estimate the power of the test (based on properties of a given time-series).

It uses some c++ code (via the Rcpp interface) for some of the more CPU-intensive parts of the calculations. 

## Installation

Following [Karl Broman's instructions](http://kbroman.org/pkg_primer/pages/github.html), you can install this package with the following steps:

1. Install the [devtools](https://cran.r-project.org/web/packages/devtools/) package - you can skip this step if the package is already installed. In the R console, run:

```
install.packages("devtools")
```

2. Load the [devtools](https://cran.r-project.org/web/packages/devtools/) package:

```
library(devtools)
```

3. Install following the syntax `install_github("author/package")`:

```
install_github("JeremySilver/RandomThinning")
```

If this doesn't work for you, then there is longer list of [instructions](https://cran.r-project.org/web/packages/githubinstall/vignettes/githubinstall.html) to install R packages from github, which may be worth trying.
