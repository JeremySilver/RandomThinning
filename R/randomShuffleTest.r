#' Perform the random shuffle test
#'
#' @param ts A time-series (a numeric vector)
#' @param its Indices of which bin the values of x correspond to (a numeric vector of integers between 1 and p). This is optional, and taken to be cyclical by default.
#' @param blockLength Shuffle the order of blocks of this length (a positive integer between 1 and less than \code{length(ts)} ). If \code{blockLength} is equal to 1, then the series is shuffled randomly. If blockLength is greater than 1, blocks of this length will be shuffled. The \code{blockLength} should be much less than \code{length(ts)} and should be co-prime with \code{p}.
#' @param p Length of the period to test for (a positive integer)
#' @param n Number of resamples to use in the random shuffle test (a positive integer)
#' @param alpha P-value threshold to use when calculating the power (a positive scalar between 0 and 1, although it is recommended to be around 0.05)
#' @param returnExtraInfo Logical value - if \code{TRUE}, the function will return extra information (the value of the range test statistic, the mean per bin, the departures within each bin from the mean value, and a confidence interval on the p-value). If \code{FALSE}, only the p-value is returned.
#' 
#' @return This depends on the value of \code{returnExtraInfo}:
#'  - if \code{returnExtraInfo} is \code{FALSE} (the default), the function returns the p-value only
#'  - if \code{returnExtraInfo} is \code{TRUE}, the function returns a list with the p-value, the value of the range test statistic, the mean per bin, the departures within each bin from the mean value, and a confidence interval on the p-value
#' 
#' @examples
#' ## Generate a random time-series:
#' ##  - length = 1000
#' ##  - signal period = 8
#' ##  - signal to noise ratio = 0.2
#' set.seed(42)
#' 
#' x <- rep(rnorm(8)*0.2,length.out = 1000) + rnorm(1000)
#' randomShuffleTest(ts = x, p = 8, n = 1000,
#'                    blockLength = 10, returnExtraInfo = TRUE)
#' 
#' ## compare with periods either too short or too long:
#' randomShuffleTest(ts = x, p = 7, n = 1000, blockLength = 10)
#' randomShuffleTest(ts = x, p = 9, n = 1000, blockLength = 10)
#' @export
#' @importFrom Rcpp evalCpp
#' @useDynLib RandomThinning

randomShuffleTest <- function(ts, its = NULL, blockLength = 1, p = 5, n = 1000,
                               alpha = 0.05, returnExtraInfo = FALSE){

    m <- length(ts)
    if(is.null(its)){
        its <- ((0:(m-1)) %% p)
    } else {
        if(min(its) == 1){
            its <- its - 1
        }
    }
    MCstatistics <- rep(0.0,n)
    TS <- ts[is.finite(ts)]
    ITS <- its[is.finite(ts)]
    nExtra <- (blockLength - (m %% blockLength)) %% blockLength
    nBlocks <- (m + nExtra) %/% blockLength
    tsBlocks <- matrix(c(ts,rep(NA,nExtra)),nBlocks,blockLength,byrow = TRUE)
    itsExtended <- c(its,its[1:nExtra])
    for(i in 1:n){
        iorder <- sample(nBlocks)
        shuffled <-  c(tsBlocks[iorder,])
        iok <- !is.na(shuffled)
        MCstatistics[i] <- rangeTestFinitesOnly(ts = shuffled[iok], its = itsExtended[iok], p = p)
    }
    stats <- rangeTestGeneral(ts = TS, its = ITS, p = p)
    Nabove = sum(MCstatistics >= stats$range)
    phat = Nabove/n
    if(returnExtraInfo){
        CI <- binom.test(Nabove, n, conf.level = 1.0 - alpha)
        return(list(p = phat,
                    stat = stats$range,
                    prof = stats$means,
                    resid = stats$means - mean(ts,na.rm=TRUE),
                    CI = CI))
    } else {
        return(phat)
    }
    
}
