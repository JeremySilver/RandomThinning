#' Calculate various statistics for the time-series provided as an argument
#'
#' @param ts A time-series (a numeric vector)
#' @param its Indices of which bin the values of x correspond to (a numeric vector of integers between 1 and p). This is optional, and taken to be cyclical by default.
#' @param p Length of the period to test for (a positive integer)
#' @return A list with the following components:
#'  - \code{signalNoiseRatio}: The signal to noise ratio estimated for the time-series
#'  - \code{noiseAC}: The autocorrelation of the noise time-series
#'  - \code{signalAC}: The (circular) autocorrelation of the signal of length \code{p}
#'  - \code{nSignals}: The number of length \code{p} periods in the time-series 
#' @examples
#' ## Generate a random time-series:
#' ##  - length = 1000
#' ##  - signal period = 8
#' ##  - signal to noise ratio = 0.2
#' set.seed(42)
#' 
#' x <- rep(rnorm(8)*0.2,length.out = 1000) + rnorm(1000)
#' calculateTSstatistics(ts = x, p = 8)
#' @export
#' @useDynLib RandomThinning

calculateTSstatistics <- function(ts, its = NULL, p){
    m <- length(ts)
    if(is.null(its)){
        its <- ((0:(m-1)) %% p) + 1
    }
    nSignals <- m/p
    signal <- sapply(1:p,function(i) mean(ts[its == i],na.rm=TRUE))
    noise <- ts - signal[its]
    meanN <- mean(noise,na.rm=TRUE)
    AC <- mean((head(noise,-1) - meanN) * (tail(noise,-1) - meanN),na.rm=TRUE)/var(noise,na.rm = TRUE)
    ##
    meanS <- mean(signal,na.rm=TRUE)
    cycleSignal <- c(tail(signal,-1),signal[1])
    SAC <- mean((signal - meanS) * (cycleSignal - meanS),na.rm=TRUE)/var(signal,na.rm = TRUE)
    signalNoiseRatio <- sd(signal,na.rm = TRUE)/sd(noise,na.rm = TRUE)
    return(list(signalNoiseRatio = signalNoiseRatio,
                noiseAC = AC,
                signalAC = SAC,
                nSignals = nSignals))
}
