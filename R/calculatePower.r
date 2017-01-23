#' Estimate the power of the random thinning test based on the properties a given time-series
#'
#' @param ts A time-series (a numeric vector)
#' @param p Length of the period to test for (a positive integer)
#' @param nMC Number of samples to estimate the power (a positive integer)
#' @param nThin Number of resamples to use in the random thinning test (a positive integer)
#' @param its Indices of which bin the values of x correspond to (a numeric vector of integers between 1 and p). This is optional, and taken to be cyclical by default.
#' @param fr Which fractions to consider discarding (a numeric vector, with values between 0 and 1, although it is best if they are kept between between 0.001 and 0.2)
#' @param pThreshold P-value threshold to use when calculating the power (a positive scalar between 0 and 1, although it is recommended to be around 0.05)
#' @param useNApattern Logical value - if \code{TRUE}, the synthetic time-series inherit the same missingness pattern as in the original time-series
#' 
#' @return A numeric vector, with one entry per value of \code{fr}, giving the power of the test for this particular value
#' @examples
#' ## Generate a random time-series:
#' ##  - length = 1000
#' ##  - signal period = 8
#' ##  - signal to noise ratio = 0.2
#' set.seed(42)
#' 
#' x <- rep(rnorm(8)*0.2,length.out = 1000) + rnorm(1000)
#' calculatePower(ts = x, p = 8, nMC = 200, nThin = 200,
#'                            fr = c(0.01,0.025,0.1))
#' @export
#' @useDynLib RandomThinning

calculatePower <- function(ts, p, nMC = 1000,
                           nThin = 1000,
                           its = NULL,
                           fr = c(0.001,0.025,0.01,0.025,0.1),
                           pThreshold = 0.05,
                           useNApattern = FALSE){

    TSstats <- calculateTSstatistics(ts = ts, its = its, p = p)
    m <- length(ts)
    if(is.null(its)){
        its <- ((0:(m-1)) %% p) + 1
    }

    nFR <- length(fr)
    margin <- 0.05
    maxcounter <- 100L
    sac <- TSstats$signalAC
    power <- rep(0,nFR)
    pvals <- rep(0,nMC)
    for(iFR in 1:nFR){
        cat('fr = ',fr[iFR],fill = TRUE)
        for(iMC in 1:nMC){
            signal <- generateRandomSignal(p, sac, maxcounter, margin)
            signal <- rep(signal,length = m)
            signal <- signal/sd(signal)
            noise <- arima.sim(model=list(ar=TSstats$noiseAC),n = m)
            noise <- noise/sd(noise)
            synthetic <- noise + signal*TSstats$signalNoiseRatio
            if(useNApattern)
                synthetic[is.na(ts)] <- NA
            ##
            pval <- randomThinningTest(ts = synthetic,its = its, fr = fr[iFR],
                                       p = p, n = nThin)

            if(pval <= pThreshold){
                power[iFR] <- power[iFR] + 1
            }
            pvals[iMC] <- pval
        }
        power[iFR] <- power[iFR]/nMC
    }
    return(power)
}
