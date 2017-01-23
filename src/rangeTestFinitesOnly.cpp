#include <Rcpp.h>
#include <algorithm>

using namespace Rcpp;

//' Range test - optimized for finite values only
//'
//' @param ts A time-series (a numeric vector)
//' @param its Indices of which bin the values of x correspond to (a numeric vector of integers between 1 and p). This is optional, and taken to be cyclical by default.
//' @param p Length of the period to test for (a positive integer)
//' 
//' @return A scalar numerical value giving the maximum difference (maximum - minumum) of the means of the \code{p} bins
//'
//' @export
//'
// [[Rcpp::export]]
double rangeTestFinitesOnly(NumericVector ts, NumericVector its, int p){
  
  int i, j;
  bool first;
  double maxmean, minmean;

  int n = ts.size();
  NumericVector means(p);
  NumericVector counts(p);

  for (i = 0; i < n; i++) {
    j = its[i];
    means[j] += ts[i];
    counts[j] +=  1.0;
  }

  first = true;
  for (j = 0; j < p; j++) {
    if(counts[j] != 0.0){
      means[j] /= counts[j];
      if(first){
	first = false;
	maxmean = means[j];
	minmean = means[j];
      } else {
	maxmean = std::max(maxmean,means[j]);
	minmean = std::min(minmean,means[j]);
      }
    }
  }

  double range = maxmean - minmean;

  return range;
}
