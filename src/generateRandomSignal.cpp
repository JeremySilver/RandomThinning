#include <Rcpp.h>
#include <algorithm>

using namespace Rcpp;

//' Calculate the circular autocorrelation in a time-series
//'
//' @param ts A time-series (a numeric vector)
//'
//' @details Note that all values are included. Non-finite values may cause odd answers
//' 
//' @return A scalar numerical value circular autocorrelation
//'
//' @export
// [[Rcpp::export]]
double autocor(NumericVector ts){
  // outputs
  double ac;
  // locals
  int m = ts.size(); // Size of vector

  double mu = mean(ts);
  double s2 = var(ts);
  //
  ac = (ts[0] - mu)*(ts[m-1] - mu);

  for(int i = 0; i < m-1; i++ ){
    ac += (ts[i] - mu)*(ts[i+1] - mu);
  }
  ac = ac/((double) m)/s2;

  return ac;
}

//' Generate a random signal with given circular autocorrelation
//'
//' @param m Length of the signal (a positive integer)
//' @param sac Strength of the circular autocorrelation (a positive double, should be between 0 and 1)
//' @param maxcounter Maximum number of tries in the \code{while} loop (a positive integer)
//' @param margin The margin of error between the desired and obtained autocorrelation (a positive double)
//'
//' @return A numerical vector
//'
//' @export
// [[Rcpp::export]]
NumericVector generateRandomSignal(int m,
                                   double sac,
				   int maxcounter,
				   double margin){
  NumericVector noise(m);
  NumericVector signal(m);
  double sample_ac;
  int  i, counter;
  bool SAC_within_bounds;

  SAC_within_bounds = false;
  
  counter = 0;
  while(!SAC_within_bounds){
    // generate a normally distributed signal
    noise = rnorm(m);
    signal[0] = noise[0];
    for( i = 1; i < m; i++ ) {
      signal[i] = sac * signal[i-1] + noise[i];
    }
    sample_ac = autocor(signal);
    SAC_within_bounds = (sac - margin <= sample_ac) && (sample_ac <= sac + margin) ;
    counter++;
    // if(counter > maxcounter) {
    //   call rexit('Method for generating random signal over-ran the iteration limit...');
    // }
  }
  
  // set the standard deviation to 1.0
  signal = signal/sd(signal);
     
  return signal;
}

