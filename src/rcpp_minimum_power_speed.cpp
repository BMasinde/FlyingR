#include <Rcpp.h>
using namespace Rcpp;

//' Function calculates the minimum power speed
//' @param bm all-up mass
//' @param ws wing span
//' @param ipf induced power factor
//' @param  g gravity
//' @param airDensity
//' @param bdc body drag coefficient
//' @export
// [[Rcpp::export]]
NumericVector minpowspeed_cpp(NumericVector bm, NumericVector ws, float ipf,
                              float g, float airDensity, float bdc) {
  int n = bm.size();

  NumericVector vmp(n);


  for(int i = 0; i < n; ++i){
    vmp[i] = (0.807 * pow(ipf, 0.25) * pow(bm[i], 0.5) * pow(g, 0.5))/
      (pow(airDensity, 0.5) * pow(ws[i], 0.5) * pow((0.00813 * pow(bm[i], 0.666)), 0.25) * pow(bdc, 0.25));
  }

  return vmp;

  //NumericVector bfa = 0.00813 * pow(bm, 0.666);

  //NumericVector vmp = (0.807 * pow(ipf, 0.25) * pow(bm, 0.5) * pow(g, 0.5) ) /
  //    (pow(airDensity, 0.5) * pow(ws, 0.5) * pow(bfa, 0.25) * pow(bdc, 0.25));
}
