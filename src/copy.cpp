#include <Rcpp.h>
using namespace Rcpp;

//' Deep copy a list
//'
//' @param x a list
//' @return a deep copy of the input list
//'
//' @export
// [[Rcpp::export]]
Rcpp::List copy(Rcpp::List x) {
  return Rcpp::clone(x);
}
