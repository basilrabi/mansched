#ifndef GETMIN_H
#define GETMIN_H

#include <Rcpp.h>

//' Paired Minimum
//'
//' Get minimum value between two integer vectors
//'
//' @param a integer vector
//' @param b integer vector
//' @return an integer vector with the least value for each element by element
//'   comparison between a and b
//' @export
// [[Rcpp::export]]
Rcpp::IntegerVector getMin ( const Rcpp::IntegerVector& a,
                             const Rcpp::IntegerVector& b );

#endif
