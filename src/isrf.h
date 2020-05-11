#ifndef ISRF_H
#define ISRF_H

#include <Rcpp.h>

//' isRF
//'
//' Is the employee rank and file? Also checks whether an RF flag is correctly
//' assigned to the employees.
//'
//' @param theObject an \code{\link{Employee-class}} object
//' @return logical value
//' @export isRF
// [[Rcpp::export]]
bool isRF ( const Rcpp::S4& theObject );

#endif
