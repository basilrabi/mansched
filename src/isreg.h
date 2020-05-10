#ifndef ISREG_H
#define ISREG_H

#include <Rcpp.h>

//' isReg
//'
//' Is the employee regular?
//'
//' @param theObject an \code{\link{Employee-class}} object
//' @return logical value
//' @export isReg
// [[Rcpp::export]]
bool isReg ( const Rcpp::S4& theObject );

#endif
