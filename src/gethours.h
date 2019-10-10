#ifndef GETHOURS_H
#define GETHOURS_H

#include <Rcpp.h>

//' Get remaining working hours
//'
//' This function returns all remaining man hour types including overtime.
//'
//' @param employee an \code{\link{Employee-class}} object
//' @return a \code{\link{data.frame}} representing the remaining man hours
//'
//'   Each row represents a month while each column represents a man hour type.
//' @export
// [[Rcpp::export]]
Rcpp::DataFrame getHours ( const Rcpp::S4& employee );

#endif
