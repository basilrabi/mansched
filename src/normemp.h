#ifndef NORMEMP_H
#define NORMEMP_H

#include <Rcpp.h>

//' Strip all over time hours from an employee
//'
//' This function modifies an \code{\link{Employee-class}} object with overtime
//'   hours removed (see \code{\link{premium}}). The cost code is also changed
//'   into a dummy cost code \code{"0-0"}.
//'
//' @param emp an \code{\link{Employee-class}} object
//' @return a \code{\link{data.frame}} of dropped man-hours. The columns of the
//'   which is similar to what is being returned in \code{\link{getmhDB}}.
//' @export
// [[Rcpp::export]]
Rcpp::DataFrame normEmp ( Rcpp::S4 emp );

#endif
