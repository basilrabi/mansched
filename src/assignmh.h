#ifndef ASSIGNMH_H
#define ASSIGNMH_H

#include <Rcpp.h>

//' Spend all available man hours
//'
//' Available man hours of a specific man hour type from an
//'   \code{\link{Employee-class}} object representing a real employee is
//'   assigned to the man hours of an \code{Employee-class} object representing
//'   an employee requirement.
//'
//' @param hoursT integer vector of length 12
//'
//'   This represents the man hours from a theoretical employee with a certain
//'   man hour type.
//' @param hoursR integer vector of length 12
//'
//'   This represents the man hours from a real employee with a certain man hour
//'   type.
//' @return a \code{\link{data.frame}} with 12 rows and 4 columns
//'
//'   Each row represents a month. The columns are defined as follows:
//'   \describe{
//'     \item{hoursT}{man hours from the theoretical employee}
//'     \item{hoursR}{man hours from the real employee}
//'     \item{hoursA}{man hours to be assigned}
//'     \item{month}{integer representing the month of assignment}
//'   }
//' @export
// [[Rcpp::export]]
Rcpp::DataFrame assignMH ( const Rcpp::IntegerVector& hoursT,
                           const Rcpp::IntegerVector& hoursR );

#endif
