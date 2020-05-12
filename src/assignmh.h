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
//' @return an integer vector of length 12 representing the man-hours assigned
//'
//'   The hoursT and hoursR will also be reduced by the value of the returned
//'   integer vector.
//' @export
// [[Rcpp::export]]
Rcpp::IntegerVector assignMH ( Rcpp::IntegerVector hoursT,
                               Rcpp::IntegerVector hoursR );

// Return the remaining man-hours and assign zero.
Rcpp::IntegerVector assignSelfMH ( Rcpp::IntegerVector hoursT );

#endif
