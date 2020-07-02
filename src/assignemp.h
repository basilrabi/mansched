#ifndef ASSIGNMH_H
#define ASSIGNMH_H

#include <Rcpp.h>

//' Assign an employee
//'
//' Man hours from an \code{\link{Employee-class}} object representing a real
//'   employee is assigned to the man hours from an \code{\link{Employee-class}}
//'   object representing another employee (manpower requirement). During
//'   successful assignment, the man hours of both \code{Employee-class} objects
//'   are reduced.
//'
//' @param empT an \code{\link{Employee-class}} object representing a
//'   theoretical employee
//' @param empR an \code{\link{Employee-class}} object representing a real
//'   employee
//' @param selfAssign boolean
//'
//'   Is the employee being self-assigned? Self assigning means that the two
//'   arguments above points to the same object.
//' @return a \code{\link{data.frame}} containing the man hours database
//'   resulting from the employee assignment. This is consisted of the following
//'   columns:
//'   \describe{
//'     \item{ID}{character string representing the unique identifier of the
//'       real employee}
//'     \item{reqID}{character string representing the unique identifier of the
//'       theoretical/requirement employee}
//'     \item{mh}{integer value representing the man hours assigned}
//'     \item{mhType}{character string representing the man hour type
//'
//'       \code{mhType} can be one of the following:
//'       \describe{
//'         \item{reg}{working hours not more than 8 hours during a regular day}
//'         \item{rd}{working hours not more than 8 hours during a rest day}
//'         \item{sh}{working hours not more than 8 hours during a special
//'           holiday}
//'         \item{lh}{working hours not more than 8 hours during a legal
//'           holiday}
//'         \item{nh}{working hours not more than 8 hours during a negotiated
//'           holiday}
//'         \item{rs}{working hours not more than 8 hours during a rest day on a
//'           special holiday}
//'         \item{rl}{working hours not more than 8 hours during a rest day on a
//'           legal holiday}
//'         \item{rn}{working hours not more than 8 hours during a rest day on a
//'           negotiated holiday}
//'         \item{regOT}{working hours more than 8 hours during a regular day}
//'         \item{rdOT}{working hours more than 8 hours during a rest day}
//'         \item{shOT}{working hours more than 8 hours during a special
//'           holiday}
//'         \item{lhOT}{working hours more than 8 hours during a legal holiday}
//'         \item{nhOT}{working hours more than 8 hours during a negotiated
//'           holiday}
//'         \item{rsOT}{working hours more than 8 hours during a rest day on a
//'           special holiday}
//'         \item{rlOT}{working hours more than 8 hours during a rest day on a
//'           legal holiday}
//'         \item{rnOT}{working hours more than 8 hours during a rest day on a
//'           negotiated holiday}
//'       }
//'     }
//'     \item{month}{integer value representing the month}
//'     \item{np}{integer value representing the man hours with night premium
//'       pay}
//'     \item{costCenter}{character string representing the cost center
//'       wherein the man hours is charged}
//'   }
//' @export
// [[Rcpp::export]]
Rcpp::DataFrame assignEmp ( Rcpp::S4 empT, Rcpp::S4 empR, bool selfAssign );

#endif
