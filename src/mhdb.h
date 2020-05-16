#ifndef MHDB_H
#define MHDB_H

#include <Rcpp.h>

// Combines man-hours database by row
Rcpp::DataFrame combineMHDB ( Rcpp::DataFrame a, Rcpp::DataFrame b );

// 1-row manhours database
Rcpp::DataFrame mhdbBlank ( const Rcpp::S4& emp );

// Append man-hours database b to man-hours database a. `x` is the index in R of
// the first empty row in a where b will be appended to. If the rows are not
// enough, man-hours database a will be expanded.
Rcpp::DataFrame dfAppend ( Rcpp::DataFrame a, Rcpp::DataFrame b, R_xlen_t& x );

// Initialize an empty man-hours database with n x 12 rows
Rcpp::DataFrame mhdbInit ( const R_xlen_t& n );

// Initialize an empty man-hours database of an employee. Depending on the
// Employee sub-class, different number of rows will be created.
// `mhTypes` for every man-hour type (mhType), a set of 12-row data frame will be
// created. Each row will represent a month.
// `id` employee ID
// `cc` cost center to be assigned
Rcpp::DataFrame mhdbInitEmployee ( Rcpp::StringVector mhTypes,
                                   Rcpp::String empID,
                                   Rcpp::String cc );

// Test man-hours database a whether there is enough blank rows for appending.
// `x` is the index in R of the first empty row in a where another man-hours
// will be appended.
// `y` is the length of the man-hours database to be appended.
Rcpp::DataFrame testAndExpand ( Rcpp::DataFrame a,
                                const R_xlen_t& x,
                                const R_xlen_t& y );

#endif
