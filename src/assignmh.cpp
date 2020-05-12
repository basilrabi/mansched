#include "assignmh.h"
#include "getmin.h"

Rcpp::IntegerVector assignMH ( Rcpp::IntegerVector hoursT,
                               Rcpp::IntegerVector hoursR )
{
  Rcpp::IntegerVector hoursA = getMin( hoursT, hoursR );
  hoursT = hoursT - hoursA;
  hoursR = hoursR - hoursA;
  return hoursA;
}

Rcpp::IntegerVector assignSelfMH ( Rcpp::IntegerVector hoursT )
{
  Rcpp::IntegerVector hours = Rcpp::clone( hoursT );
  hoursT = hoursT - hours;
  return hours;
}
