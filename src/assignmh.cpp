#include "assignmh.h"
#include "getmin.h"

Rcpp::DataFrame assignMH ( const Rcpp::IntegerVector& hoursT,
                           const Rcpp::IntegerVector& hoursR )
{
  Rcpp::IntegerVector hoursA = getMin( hoursT, hoursR );
  Rcpp::IntegerVector hourst = hoursT - hoursA;
  Rcpp::IntegerVector hoursr = hoursR - hoursA;
  Rcpp::IntegerVector month = Rcpp::seq_len( 12 );

  Rcpp::DataFrame hoursData = Rcpp::DataFrame::create(
    Rcpp::Named( "hoursT" ) = hourst,
    Rcpp::Named( "hoursR" ) = hoursr,
    Rcpp::Named( "hoursA" ) = hoursA,
    Rcpp::Named( "month"  ) = month
  );

  return hoursData;
}
