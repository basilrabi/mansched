#include "assignmh.h"

Rcpp::DataFrame assignMH ( const Rcpp::IntegerVector& hoursT,
                           const Rcpp::IntegerVector& hoursR )
{
  Rcpp::IntegerVector hoursA ( hoursT.length() );
  Rcpp::IntegerVector month  ( hoursT.length() );

  Rcpp::IntegerVector hourst = Rcpp::clone( hoursT );
  Rcpp::IntegerVector hoursr = Rcpp::clone( hoursR );

  for ( int i = 0; i < hoursA.length(); i++ )
  {
    if ( hoursT[i] >= hoursR[i] )
    {
      hoursA[i] = hoursR[i];
    }
    else
    {
      hoursA[i] = hoursT[i];
    }
    hoursr[i] = hoursr[i] - hoursA[i];
    hourst[i] = hourst[i] - hoursA[i];

    month[i] = i + 1;
  }

  Rcpp::DataFrame hoursData = Rcpp::DataFrame::create(
    Rcpp::Named( "hoursT" ) = hourst,
    Rcpp::Named( "hoursR" ) = hoursr,
    Rcpp::Named( "hoursA" ) = hoursA,
    Rcpp::Named( "month"  ) = month
  );

  return hoursData;
}
