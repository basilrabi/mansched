#include "getmin.h"

Rcpp::IntegerVector getMin ( const Rcpp::IntegerVector& a,
                             const Rcpp::IntegerVector& b )
{
  if ( a.length() != b.length() )
    Rcpp::stop("Vector lengths not equal!.");

  Rcpp::IntegerVector c ( a.length() );

  for ( int i = 0; i < a.length(); i++ )
  {
    if ( a[i] >= b[i] )
    {
      c[i] = b[i];
    }
    else
    {
      c[i] = a[i];
    }
  }

  return c;
}
