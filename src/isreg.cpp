#include "isreg.h"

bool isReg ( const Rcpp::S4& theObject )
{
  Rcpp::StringVector status = Rcpp::as<Rcpp::StringVector>( theObject.slot( "status" ) );
  if ( status[0] == "reg" )
    return true;
  return false;
}
