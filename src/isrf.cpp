#include "isrf.h"

bool getRF ( const Rcpp::S4& theObject )
{
  Rcpp::LogicalVector rf = theObject.slot( "isRF" );
  if ( rf.length() != 1 )
    Rcpp::stop( "Length of isRF slot != 1." );
  if ( Rcpp::LogicalVector::is_na( rf[0] ) )
  {
    Rcpp::Rcout << "isRF slot of "
                << Rcpp::as<Rcpp::StringVector>( theObject.slot( "ID" ) )
                << " is not initialized.\n";
    Rcpp::stop( "isRf slot must be initialized." );
  }
  return rf[0];
}

bool checkNonRF ( const Rcpp::S4& theObject )
{
  if ( !getRF( theObject ) )
    return false;
  Rcpp::Rcout << Rcpp::as<Rcpp::StringVector>( theObject.slot( "ID" ) )
              << " was declaread rank and file";
  Rcpp::stop( "Wrong assignment of RF status." );
  return true;
}

bool checkRF ( const Rcpp::S4& theObject )
{
  if ( getRF( theObject ) )
    return true;
  Rcpp::Rcout << Rcpp::as<Rcpp::StringVector>( theObject.slot( "ID" ) )
              << " was not declaread rank and file";
  Rcpp::stop( "Wrong assignment of RF status." );
  return false;
}

bool isRF ( const Rcpp::S4& theObject )
{
  Rcpp::String empClass = Rcpp::as<Rcpp::String>( theObject.attr( "class" ) );
  if ( empClass == "Laborer" ||
       empClass == "Operator" )
  {
    return checkRF( theObject );
  }
  else if ( empClass == "Supervisor" ||
            empClass == "Technical" )
  {
    return checkNonRF( theObject );
  }
  else if ( empClass == "SectionHead" ||
            empClass == "DepartmentManager" ||
            empClass == "GroupManager" ||
            empClass == "DivisionManager" )
  {
    return false;
  }
  else if ( empClass == "Clerk" )
  {
    return getRF( theObject );
  }
  else
  {
    Rcpp::stop( "Unknown employee class." );
  }
}


