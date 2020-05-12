#include "mhdb.h"
#include "template.h"

Rcpp::DataFrame combineMHDB ( Rcpp::DataFrame a, Rcpp::DataFrame b )
{
  Rcpp::StringVector  id       = combine( Rcpp::as<Rcpp::StringVector >( a["ID"       ] ), Rcpp::as<Rcpp::StringVector >( b["ID"       ] ) );
  Rcpp::IntegerVector mh       = combine( Rcpp::as<Rcpp::IntegerVector>( a["mh"       ] ), Rcpp::as<Rcpp::IntegerVector>( b["mh"       ] ) );
  Rcpp::StringVector  mhType   = combine( Rcpp::as<Rcpp::StringVector >( a["mhType"   ] ), Rcpp::as<Rcpp::StringVector >( b["mhType"   ] ) );
  Rcpp::IntegerVector month    = combine( Rcpp::as<Rcpp::IntegerVector>( a["month"    ] ), Rcpp::as<Rcpp::IntegerVector>( b["month"    ] ) );
  Rcpp::NumericVector np       = combine( Rcpp::as<Rcpp::NumericVector>( a["np"       ] ), Rcpp::as<Rcpp::NumericVector>( b["np"       ] ) );
  Rcpp::StringVector  costCode = combine( Rcpp::as<Rcpp::StringVector >( a["costCode" ] ), Rcpp::as<Rcpp::StringVector >( b["costCode" ] ) );
  Rcpp::DataFrame mhDB = Rcpp::DataFrame::create(
    Rcpp::Named( "ID"               ) = id,
    Rcpp::Named( "mh"               ) = mh,
    Rcpp::Named( "mhType"           ) = mhType,
    Rcpp::Named( "month"            ) = month,
    Rcpp::Named( "np"               ) = np,
    Rcpp::Named( "costCode"         ) = costCode,
    Rcpp::Named( "stringsAsFactors" ) = false
  );
  return mhDB;
}

Rcpp::DataFrame dfAppend ( Rcpp::DataFrame a, Rcpp::DataFrame b, int& x )
{
  R_xlen_t lengthB = ( Rcpp::as<Rcpp::StringVector>( b["ID"] ) ).length();
  a = testAndExpand( a, x, lengthB );

  Rcpp::StringVector  idA       = Rcpp::as<Rcpp::StringVector >( a["ID"       ] );
  Rcpp::IntegerVector mhA       = Rcpp::as<Rcpp::IntegerVector>( a["mh"       ] );
  Rcpp::StringVector  mhTypeA   = Rcpp::as<Rcpp::StringVector >( a["mhType"   ] );
  Rcpp::IntegerVector monthA    = Rcpp::as<Rcpp::IntegerVector>( a["month"    ] );
  Rcpp::NumericVector npA       = Rcpp::as<Rcpp::NumericVector>( a["np"       ] );
  Rcpp::StringVector  costCodeA = Rcpp::as<Rcpp::StringVector >( a["costCode" ] );

  Rcpp::StringVector  idB       = Rcpp::as<Rcpp::StringVector >( b["ID"       ] );
  Rcpp::IntegerVector mhB       = Rcpp::as<Rcpp::IntegerVector>( b["mh"       ] );
  Rcpp::StringVector  mhTypeB   = Rcpp::as<Rcpp::StringVector >( b["mhType"   ] );
  Rcpp::IntegerVector monthB    = Rcpp::as<Rcpp::IntegerVector>( b["month"    ] );
  Rcpp::NumericVector npB       = Rcpp::as<Rcpp::NumericVector>( b["np"       ] );
  Rcpp::StringVector  costCodeB = Rcpp::as<Rcpp::StringVector >( b["costCode" ] );

  for ( R_xlen_t i = 0; i < lengthB; i++ )
  {
    idA      [x-1] = idB      [i];
    mhA      [x-1] = mhB      [i];
    mhTypeA  [x-1] = mhTypeB  [i];
    monthA   [x-1] = monthB   [i];
    npA      [x-1] = npB      [i];
    costCodeA[x-1] = costCodeB[i];
    x = x + 1;
  }

  return a;
}

Rcpp::DataFrame mhdbInit ( R_xlen_t n )
{
  Rcpp::StringVector  id       ( 12 * n, Rcpp::StringVector::get_na()  );
  Rcpp::IntegerVector mh       ( 12 * n, 0 );
  Rcpp::StringVector  mhType   ( 12 * n, Rcpp::StringVector::get_na()  );
  Rcpp::IntegerVector month    ( 12 * n, Rcpp::IntegerVector::get_na() );
  Rcpp::NumericVector np       ( 12 * n, 0 );
  Rcpp::StringVector  costCode ( 12 * n, Rcpp::StringVector::get_na()  );
  Rcpp::DataFrame mhDB = Rcpp::DataFrame::create(
    Rcpp::Named( "ID"               ) = id,
    Rcpp::Named( "mh"               ) = mh,
    Rcpp::Named( "mhType"           ) = mhType,
    Rcpp::Named( "month"            ) = month,
    Rcpp::Named( "np"               ) = np,
    Rcpp::Named( "costCode"         ) = costCode,
    Rcpp::Named( "stringsAsFactors" ) = false
  );
  return mhDB;
}

Rcpp::DataFrame mhdbInitEmployee ( Rcpp::StringVector mhTypes,
                                   Rcpp::String empID,
                                   Rcpp::String cc )
{
  Rcpp::DataFrame mhDB = mhdbInit( mhTypes.length() );
  Rcpp::StringVector id       = Rcpp::as<Rcpp::StringVector >( mhDB["ID"      ] );
  Rcpp::StringVector mhType   = Rcpp::as<Rcpp::StringVector >( mhDB["mhType"  ] );
  Rcpp::IntegerVector month   = Rcpp::as<Rcpp::IntegerVector>( mhDB["month"   ] );
  Rcpp::StringVector costCode = Rcpp::as<Rcpp::StringVector >( mhDB["costCode"] );
  for ( int i = 0; i < mhTypes.length(); i++ )
  {
    for ( int j = 1; j < 13; j++ )
    {
      R_xlen_t idx = ( i * 12 ) + ( j - 1 );
      id      [idx] = empID;
      mhType  [idx] = mhTypes[i];
      month   [idx] = j;
      costCode[idx] = cc;
    }
  }
  return mhDB;
}

Rcpp::DataFrame testAndExpand ( Rcpp::DataFrame a, int& x, const int& y )
{
  R_xlen_t lengthA = ( Rcpp::as<Rcpp::StringVector>( a["ID"] ) ).length();
  if ( y <= ( lengthA + 1 - x ) )
    return a;
  Rcpp::Rcout << "DataFrame rows not enough. Expanding…\n";
  Rcpp::DataFrame dfy = mhdbInit( y + lengthA );
  Rcpp::DataFrame b = combineMHDB( a, dfy );
  return b;
}
