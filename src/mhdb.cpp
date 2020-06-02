#include "mhdb.h"
#include "template.h"

Rcpp::DataFrame combineMHDB ( Rcpp::DataFrame a, Rcpp::DataFrame b )
{
  Rcpp::StringVector  id       = combine( Rcpp::as<Rcpp::StringVector >( a["ID"       ] ), Rcpp::as<Rcpp::StringVector >( b["ID"       ] ) );
  Rcpp::StringVector  reqid    = combine( Rcpp::as<Rcpp::StringVector >( a["reqID"    ] ), Rcpp::as<Rcpp::StringVector >( b["reqID"    ] ) );
  Rcpp::IntegerVector mh       = combine( Rcpp::as<Rcpp::IntegerVector>( a["mh"       ] ), Rcpp::as<Rcpp::IntegerVector>( b["mh"       ] ) );
  Rcpp::StringVector  mhType   = combine( Rcpp::as<Rcpp::StringVector >( a["mhType"   ] ), Rcpp::as<Rcpp::StringVector >( b["mhType"   ] ) );
  Rcpp::IntegerVector month    = combine( Rcpp::as<Rcpp::IntegerVector>( a["month"    ] ), Rcpp::as<Rcpp::IntegerVector>( b["month"    ] ) );
  Rcpp::NumericVector np       = combine( Rcpp::as<Rcpp::NumericVector>( a["np"       ] ), Rcpp::as<Rcpp::NumericVector>( b["np"       ] ) );
  Rcpp::StringVector  costCode = combine( Rcpp::as<Rcpp::StringVector >( a["costCode" ] ), Rcpp::as<Rcpp::StringVector >( b["costCode" ] ) );
  Rcpp::DataFrame mhDB = Rcpp::DataFrame::create(
    Rcpp::Named( "ID"               ) = id,
    Rcpp::Named( "reqID"            ) = reqid,
    Rcpp::Named( "mh"               ) = mh,
    Rcpp::Named( "mhType"           ) = mhType,
    Rcpp::Named( "month"            ) = month,
    Rcpp::Named( "np"               ) = np,
    Rcpp::Named( "costCode"         ) = costCode,
    Rcpp::Named( "stringsAsFactors" ) = false
  );
  return mhDB;
}

Rcpp::DataFrame mhdbBlank ( const Rcpp::S4& emp )
{
  Rcpp::StringVector  id       = { Rcpp::as<Rcpp::String>( emp.slot( "ID" ) ) };
  Rcpp::StringVector  reqid    = { Rcpp::as<Rcpp::String>( emp.slot( "ID" ) ) };
  Rcpp::IntegerVector mh       = { 0 };
  Rcpp::StringVector  mhType   = { "reg" };
  Rcpp::IntegerVector month    = { 1 };
  Rcpp::NumericVector np       = { 0 };
  Rcpp::StringVector  costCode = { NA_STRING };
  Rcpp::DataFrame mhDB = Rcpp::DataFrame::create(
    Rcpp::Named( "ID"               ) = id,
    Rcpp::Named( "reqID"            ) = reqid,
    Rcpp::Named( "mh"               ) = mh,
    Rcpp::Named( "mhType"           ) = mhType,
    Rcpp::Named( "month"            ) = month,
    Rcpp::Named( "np"               ) = np,
    Rcpp::Named( "costCode"         ) = costCode,
    Rcpp::Named( "stringsAsFactors" ) = false
  );
  return mhDB;
}

Rcpp::DataFrame mhdbFilter ( Rcpp::DataFrame mhdb )
{
  Rcpp::StringVector  id       = Rcpp::as<Rcpp::StringVector >( mhdb["ID"       ] );
  Rcpp::StringVector  reqid    = Rcpp::as<Rcpp::StringVector >( mhdb["reqID"    ] );
  Rcpp::IntegerVector mh       = Rcpp::as<Rcpp::IntegerVector>( mhdb["mh"       ] );
  Rcpp::StringVector  mhType   = Rcpp::as<Rcpp::StringVector >( mhdb["mhType"   ] );
  Rcpp::IntegerVector month    = Rcpp::as<Rcpp::IntegerVector>( mhdb["month"    ] );
  Rcpp::NumericVector np       = Rcpp::as<Rcpp::NumericVector>( mhdb["np"       ] );
  Rcpp::StringVector  costCode = Rcpp::as<Rcpp::StringVector >( mhdb["costCode" ] );

  Rcpp::LogicalVector withValues = mh > 0;
  id       = id      [withValues];
  reqid    = reqid   [withValues];
  mh       = mh      [withValues];
  mhType   = mhType  [withValues];
  month    = month   [withValues];
  np       = np      [withValues];
  costCode = costCode[withValues];
  Rcpp::DataFrame mhDB = Rcpp::DataFrame::create(
    Rcpp::Named( "ID"               ) = id,
    Rcpp::Named( "reqID"            ) = reqid,
    Rcpp::Named( "mh"               ) = mh,
    Rcpp::Named( "mhType"           ) = mhType,
    Rcpp::Named( "month"            ) = month,
    Rcpp::Named( "np"               ) = np,
    Rcpp::Named( "costCode"         ) = costCode,
    Rcpp::Named( "stringsAsFactors" ) = false
  );
  return mhDB;
}

Rcpp::DataFrame dfAppend ( Rcpp::DataFrame a, Rcpp::DataFrame b, R_xlen_t& x )
{
  R_xlen_t lengthB = ( Rcpp::as<Rcpp::StringVector>( b["ID"] ) ).length();
  a = testAndExpand( a, x, lengthB );

  Rcpp::StringVector  idA       = Rcpp::as<Rcpp::StringVector >( a["ID"       ] );
  Rcpp::StringVector  reqidA    = Rcpp::as<Rcpp::StringVector >( a["reqID"    ] );
  Rcpp::IntegerVector mhA       = Rcpp::as<Rcpp::IntegerVector>( a["mh"       ] );
  Rcpp::StringVector  mhTypeA   = Rcpp::as<Rcpp::StringVector >( a["mhType"   ] );
  Rcpp::IntegerVector monthA    = Rcpp::as<Rcpp::IntegerVector>( a["month"    ] );
  Rcpp::NumericVector npA       = Rcpp::as<Rcpp::NumericVector>( a["np"       ] );
  Rcpp::StringVector  costCodeA = Rcpp::as<Rcpp::StringVector >( a["costCode" ] );

  Rcpp::StringVector  idB       = Rcpp::as<Rcpp::StringVector >( b["ID"       ] );
  Rcpp::StringVector  reqidB    = Rcpp::as<Rcpp::StringVector >( b["reqID"    ] );
  Rcpp::IntegerVector mhB       = Rcpp::as<Rcpp::IntegerVector>( b["mh"       ] );
  Rcpp::StringVector  mhTypeB   = Rcpp::as<Rcpp::StringVector >( b["mhType"   ] );
  Rcpp::IntegerVector monthB    = Rcpp::as<Rcpp::IntegerVector>( b["month"    ] );
  Rcpp::NumericVector npB       = Rcpp::as<Rcpp::NumericVector>( b["np"       ] );
  Rcpp::StringVector  costCodeB = Rcpp::as<Rcpp::StringVector >( b["costCode" ] );

  for ( R_xlen_t i = 0; i < lengthB; i++ )
  {
    idA      [x] = idB      [i];
    reqidA   [x] = reqidB   [i];
    mhA      [x] = mhB      [i];
    mhTypeA  [x] = mhTypeB  [i];
    monthA   [x] = monthB   [i];
    npA      [x] = npB      [i];
    costCodeA[x] = costCodeB[i];
    x = x + 1;
  }

  return a;
}

Rcpp::DataFrame mhdbInit ( const R_xlen_t& n )
{
  Rcpp::StringVector  id       ( 12 * n, Rcpp::StringVector::get_na()  );
  Rcpp::StringVector  reqid    ( 12 * n, Rcpp::StringVector::get_na()  );
  Rcpp::IntegerVector mh       ( 12 * n, 0 );
  Rcpp::StringVector  mhType   ( 12 * n, Rcpp::StringVector::get_na()  );
  Rcpp::IntegerVector month    ( 12 * n, Rcpp::IntegerVector::get_na() );
  Rcpp::NumericVector np       ( 12 * n, 0 );
  Rcpp::StringVector  costCode ( 12 * n, Rcpp::StringVector::get_na()  );
  Rcpp::DataFrame mhDB = Rcpp::DataFrame::create(
    Rcpp::Named( "ID"               ) = id,
    Rcpp::Named( "reqID"            ) = reqid,
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
                                   Rcpp::String reqID,
                                   Rcpp::String cc )
{
  Rcpp::DataFrame mhDB = mhdbInit( mhTypes.length() );
  Rcpp::StringVector id       = Rcpp::as<Rcpp::StringVector >( mhDB["ID"      ] );
  Rcpp::StringVector reqid    = Rcpp::as<Rcpp::StringVector >( mhDB["reqID"   ] );
  Rcpp::StringVector mhType   = Rcpp::as<Rcpp::StringVector >( mhDB["mhType"  ] );
  Rcpp::IntegerVector month   = Rcpp::as<Rcpp::IntegerVector>( mhDB["month"   ] );
  Rcpp::StringVector costCode = Rcpp::as<Rcpp::StringVector >( mhDB["costCode"] );
  for ( int i = 0; i < mhTypes.length(); i++ )
  {
    for ( int j = 1; j < 13; j++ )
    {
      R_xlen_t idx  = ( i * 12 ) + ( j - 1 );
      id      [idx] = empID;
      reqid   [idx] = reqID;
      mhType  [idx] = mhTypes[i];
      month   [idx] = j;
      costCode[idx] = cc;
    }
  }
  return mhDB;
}

Rcpp::DataFrame testAndExpand ( Rcpp::DataFrame a,
                                const R_xlen_t& x,
                                const R_xlen_t& y )
{
  R_xlen_t lengthA = ( Rcpp::as<Rcpp::StringVector>( a["ID"] ) ).length();
  if ( y <= ( lengthA - x ) )
    return a;
  Rcpp::Rcout << "DataFrame rows not enough. Expanding…\n";
  Rcpp::DataFrame dfy = mhdbInit( y + lengthA );
  Rcpp::DataFrame b = combineMHDB( a, dfy );
  return b;
}