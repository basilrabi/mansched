#include "gethours.h"

Rcpp::DataFrame getHours ( const Rcpp::S4& employee )
{
  Rcpp::IntegerVector reg = employee.slot( "reg" );
  Rcpp::IntegerVector regOT ( 12, 0 );
  Rcpp::IntegerVector rd    ( 12, 0 );
  Rcpp::IntegerVector sh    ( 12, 0 );
  Rcpp::IntegerVector lh    ( 12, 0 );
  Rcpp::IntegerVector nh    ( 12, 0 );
  Rcpp::IntegerVector rs    ( 12, 0 );
  Rcpp::IntegerVector rl    ( 12, 0 );
  Rcpp::IntegerVector rn    ( 12, 0 );
  Rcpp::IntegerVector rdOT  ( 12, 0 );
  Rcpp::IntegerVector shOT  ( 12, 0 );
  Rcpp::IntegerVector lhOT  ( 12, 0 );
  Rcpp::IntegerVector nhOT  ( 12, 0 );
  Rcpp::IntegerVector rsOT  ( 12, 0 );
  Rcpp::IntegerVector rlOT  ( 12, 0 );
  Rcpp::IntegerVector rnOT  ( 12, 0 );

  if ( employee.hasSlot( "regOT" ) )
    regOT = employee.slot( "regOT" );

  if ( employee.hasSlot( "rd" ) )
  {
    rd =   employee.slot( "rd"   );
    sh   = employee.slot( "sh"   );
    lh   = employee.slot( "lh"   );
    nh   = employee.slot( "nh"   );
    rs   = employee.slot( "rs"   );
    rl   = employee.slot( "rl"   );
    rn   = employee.slot( "rn"   );
    rdOT = employee.slot( "rdOT" );
    shOT = employee.slot( "shOT" );
    lhOT = employee.slot( "lhOT" );
    nhOT = employee.slot( "nhOT" );
    rsOT = employee.slot( "rsOT" );
    rlOT = employee.slot( "rlOT" );
    rnOT = employee.slot( "rnOT" );
  }

  Rcpp::DataFrame hours = Rcpp::DataFrame::create(
    Rcpp::Named( "reg"   ) = reg,
    Rcpp::Named( "rd"    ) = rd,
    Rcpp::Named( "lh"    ) = lh,
    Rcpp::Named( "sh"    ) = sh,
    Rcpp::Named( "nh"    ) = nh,
    Rcpp::Named( "rl"    ) = rl,
    Rcpp::Named( "rs"    ) = rs,
    Rcpp::Named( "rn"    ) = rn,
    Rcpp::Named( "regOT" ) = regOT,
    Rcpp::Named( "rdOT"  ) = rdOT,
    Rcpp::Named( "lhOT"  ) = lhOT,
    Rcpp::Named( "shOT"  ) = shOT,
    Rcpp::Named( "nhOT"  ) = nhOT,
    Rcpp::Named( "rlOT"  ) = rlOT,
    Rcpp::Named( "rsOT"  ) = rsOT,
    Rcpp::Named( "rnOT"  ) = rnOT
  );

  return hours;
}
