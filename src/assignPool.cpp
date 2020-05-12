#include <Rcpp.h>
#include "assignemp.h"
#include "gethours.h"

Rcpp::Environment mansched  = Rcpp::Environment::namespace_env( "mansched" );
Rcpp::Function    dfAppend  = mansched["dfAppend"];

// Is employee assignable to requirement? Assignable means both objects have
// man-hours with the same man-hour type.
bool assignable ( const Rcpp::S4& requirement, const Rcpp::S4& pool )
{
  Rcpp::DataFrame hoursT = getHours( requirement );
  Rcpp::DataFrame hoursR = getHours( pool );
  hoursR["reg"  ] = Rcpp::as<Rcpp::IntegerVector>( hoursR["reg"  ] ) + Rcpp::as<Rcpp::IntegerVector>( hoursR["rd"  ] );
  hoursR["sh"   ] = Rcpp::as<Rcpp::IntegerVector>( hoursR["sh"   ] ) + Rcpp::as<Rcpp::IntegerVector>( hoursR["rs"  ] );
  hoursR["lh"   ] = Rcpp::as<Rcpp::IntegerVector>( hoursR["lh"   ] ) + Rcpp::as<Rcpp::IntegerVector>( hoursR["rl"  ] );
  hoursR["nh"   ] = Rcpp::as<Rcpp::IntegerVector>( hoursR["nh"   ] ) + Rcpp::as<Rcpp::IntegerVector>( hoursR["rn"  ] );
  hoursR["regOT"] = Rcpp::as<Rcpp::IntegerVector>( hoursR["regOT"] ) + Rcpp::as<Rcpp::IntegerVector>( hoursR["rdOT"] );
  hoursR["shOT" ] = Rcpp::as<Rcpp::IntegerVector>( hoursR["shOT" ] ) + Rcpp::as<Rcpp::IntegerVector>( hoursR["rsOT"] );
  hoursR["lhOT" ] = Rcpp::as<Rcpp::IntegerVector>( hoursR["lhOT" ] ) + Rcpp::as<Rcpp::IntegerVector>( hoursR["rlOT"] );
  hoursR["nhOT" ] = Rcpp::as<Rcpp::IntegerVector>( hoursR["nhOT" ] ) + Rcpp::as<Rcpp::IntegerVector>( hoursR["rnOT"] );

  for ( int col = 0; col < hoursT.length(); col++ )
  {
    for ( int row = 0; row < Rcpp::as<Rcpp::IntegerVector>( hoursT[col] ).length(); row++ )
    {
      if ( Rcpp::as<Rcpp::IntegerVector>( hoursT[col] )[row] > 0 && Rcpp::as<Rcpp::IntegerVector>( hoursR[col] )[row] > 0 )
      {
        return true;
      }
    }
  }
  return false;
}

// Returns the total available working hours of an employee
int availableHours( const Rcpp::S4& employee )
{
  int totalHours;
  Rcpp::DataFrame empHours = getHours( employee );
  totalHours = Rcpp::sum( Rcpp::as<Rcpp::IntegerVector>( empHours["reg"  ] ) ) +
               Rcpp::sum( Rcpp::as<Rcpp::IntegerVector>( empHours["rd"   ] ) ) +
               Rcpp::sum( Rcpp::as<Rcpp::IntegerVector>( empHours["lh"   ] ) ) +
               Rcpp::sum( Rcpp::as<Rcpp::IntegerVector>( empHours["sh"   ] ) ) +
               Rcpp::sum( Rcpp::as<Rcpp::IntegerVector>( empHours["nh"   ] ) ) +
               Rcpp::sum( Rcpp::as<Rcpp::IntegerVector>( empHours["rl"   ] ) ) +
               Rcpp::sum( Rcpp::as<Rcpp::IntegerVector>( empHours["rs"   ] ) ) +
               Rcpp::sum( Rcpp::as<Rcpp::IntegerVector>( empHours["rn"   ] ) ) +
               Rcpp::sum( Rcpp::as<Rcpp::IntegerVector>( empHours["regOT"] ) ) +
               Rcpp::sum( Rcpp::as<Rcpp::IntegerVector>( empHours["rdOT" ] ) ) +
               Rcpp::sum( Rcpp::as<Rcpp::IntegerVector>( empHours["lhOT" ] ) ) +
               Rcpp::sum( Rcpp::as<Rcpp::IntegerVector>( empHours["shOT" ] ) ) +
               Rcpp::sum( Rcpp::as<Rcpp::IntegerVector>( empHours["nhOT" ] ) ) +
               Rcpp::sum( Rcpp::as<Rcpp::IntegerVector>( empHours["rlOT" ] ) ) +
               Rcpp::sum( Rcpp::as<Rcpp::IntegerVector>( empHours["rsOT" ] ) ) +
               Rcpp::sum( Rcpp::as<Rcpp::IntegerVector>( empHours["rnOT" ] ) );
  return totalHours;
}

// Returns the indexes of the elements with TRUE values in a LogicalVector
Rcpp::IntegerVector which( const Rcpp::LogicalVector& v, bool sample = true )
{
  Rcpp::IntegerVector out ( v.length() );
  R_xlen_t n = 0;
  for ( R_xlen_t i = 0; i < v.length(); i++ )
  {
    if ( v[i] )
    {
      out[n] = i;
      n = n + 1;
    }
  }

  if ( n < v.length() )
    out.erase( n, v.length() );

  if ( sample )
    return Rcpp::sample( out, n, false );
  else
    return out;
}

// Returns the original indexes of a sorted vector
Rcpp::IntegerVector sortedIdx( const Rcpp::IntegerVector& v ) {
  int n = v.length();
  Rcpp::IntegerVector idx = Rcpp::seq( 0, n - 1 );
  std::sort( idx.begin(), idx.end(), [&]( int i, int j ) {
    return v[i] < v[j];
  });
  return idx;
}

//' Assign pool to requirement faster
//'
//' This is used in \code{\link{assignPrio}}.
//'
//' @param empReq passed from \code{\link{assignPrio}}
//' @param empPool passed from \code{\link{assignPrio}}
//' @param listT list of theoretical employees created from empReq
//' @param listR list of real employees created fro empPool
//' @param prioStat character vector defining the employee status that are
//'   prioritized in assigning man hours
//' @param prioCode logical value \cr
//'   Is cost code prioritized in assigning?
//' @return a list containing the following:
//'   \enumerate{
//'     \item remaining empReq
//'     \item remaining empPool
//'     \item remaining listT
//'     \item remaining listR
//'     \item man hour database resulting from the assignment
//'
//'       This is also merged and described well in \code{\link{getmhDB}}.
//'   }
//' @export
// [[Rcpp::export]]
Rcpp::List assignPool( Rcpp::DataFrame empReq,
                       Rcpp::DataFrame empPool,
                       Rcpp::List listT,
                       Rcpp::List listR,
                       Rcpp::StringVector prioStat = NA_STRING,
                       bool prioCode = false )
{
  Rcpp::Rcout << "\n\n*************************"
              << "* Begin asignPool *"
              << "*************************\n\n";

  int empHours, i, j, k;
  Rcpp::List listTC = Rcpp::clone( listT );
  Rcpp::List listRC = Rcpp::clone( listR );

  Rcpp::StringVector reqID       = Rcpp::clone( Rcpp::as<Rcpp::StringVector  >( empReq["ID"             ] ) );
  Rcpp::StringVector reqCostCode = Rcpp::clone( Rcpp::as<Rcpp::StringVector  >( empReq["costCode"       ] ) );
  Rcpp::StringVector reqClass    = Rcpp::clone( Rcpp::as<Rcpp::StringVector  >( empReq["personnelClass" ] ) );

  Rcpp::StringVector  poolID          = Rcpp::clone( Rcpp::as<Rcpp::StringVector >( empPool["ID"            ] ) );
  Rcpp::StringVector  poolName        = Rcpp::clone( Rcpp::as<Rcpp::StringVector >( empPool["name"          ] ) );
  Rcpp::StringVector  poolDesignation = Rcpp::clone( Rcpp::as<Rcpp::StringVector >( empPool["designation"   ] ) );
  Rcpp::StringVector  poolClass       = Rcpp::clone( Rcpp::as<Rcpp::StringVector >( empPool["personnelClass"] ) );
  Rcpp::LogicalVector poolField       = Rcpp::clone( Rcpp::as<Rcpp::LogicalVector>( empPool["field"         ] ) );
  Rcpp::StringVector  poolEquipment   = Rcpp::clone( Rcpp::as<Rcpp::StringVector >( empPool["equipment"     ] ) );
  Rcpp::StringVector  poolCostCode    = Rcpp::clone( Rcpp::as<Rcpp::StringVector >( empPool["costCode"      ] ) );
  Rcpp::StringVector  poolStatus      = Rcpp::clone( Rcpp::as<Rcpp::StringVector >( empPool["status"        ] ) );
  Rcpp::StringVector  poolCBegin      = Rcpp::clone( Rcpp::as<Rcpp::StringVector >( empPool["cBegin"        ] ) );
  Rcpp::StringVector  poolCEnd        = Rcpp::clone( Rcpp::as<Rcpp::StringVector >( empPool["cEnd"          ] ) );
  Rcpp::LogicalVector poolInHouse     = Rcpp::clone( Rcpp::as<Rcpp::LogicalVector>( empPool["inHouse"       ] ) );
  Rcpp::StringVector  poolRestDay     = Rcpp::clone( Rcpp::as<Rcpp::StringVector >( empPool["restday"       ] ) );
  Rcpp::LogicalVector poolIsRF        = Rcpp::clone( Rcpp::as<Rcpp::LogicalVector>( empPool["isRF"          ] ) );
  Rcpp::IntegerVector poolJan         = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["JAN"           ] ) );
  Rcpp::IntegerVector poolFeb         = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["FEB"           ] ) );
  Rcpp::IntegerVector poolMar         = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["MAR"           ] ) );
  Rcpp::IntegerVector poolApr         = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["APR"           ] ) );
  Rcpp::IntegerVector poolMay         = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["MAY"           ] ) );
  Rcpp::IntegerVector poolJun         = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["JUN"           ] ) );
  Rcpp::IntegerVector poolJul         = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["JUL"           ] ) );
  Rcpp::IntegerVector poolAug         = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["AUG"           ] ) );
  Rcpp::IntegerVector poolSep         = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["SEP"           ] ) );
  Rcpp::IntegerVector poolOct         = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["OCT"           ] ) );
  Rcpp::IntegerVector poolNov         = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["NOV"           ] ) );
  Rcpp::IntegerVector poolDec         = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["DEC"           ] ) );
  Rcpp::IntegerVector poolDRD01       = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["d.rd_1"        ] ) );
  Rcpp::IntegerVector poolDRD02       = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["d.rd_2"        ] ) );
  Rcpp::IntegerVector poolDRD03       = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["d.rd_3"        ] ) );
  Rcpp::IntegerVector poolDRD04       = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["d.rd_4"        ] ) );
  Rcpp::IntegerVector poolDRD05       = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["d.rd_5"        ] ) );
  Rcpp::IntegerVector poolDRD06       = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["d.rd_6"        ] ) );
  Rcpp::IntegerVector poolDRD07       = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["d.rd_7"        ] ) );
  Rcpp::IntegerVector poolDRD08       = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["d.rd_8"        ] ) );
  Rcpp::IntegerVector poolDRD09       = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["d.rd_9"        ] ) );
  Rcpp::IntegerVector poolDRD10       = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["d.rd_10"       ] ) );
  Rcpp::IntegerVector poolDRD11       = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["d.rd_11"       ] ) );
  Rcpp::IntegerVector poolDRD12       = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["d.rd_12"       ] ) );
  Rcpp::IntegerVector poolDHO01       = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["d.ho_1"        ] ) );
  Rcpp::IntegerVector poolDHO02       = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["d.ho_2"        ] ) );
  Rcpp::IntegerVector poolDHO03       = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["d.ho_3"        ] ) );
  Rcpp::IntegerVector poolDHO04       = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["d.ho_4"        ] ) );
  Rcpp::IntegerVector poolDHO05       = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["d.ho_5"        ] ) );
  Rcpp::IntegerVector poolDHO06       = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["d.ho_6"        ] ) );
  Rcpp::IntegerVector poolDHO07       = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["d.ho_7"        ] ) );
  Rcpp::IntegerVector poolDHO08       = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["d.ho_8"        ] ) );
  Rcpp::IntegerVector poolDHO09       = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["d.ho_9"        ] ) );
  Rcpp::IntegerVector poolDHO10       = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["d.ho_10"       ] ) );
  Rcpp::IntegerVector poolDHO11       = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["d.ho_11"       ] ) );
  Rcpp::IntegerVector poolDHO12       = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["d.ho_12"       ] ) );
  Rcpp::IntegerVector poolDRH01       = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["d.rh_1"        ] ) );
  Rcpp::IntegerVector poolDRH02       = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["d.rh_2"        ] ) );
  Rcpp::IntegerVector poolDRH03       = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["d.rh_3"        ] ) );
  Rcpp::IntegerVector poolDRH04       = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["d.rh_4"        ] ) );
  Rcpp::IntegerVector poolDRH05       = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["d.rh_5"        ] ) );
  Rcpp::IntegerVector poolDRH06       = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["d.rh_6"        ] ) );
  Rcpp::IntegerVector poolDRH07       = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["d.rh_7"        ] ) );
  Rcpp::IntegerVector poolDRH08       = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["d.rh_8"        ] ) );
  Rcpp::IntegerVector poolDRH09       = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["d.rh_9"        ] ) );
  Rcpp::IntegerVector poolDRH10       = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["d.rh_10"       ] ) );
  Rcpp::IntegerVector poolDRH11       = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["d.rh_11"       ] ) );
  Rcpp::IntegerVector poolDRH12       = Rcpp::clone( Rcpp::as<Rcpp::IntegerVector>( empPool["d.rh_12"       ] ) );
  Rcpp::StringVector  poolDCC         = Rcpp::clone( Rcpp::as<Rcpp::StringVector >( empPool["dcc"           ] ) );
  Rcpp::NumericVector poolA01         = Rcpp::clone( Rcpp::as<Rcpp::NumericVector>( empPool["a_1"           ] ) );
  Rcpp::NumericVector poolA02         = Rcpp::clone( Rcpp::as<Rcpp::NumericVector>( empPool["a_2"           ] ) );
  Rcpp::NumericVector poolA03         = Rcpp::clone( Rcpp::as<Rcpp::NumericVector>( empPool["a_3"           ] ) );
  Rcpp::NumericVector poolA04         = Rcpp::clone( Rcpp::as<Rcpp::NumericVector>( empPool["a_4"           ] ) );
  Rcpp::NumericVector poolA05         = Rcpp::clone( Rcpp::as<Rcpp::NumericVector>( empPool["a_5"           ] ) );
  Rcpp::NumericVector poolA06         = Rcpp::clone( Rcpp::as<Rcpp::NumericVector>( empPool["a_6"           ] ) );
  Rcpp::NumericVector poolA07         = Rcpp::clone( Rcpp::as<Rcpp::NumericVector>( empPool["a_7"           ] ) );
  Rcpp::NumericVector poolA08         = Rcpp::clone( Rcpp::as<Rcpp::NumericVector>( empPool["a_8"           ] ) );
  Rcpp::NumericVector poolA09         = Rcpp::clone( Rcpp::as<Rcpp::NumericVector>( empPool["a_9"           ] ) );
  Rcpp::NumericVector poolA10         = Rcpp::clone( Rcpp::as<Rcpp::NumericVector>( empPool["a_10"          ] ) );
  Rcpp::NumericVector poolA11         = Rcpp::clone( Rcpp::as<Rcpp::NumericVector>( empPool["a_11"          ] ) );
  Rcpp::NumericVector poolA12         = Rcpp::clone( Rcpp::as<Rcpp::NumericVector>( empPool["a_12"          ] ) );

  Rcpp::StringVector  tempEquip   (0);
  Rcpp::LogicalVector toBeRemoved (0);
  Rcpp::IntegerVector mh          ( 10000, Rcpp::IntegerVector::get_na() );
  Rcpp::IntegerVector month       ( 10000, Rcpp::IntegerVector::get_na() );
  Rcpp::IntegerVector np          ( 10000, Rcpp::IntegerVector::get_na() );
  Rcpp::StringVector  costCode    ( 10000, Rcpp::StringVector::get_na()  );
  Rcpp::StringVector  id          ( 10000, Rcpp::StringVector::get_na()  );
  Rcpp::StringVector  mhType      ( 10000, Rcpp::StringVector::get_na()  );

  Rcpp::DataFrame mhDB = Rcpp::DataFrame::create(
    Rcpp::Named( "ID"               ) = id,
    Rcpp::Named( "mh"               ) = mh,
    Rcpp::Named( "mhType"           ) = mhType,
    Rcpp::Named( "month"            ) = month,
    Rcpp::Named( "np"               ) = np,
    Rcpp::Named( "costCode"         ) = costCode,
    Rcpp::Named( "stringsAsFactors" ) = false
  );

  for ( i = 0; i < listTC.length(); i++ )
  {
    empHours = availableHours( Rcpp::as<Rcpp::S4>( listTC[i] ) );
    if ( empHours == 0 )
      continue;

    Rcpp::Rcout << "Assigning personnel for "
                << Rcpp::as<Rcpp::StringVector>( Rcpp::as<Rcpp::S4>( listTC[i] ).slot( "ID" ) )
                << ".\n"
                << "Iteration: "
                << i
                << " / "
                << listTC.length()
                << ".\n";

    if ( listRC.length() < 1 )
    {
      Rcpp::Rcout << "No more available employee pool for the required personnel.";
      listRC = R_NilValue;
      break;
    }

    if ( Rcpp::as<Rcpp::S4>( listTC[i] ).hasSlot( "equipment" ) )
    {
      tempEquip = Rcpp::as<Rcpp::String>( Rcpp::as<Rcpp::S4>( listTC[i] ).slot( "equipment" ) );
    }
    else
    {
      tempEquip = NA_STRING;
    }

    Rcpp::StringVector tempCostCode = Rcpp::as<Rcpp::S4>( listTC[i] ).slot( "costCode" );
    Rcpp::LogicalVector matchClass    ( listRC.length(), FALSE );
    Rcpp::LogicalVector matchEquip    ( listRC.length(), FALSE );
    Rcpp::LogicalVector matchCostCode ( listRC.length(), TRUE  );
    Rcpp::LogicalVector choice        ( listRC.length(), FALSE );
    for ( j = 0; j < listRC.length(); j++ )
    {
      if ( Rcpp::any( Rcpp::is_na( prioStat ) ).is_true() )
        matchClass[j] = TRUE;
      else
      {
        if ( Rcpp::intersect( Rcpp::as<Rcpp::StringVector>( Rcpp::as<Rcpp::S4>( listRC[j] ).slot( "status" ) ), prioStat ).length() > 0 )
          matchClass[j] = TRUE;
      }

      if ( Rcpp::is_na( tempEquip )[0] )
        matchEquip[j] = TRUE;
      else if ( Rcpp::as<Rcpp::S4>( listRC[j] ).hasSlot( "equipment" ) &&
                Rcpp::intersect( tempEquip, Rcpp::as<Rcpp::StringVector>( Rcpp::as<Rcpp::S4>( listRC[j] ).slot( "equipment" ) ) ).length() > 0 )
        matchEquip[j] = TRUE;

      if ( prioCode &&
           Rcpp::intersect( tempCostCode, Rcpp::as<Rcpp::StringVector>( Rcpp::as<Rcpp::S4>( listRC[j] ).slot( "costCode" ) ) ).length() < 1 )
        matchCostCode[j] = FALSE;

      choice[j] = matchClass[j] &&
                  matchEquip[j] &&
                  matchCostCode[j];
    }
    Rcpp::LogicalVector choices = choice[choice == TRUE];
    Rcpp::Rcout << "Identified "
                << choices.length()
                << " personnel to be assigned to "
                << Rcpp::as<Rcpp::StringVector>( Rcpp::as<Rcpp::S4>( listTC[i] ).slot( "ID" ) )
                << ".\n";
    if ( choices.length() > 0 )
    {
      Rcpp::IntegerVector index = which( choice );

      if ( Rcpp::as<Rcpp::S4>( listTC[i] ).hasSlot( "equipment" ) )
      {
        // Store number of equipment authorized for each personnel
        Rcpp::IntegerVector nEquip ( index.length() );
        for ( k = 0; k < index.length(); k++ )
          nEquip[k] = Rcpp::as<Rcpp::StringVector>( Rcpp::as<Rcpp::S4>( listRC[(int) index[k]] ).slot( "equipment" ) ).length();
        // Prioritize assigning of operators with lower number of equipment
        // authorized. Multi-skilled operators are assigned last.
        index = index[sortedIdx( nEquip )];
      }

      // Assign employees
      if ( availableHours( Rcpp::as<Rcpp::S4>( listTC[i] ) ) > 0 )
      {
        for ( Rcpp::IntegerVector::iterator jj = index.begin(); jj != index.end(); jj++ )
        {
          if ( !assignable( listTC[i] , listRC[*jj] ) )
            continue;

          Rcpp::Rcout << "Assigning "
                      << Rcpp::as<Rcpp::StringVector>( Rcpp::as<Rcpp::S4>( listRC[*jj] ).slot( "ID" ) )
                      << " to "
                      << Rcpp::as<Rcpp::StringVector>( Rcpp::as<Rcpp::S4>( listTC[i] ).slot( "ID" ) )
                      << "\n"
                      << "MH Req: "
                      << availableHours( listTC[i] )
                      << "\nMH Pool: "
                      << availableHours( listRC[*jj] )
                      << "\n";
          Rcpp::DataFrame tempData = assignEmp( listTC[i], listRC[*jj], false );
          Rcpp::Rcout << Rcpp::as<Rcpp::StringVector>( Rcpp::as<Rcpp::S4>( listRC[*jj] ).slot( "ID" ) );

          int assignedMH = Rcpp::sum(Rcpp::as<Rcpp::IntegerVector>( tempData["mh"] ) );
          if ( assignedMH > 0 )
          {
            Rcpp::Rcout << " assigned.\nTotal Assigned: "
                        << assignedMH
                        << "\nRemaining man-hours\nMH Req: "
                        << availableHours( listTC[i] )
                        << "\nMH Pool: "
                        << availableHours( listRC[*jj] )
                        << "\n";
            mhDB = dfAppend( mhDB, tempData );
          }
          if ( availableHours(listTC[i] ) < 1 )
            break;
        }
      }

      toBeRemoved = Rcpp::LogicalVector ( listRC.length(), FALSE );
      for ( j = 0; j < listRC.length(); j++ )
      {
        if ( availableHours( listRC[j] ) < 1 )
        {
          toBeRemoved[j] = TRUE;
          Rcpp::Rcout << Rcpp::as<Rcpp::StringVector>( Rcpp::as<Rcpp::S4>( listRC[j] ).slot( "ID" ) )
                      << " is fully spent.\n";
        }
      }
      // Retain personnel pool with available man hours
      if ( Rcpp::any( toBeRemoved ).is_true() )
      {
        listRC          = listRC         [!toBeRemoved];
        poolID          = poolID         [!toBeRemoved];
        poolName        = poolName       [!toBeRemoved];
        poolDesignation = poolDesignation[!toBeRemoved];
        poolClass       = poolClass      [!toBeRemoved];
        poolField       = poolField      [!toBeRemoved];
        poolEquipment   = poolEquipment  [!toBeRemoved];
        poolCostCode    = poolCostCode   [!toBeRemoved];
        poolStatus      = poolStatus     [!toBeRemoved];
        poolCBegin      = poolCBegin     [!toBeRemoved];
        poolCEnd        = poolCEnd       [!toBeRemoved];
        poolInHouse     = poolInHouse    [!toBeRemoved];
        poolRestDay     = poolRestDay    [!toBeRemoved];
        poolIsRF        = poolIsRF       [!toBeRemoved];
        poolJan         = poolJan        [!toBeRemoved];
        poolFeb         = poolFeb        [!toBeRemoved];
        poolMar         = poolMar        [!toBeRemoved];
        poolApr         = poolApr        [!toBeRemoved];
        poolMay         = poolMay        [!toBeRemoved];
        poolJun         = poolJun        [!toBeRemoved];
        poolJul         = poolJul        [!toBeRemoved];
        poolAug         = poolAug        [!toBeRemoved];
        poolSep         = poolSep        [!toBeRemoved];
        poolOct         = poolOct        [!toBeRemoved];
        poolNov         = poolNov        [!toBeRemoved];
        poolDec         = poolDec        [!toBeRemoved];
        poolDRD01       = poolDRD01      [!toBeRemoved];
        poolDRD02       = poolDRD02      [!toBeRemoved];
        poolDRD03       = poolDRD03      [!toBeRemoved];
        poolDRD04       = poolDRD04      [!toBeRemoved];
        poolDRD05       = poolDRD05      [!toBeRemoved];
        poolDRD06       = poolDRD06      [!toBeRemoved];
        poolDRD07       = poolDRD07      [!toBeRemoved];
        poolDRD08       = poolDRD08      [!toBeRemoved];
        poolDRD09       = poolDRD09      [!toBeRemoved];
        poolDRD10       = poolDRD10      [!toBeRemoved];
        poolDRD11       = poolDRD11      [!toBeRemoved];
        poolDRD12       = poolDRD12      [!toBeRemoved];
        poolDHO01       = poolDHO01      [!toBeRemoved];
        poolDHO02       = poolDHO02      [!toBeRemoved];
        poolDHO03       = poolDHO03      [!toBeRemoved];
        poolDHO04       = poolDHO04      [!toBeRemoved];
        poolDHO05       = poolDHO05      [!toBeRemoved];
        poolDHO06       = poolDHO06      [!toBeRemoved];
        poolDHO07       = poolDHO07      [!toBeRemoved];
        poolDHO08       = poolDHO08      [!toBeRemoved];
        poolDHO09       = poolDHO09      [!toBeRemoved];
        poolDHO10       = poolDHO10      [!toBeRemoved];
        poolDHO11       = poolDHO11      [!toBeRemoved];
        poolDHO12       = poolDHO12      [!toBeRemoved];
        poolDRH01       = poolDRH01      [!toBeRemoved];
        poolDRH02       = poolDRH02      [!toBeRemoved];
        poolDRH03       = poolDRH03      [!toBeRemoved];
        poolDRH04       = poolDRH04      [!toBeRemoved];
        poolDRH05       = poolDRH05      [!toBeRemoved];
        poolDRH06       = poolDRH06      [!toBeRemoved];
        poolDRH07       = poolDRH07      [!toBeRemoved];
        poolDRH08       = poolDRH08      [!toBeRemoved];
        poolDRH09       = poolDRH09      [!toBeRemoved];
        poolDRH10       = poolDRH10      [!toBeRemoved];
        poolDRH11       = poolDRH11      [!toBeRemoved];
        poolDRH12       = poolDRH12      [!toBeRemoved];
        poolDCC         = poolDCC        [!toBeRemoved];
        poolA01         = poolA01        [!toBeRemoved];
        poolA02         = poolA02        [!toBeRemoved];
        poolA03         = poolA03        [!toBeRemoved];
        poolA04         = poolA04        [!toBeRemoved];
        poolA05         = poolA05        [!toBeRemoved];
        poolA06         = poolA06        [!toBeRemoved];
        poolA07         = poolA07        [!toBeRemoved];
        poolA08         = poolA08        [!toBeRemoved];
        poolA09         = poolA09        [!toBeRemoved];
        poolA10         = poolA10        [!toBeRemoved];
        poolA11         = poolA11        [!toBeRemoved];
        poolA12         = poolA12        [!toBeRemoved];
      }
    }
  }

  toBeRemoved = Rcpp::LogicalVector ( listTC.length(), FALSE );
  for ( i = 0; i < listTC.length(); i++ )
  {
    if ( availableHours( listTC[i] ) < 1 )
    {
      toBeRemoved[i] = TRUE;
      Rcpp::Rcout << Rcpp::as<Rcpp::StringVector>( Rcpp::as<Rcpp::S4>( listTC[i] ).slot( "ID" ) )
                  << " is fully spent.\n";
    }
  }
  if ( Rcpp::any( toBeRemoved ).is_true() )
  {
    listTC      = listTC      [!toBeRemoved];
    reqID       = reqID       [!toBeRemoved];
    reqCostCode = reqCostCode [!toBeRemoved];
    reqClass    = reqClass    [!toBeRemoved];
  }

  Rcpp::DataFrame empReqC = Rcpp::DataFrame::create(
    Rcpp::Named( "ID"               ) = reqID,
    Rcpp::Named( "costCode"         ) = reqCostCode,
    Rcpp::Named( "personnelClass"   ) = reqClass,
    Rcpp::Named( "stringsAsFactors" ) = false
  );

  Rcpp::DataFrame empPoolC = Rcpp::DataFrame::create(
    Rcpp::Named( "ID"               ) = poolID,
    Rcpp::Named( "name"             ) = poolName,
    Rcpp::Named( "designation"      ) = poolDesignation,
    Rcpp::Named( "personnelClass"   ) = poolClass,
    Rcpp::Named( "field"            ) = poolField,
    Rcpp::Named( "equipment"        ) = poolEquipment,
    Rcpp::Named( "costCode"         ) = poolCostCode,
    Rcpp::Named( "status"           ) = poolStatus,
    Rcpp::Named( "cBegin"           ) = poolCBegin,
    Rcpp::Named( "cEnd"             ) = poolCEnd,
    Rcpp::Named( "inHouse"          ) = poolInHouse,
    Rcpp::Named( "restday"          ) = poolRestDay,
    Rcpp::Named( "isRF"             ) = poolIsRF,
    Rcpp::Named( "stringsAsFactors" ) = false
  );
  empPoolC.push_back( poolJan,   "JAN"     );
  empPoolC.push_back( poolFeb,   "FEB"     );
  empPoolC.push_back( poolMar,   "MAR"     );
  empPoolC.push_back( poolApr,   "APR"     );
  empPoolC.push_back( poolMay,   "MAY"     );
  empPoolC.push_back( poolJun,   "JUN"     );
  empPoolC.push_back( poolJul,   "JUL"     );
  empPoolC.push_back( poolAug,   "AUG"     );
  empPoolC.push_back( poolSep,   "SEP"     );
  empPoolC.push_back( poolOct,   "OCT"     );
  empPoolC.push_back( poolNov,   "NOV"     );
  empPoolC.push_back( poolDec,   "DEC"     );
  empPoolC.push_back( poolDRD01, "d.rd_1"  );
  empPoolC.push_back( poolDRD02, "d.rd_2"  );
  empPoolC.push_back( poolDRD03, "d.rd_3"  );
  empPoolC.push_back( poolDRD04, "d.rd_4"  );
  empPoolC.push_back( poolDRD05, "d.rd_5"  );
  empPoolC.push_back( poolDRD06, "d.rd_6"  );
  empPoolC.push_back( poolDRD07, "d.rd_7"  );
  empPoolC.push_back( poolDRD08, "d.rd_8"  );
  empPoolC.push_back( poolDRD09, "d.rd_9"  );
  empPoolC.push_back( poolDRD10, "d.rd_10" );
  empPoolC.push_back( poolDRD11, "d.rd_11" );
  empPoolC.push_back( poolDRD12, "d.rd_12" );
  empPoolC.push_back( poolDHO01, "d.ho_1"  );
  empPoolC.push_back( poolDHO02, "d.ho_2"  );
  empPoolC.push_back( poolDHO03, "d.ho_3"  );
  empPoolC.push_back( poolDHO04, "d.ho_4"  );
  empPoolC.push_back( poolDHO05, "d.ho_5"  );
  empPoolC.push_back( poolDHO06, "d.ho_6"  );
  empPoolC.push_back( poolDHO07, "d.ho_7"  );
  empPoolC.push_back( poolDHO08, "d.ho_8"  );
  empPoolC.push_back( poolDHO09, "d.ho_9"  );
  empPoolC.push_back( poolDHO10, "d.ho_10" );
  empPoolC.push_back( poolDHO11, "d.ho_11" );
  empPoolC.push_back( poolDHO12, "d.ho_12" );
  empPoolC.push_back( poolDRH01, "d.rh_1"  );
  empPoolC.push_back( poolDRH02, "d.rh_2"  );
  empPoolC.push_back( poolDRH03, "d.rh_3"  );
  empPoolC.push_back( poolDRH04, "d.rh_4"  );
  empPoolC.push_back( poolDRH05, "d.rh_5"  );
  empPoolC.push_back( poolDRH06, "d.rh_6"  );
  empPoolC.push_back( poolDRH07, "d.rh_7"  );
  empPoolC.push_back( poolDRH08, "d.rh_8"  );
  empPoolC.push_back( poolDRH09, "d.rh_9"  );
  empPoolC.push_back( poolDRH10, "d.rh_10" );
  empPoolC.push_back( poolDRH11, "d.rh_11" );
  empPoolC.push_back( poolDRH12, "d.rh_12" );
  empPoolC.push_back( poolDCC,   "dcc"  );
  empPoolC.push_back( poolA01,   "a_1"  );
  empPoolC.push_back( poolA02,   "a_2"  );
  empPoolC.push_back( poolA03,   "a_3"  );
  empPoolC.push_back( poolA04,   "a_4"  );
  empPoolC.push_back( poolA05,   "a_5"  );
  empPoolC.push_back( poolA06,   "a_6"  );
  empPoolC.push_back( poolA07,   "a_7"  );
  empPoolC.push_back( poolA08,   "a_8"  );
  empPoolC.push_back( poolA09,   "a_9"  );
  empPoolC.push_back( poolA10,   "a_10" );
  empPoolC.push_back( poolA11,   "a_11" );
  empPoolC.push_back( poolA12,   "a_12" );
  empPoolC.attr( "class" ) = "data.frame";
  empPoolC.attr( "row.names" ) = Rcpp::IntegerVector::create( NA_INTEGER, XLENGTH( poolID ) );

  costCode = mhDB["costCode"];
  id       = mhDB["ID"      ];
  mh       = mhDB["mh"      ];
  mhType   = mhDB["mhType"  ];
  month    = mhDB["month"   ];
  np       = mhDB["np"      ];

  // Remove NA values at the bottom
  Rcpp::LogicalVector withValues = !Rcpp::is_na( mh );
  costCode = costCode[withValues];
  id       = id      [withValues];
  mh       = mh      [withValues];
  mhType   = mhType  [withValues];
  month    = month   [withValues];
  np       = np      [withValues];

  mhDB = Rcpp::DataFrame::create(
    Rcpp::Named( "ID"               ) = id,
    Rcpp::Named( "mh"               ) = mh,
    Rcpp::Named( "mhType"           ) = mhType,
    Rcpp::Named( "month"            ) = month,
    Rcpp::Named( "np"               ) = np,
    Rcpp::Named( "costCode"         ) = costCode,
    Rcpp::Named( "stringsAsFactors" ) = false
  );

  Rcpp::List out = Rcpp::List::create( empReqC, empPoolC, listTC, listRC, mhDB );
  Rcpp::Rcout << "\n\n***********************"
              << "**** End asignPool ****"
              << "***********************\n\n";
  return out;
}
