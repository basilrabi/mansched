#include <Rcpp.h>
#include "assignemp.h"
#include "gethours.h"
#include "mhdb.h"

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
//' @param listT list of theoretical employees created from empReq
//' @param listR list of real employees created fro empPool
//' @param prioStat character vector defining the employee status that are
//'   prioritized in assigning man hours
//' @param prioCode logical value \cr
//'   Is cost code prioritized in assigning?
//' @return a list containing the following:
//'   \enumerate{
//'     \item remaining listT
//'     \item remaining listR
//'     \item man hour database resulting from the assignment
//'
//'       This is also merged and described well in \code{\link{getmhDB}}.
//'   }
//' @export
// [[Rcpp::export]]
Rcpp::List assignPool( Rcpp::List listT,
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

  Rcpp::StringVector  tempEquip   (0);
  Rcpp::LogicalVector toBeRemoved (0);
  Rcpp::DataFrame mhDB = mhdbInit( 100 );
  R_xlen_t idx = 0;

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
            mhDB = dfAppend( mhDB, tempData, idx );
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
        listRC = listRC[!toBeRemoved];
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
    listTC = listTC[!toBeRemoved];

  Rcpp::StringVector  costCode = Rcpp::as<Rcpp::StringVector >( mhDB["costCode"] );
  Rcpp::StringVector  id       = Rcpp::as<Rcpp::StringVector >( mhDB["ID"      ] );
  Rcpp::StringVector  reqid    = Rcpp::as<Rcpp::StringVector >( mhDB["reqID"   ] );
  Rcpp::IntegerVector mh       = Rcpp::as<Rcpp::IntegerVector>( mhDB["mh"      ] );
  Rcpp::StringVector  mhType   = Rcpp::as<Rcpp::StringVector >( mhDB["mhType"  ] );
  Rcpp::IntegerVector month    = Rcpp::as<Rcpp::IntegerVector>( mhDB["month"   ] );
  Rcpp::NumericVector np       = Rcpp::as<Rcpp::NumericVector>( mhDB["np"      ] );

  // Remove NA values at the bottom
  Rcpp::LogicalVector withValues = mh > 0;
  costCode = costCode[withValues];
  id       = id      [withValues];
  reqid    = reqid   [withValues];
  mh       = mh      [withValues];
  mhType   = mhType  [withValues];
  month    = month   [withValues];
  np       = np      [withValues];

  mhDB = Rcpp::DataFrame::create(
    Rcpp::Named( "ID"               ) = id,
    Rcpp::Named( "reqID"            ) = reqid,
    Rcpp::Named( "mh"               ) = mh,
    Rcpp::Named( "mhType"           ) = mhType,
    Rcpp::Named( "month"            ) = month,
    Rcpp::Named( "np"               ) = np,
    Rcpp::Named( "costCode"         ) = costCode,
    Rcpp::Named( "stringsAsFactors" ) = false
  );

  Rcpp::List out = Rcpp::List::create( listTC, listRC, mhDB );
  Rcpp::Rcout << "\n\n***********************"
              << "**** End asignPool ****"
              << "***********************\n\n";
  return out;
}
