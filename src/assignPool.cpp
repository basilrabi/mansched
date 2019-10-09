#include <Rcpp.h>

Rcpp::Environment mansched   = Rcpp::Environment::namespace_env( "mansched" );
Rcpp::Function    assignEmp2 = mansched["assignEmp2"];
Rcpp::Function    dfAppend   = mansched["dfAppend"];
Rcpp::Function    getHours   = mansched["getHours"];

template <class T>
T combine ( T a, T b ) {
  std::size_t n = a.size() + b.size();
  T output = Rcpp::no_init( n );
  std::size_t index = 0;
  std::copy( a.begin(), a.end(), output.begin() + index );
  index += a.size();
  std::copy( b.begin(), b.end(), output.begin() + index );
  return output;
}

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
  Rcpp::DataFrame empReqC  = Rcpp::clone( empReq  );
  Rcpp::DataFrame empPoolC = Rcpp::clone( empPool );
  Rcpp::List listTC        = Rcpp::clone( listT   );
  Rcpp::List listRC        = Rcpp::clone( listR   );

  Rcpp::StringVector  tempEquip   (0);
  Rcpp::LogicalVector hasManHours (0);
  Rcpp::IntegerVector maxReg        ( 10000, Rcpp::IntegerVector::get_na() );
  Rcpp::IntegerVector mh            ( 10000, Rcpp::IntegerVector::get_na() );
  Rcpp::IntegerVector month         ( 10000, Rcpp::IntegerVector::get_na() );
  Rcpp::IntegerVector np            ( 10000, Rcpp::IntegerVector::get_na() );
  Rcpp::StringVector  costCode      ( 10000, Rcpp::StringVector::get_na()  );
  Rcpp::StringVector  id            ( 10000, Rcpp::StringVector::get_na()  );
  Rcpp::StringVector  mhType        ( 10000, Rcpp::StringVector::get_na()  );
  Rcpp::StringVector  sal           ( 10000, Rcpp::StringVector::get_na()  );
  Rcpp::StringVector  scheme        ( 10000, Rcpp::StringVector::get_na()  );
  Rcpp::StringVector  status        ( 10000, Rcpp::StringVector::get_na()  );

  Rcpp::DataFrame mhDB = Rcpp::DataFrame::create(
    Rcpp::Named( "ID"               ) = id,
    Rcpp::Named( "mh"               ) = mh,
    Rcpp::Named( "mhType"           ) = mhType,
    Rcpp::Named( "month"            ) = month,
    Rcpp::Named( "np"               ) = np,
    Rcpp::Named( "costCode"         ) = costCode,
    Rcpp::Named( "sal"              ) = sal,
    Rcpp::Named( "scheme"           ) = scheme,
    Rcpp::Named( "status"           ) = status,
    Rcpp::Named( "maxReg"           ) = maxReg,
    Rcpp::Named( "stringsAsFactors" ) = false
  );

  for ( i = 0; i < empReqC.nrows(); i++ )
  {
    empHours = availableHours( Rcpp::as<Rcpp::S4>( listTC[i] ) );
    if ( empHours == 0 )
      continue;

    Rcpp::Rcout << "Assigning personnel for "
                << Rcpp::as<Rcpp::StringVector>( Rcpp::as<Rcpp::S4>( listTC[i] ).slot( "ID" ) )
                << ".\n"
                << "Iteration: "
                << i
                << "\n";

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

    Rcpp::Rcout << "Identifying which personnel will be assigned from "
                << listRC.length()
                << " personnel.\n";
    Rcpp::LogicalVector hasAviHours   ( listRC.length(), FALSE );
    Rcpp::LogicalVector matchClass    ( listRC.length(), FALSE );
    Rcpp::LogicalVector matchEquip    ( listRC.length(), FALSE );
    Rcpp::LogicalVector matchCostCode ( listRC.length(), TRUE  );
    Rcpp::LogicalVector choice        ( listRC.length(), FALSE );
    for ( j = 0; j < listRC.length(); j++ )
    {
      Rcpp::Rcout << "Identifying "
                  << j + 1
                  << " out of "
                  << listRC.length()
                  << ".\n";
      Rcpp::Rcout << "Checking available man hours...\n";
      if ( assignable( listTC[i] , listRC[j] ) )
        hasAviHours[j] = TRUE;

      Rcpp::Rcout << "Checking matching employee class...\n";
      if ( Rcpp::any( Rcpp::is_na( prioStat ) ) )
        matchClass[j] = TRUE;
      else
      {
        if ( Rcpp::intersect( Rcpp::as<Rcpp::StringVector>( Rcpp::as<Rcpp::S4>( listRC[j] ).slot( "status" ) ), prioStat ).length() > 0 )
          matchClass[j] = TRUE;
      }

      Rcpp::Rcout << "Checking matching equipment...\n";
      if ( Rcpp::is_na( tempEquip )[0] )
        matchEquip[j] = TRUE;
      else if ( Rcpp::as<Rcpp::S4>( listRC[j] ).hasSlot( "equipment" ) &&
                Rcpp::intersect( tempEquip, Rcpp::as<Rcpp::StringVector>( Rcpp::as<Rcpp::S4>( listRC[j] ).slot( "equipment" ) ) ).length() > 0 )
        matchEquip[j] = TRUE;

      Rcpp::Rcout << "Checking matching cost centers...\n";
      if ( prioCode &&
           Rcpp::intersect( tempCostCode, Rcpp::as<Rcpp::StringVector>( Rcpp::as<Rcpp::S4>( listRC[j] ).slot( "costCode" ) ) ).length() < 1 )
        matchCostCode[j] = FALSE;

      choice[j] = hasAviHours[j] &&
                  matchClass[j] &&
                  matchEquip[j] &&
                  matchCostCode[j];
    }
    Rcpp::Rcout << "Done identifying.\n";
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
        // Store number of equipment authorized for each personel
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
                      << availableHours( Rcpp::as<Rcpp::S4>( listTC[i] ) )
                      << "\nMH Pool: "
                      << availableHours( listRC[*jj] )
                      << "\n";
          Rcpp::List tempData = assignEmp2( Rcpp::as<Rcpp::S4>( listTC[i] ), Rcpp::as<Rcpp::S4>( listRC[*jj] ) );
          listTC[i] = Rcpp::as<Rcpp::S4>( tempData[1] );
          Rcpp::Rcout << Rcpp::as<Rcpp::StringVector>( Rcpp::as<Rcpp::S4>( tempData[2] ).slot( "ID" ) );
          listRC[*jj] = Rcpp::as<Rcpp::S4>( tempData[2] );

          if ( Rcpp::as<Rcpp::StringVector>( Rcpp::as<Rcpp::DataFrame>( tempData[0] )[0] ).length() > 0 )
          {
            Rcpp::Rcout << " assigned.\nTotal Assigned: "
                        << Rcpp::sum( Rcpp::as<Rcpp::IntegerVector>( Rcpp::as<Rcpp::DataFrame>( tempData[0] )[1] ) )
                        << "\nRemaining man-hours\nMH Req: "
                        << availableHours( Rcpp::as<Rcpp::S4>( listTC[i] ) )
                        << "\nMH Pool: "
                        << availableHours( listRC[*jj] )
                        << "\n";
            mhDB = dfAppend( mhDB, tempData[0] );
          }
          if ( availableHours( Rcpp::as<Rcpp::S4>( listTC[i] ) ) < 1 )
            break;
        }
      }

      hasManHours = Rcpp::LogicalVector ( listRC.length(), FALSE );
      for ( j = 0; j < listRC.length(); j++ )
      {
        if ( availableHours( listRC[j] ) > 0 )
          hasManHours[j] = TRUE;
        else
          Rcpp::Rcout << Rcpp::as<Rcpp::StringVector>( Rcpp::as<Rcpp::S4>( listRC[j] ).slot( "ID" ) )
                      << " is fully spent.\n";
      }
      // Retain personnel pool with available man hours
      if ( which( hasManHours ).length() < listRC.length() )
      {
        Rcpp::Rcout << "Removing spent employees...\n";
        listRC = listRC[hasManHours];
        Rcpp::StringVector  poolID          = Rcpp::as<Rcpp::StringVector >( empPoolC["ID"            ] )[hasManHours];
        Rcpp::StringVector  poolName        = Rcpp::as<Rcpp::StringVector >( empPoolC["name"          ] )[hasManHours];
        Rcpp::StringVector  poolDesignation = Rcpp::as<Rcpp::StringVector >( empPoolC["designation"   ] )[hasManHours];
        Rcpp::StringVector  poolClass       = Rcpp::as<Rcpp::StringVector >( empPoolC["personnelClass"] )[hasManHours];
        Rcpp::LogicalVector poolField       = Rcpp::as<Rcpp::LogicalVector>( empPoolC["field"         ] )[hasManHours];
        Rcpp::NumericVector poolAttendance  = Rcpp::as<Rcpp::NumericVector>( empPoolC["attendance"    ] )[hasManHours];
        Rcpp::StringVector  poolEquipment   = Rcpp::as<Rcpp::StringVector >( empPoolC["equipment"     ] )[hasManHours];
        Rcpp::StringVector  poolCostCode    = Rcpp::as<Rcpp::StringVector >( empPoolC["costCode"      ] )[hasManHours];
        Rcpp::StringVector  poolStatus      = Rcpp::as<Rcpp::StringVector >( empPoolC["status"        ] )[hasManHours];
        Rcpp::StringVector  poolCBegin      = Rcpp::as<Rcpp::StringVector >( empPoolC["cBegin"        ] )[hasManHours];
        Rcpp::StringVector  poolCEnd        = Rcpp::as<Rcpp::StringVector >( empPoolC["cEnd"          ] )[hasManHours];
        Rcpp::LogicalVector poolInHouse     = Rcpp::as<Rcpp::LogicalVector>( empPoolC["inHouse"       ] )[hasManHours];
        Rcpp::StringVector  poolRestDay     = Rcpp::as<Rcpp::StringVector >( empPoolC["restday"       ] )[hasManHours];
        Rcpp::LogicalVector poolIsRF        = Rcpp::as<Rcpp::LogicalVector>( empPoolC["isRF"          ] )[hasManHours];
        Rcpp::IntegerVector poolJan         = Rcpp::as<Rcpp::IntegerVector>( empPoolC["JAN"           ] )[hasManHours];
        Rcpp::IntegerVector poolFeb         = Rcpp::as<Rcpp::IntegerVector>( empPoolC["FEB"           ] )[hasManHours];
        Rcpp::IntegerVector poolMar         = Rcpp::as<Rcpp::IntegerVector>( empPoolC["MAR"           ] )[hasManHours];
        Rcpp::IntegerVector poolApr         = Rcpp::as<Rcpp::IntegerVector>( empPoolC["APR"           ] )[hasManHours];
        Rcpp::IntegerVector poolMay         = Rcpp::as<Rcpp::IntegerVector>( empPoolC["MAY"           ] )[hasManHours];
        Rcpp::IntegerVector poolJun         = Rcpp::as<Rcpp::IntegerVector>( empPoolC["JUN"           ] )[hasManHours];
        Rcpp::IntegerVector poolJul         = Rcpp::as<Rcpp::IntegerVector>( empPoolC["JUL"           ] )[hasManHours];
        Rcpp::IntegerVector poolAug         = Rcpp::as<Rcpp::IntegerVector>( empPoolC["AUG"           ] )[hasManHours];
        Rcpp::IntegerVector poolSep         = Rcpp::as<Rcpp::IntegerVector>( empPoolC["SEP"           ] )[hasManHours];
        Rcpp::IntegerVector poolOct         = Rcpp::as<Rcpp::IntegerVector>( empPoolC["OCT"           ] )[hasManHours];
        Rcpp::IntegerVector poolNov         = Rcpp::as<Rcpp::IntegerVector>( empPoolC["NOV"           ] )[hasManHours];
        Rcpp::IntegerVector poolDec         = Rcpp::as<Rcpp::IntegerVector>( empPoolC["DEC"           ] )[hasManHours];
        Rcpp::IntegerVector poolDRD         = Rcpp::as<Rcpp::IntegerVector>( empPoolC["d.rd"          ] )[hasManHours];
        Rcpp::IntegerVector poolDHO         = Rcpp::as<Rcpp::IntegerVector>( empPoolC["d.ho"          ] )[hasManHours];
        Rcpp::IntegerVector poolDRH         = Rcpp::as<Rcpp::IntegerVector>( empPoolC["d.rh"          ] )[hasManHours];
        Rcpp::StringVector  poolDCC         = Rcpp::as<Rcpp::StringVector >( empPoolC["dcc"           ] )[hasManHours];

        empPoolC = Rcpp::DataFrame::create(
          Rcpp::Named( "ID"               ) = poolID,
          Rcpp::Named( "name"             ) = poolName,
          Rcpp::Named( "designation"      ) = poolDesignation,
          Rcpp::Named( "personnelClass"   ) = poolClass,
          Rcpp::Named( "field"            ) = poolField,
          Rcpp::Named( "attendance"       ) = poolAttendance,
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
        empPoolC.push_back( poolJan, "JAN"  );
        empPoolC.push_back( poolFeb, "FEB"  );
        empPoolC.push_back( poolMar, "MAR"  );
        empPoolC.push_back( poolApr, "APR"  );
        empPoolC.push_back( poolMay, "MAY"  );
        empPoolC.push_back( poolJun, "JUN"  );
        empPoolC.push_back( poolJul, "JUL"  );
        empPoolC.push_back( poolAug, "AUG"  );
        empPoolC.push_back( poolSep, "SEP"  );
        empPoolC.push_back( poolOct, "OCT"  );
        empPoolC.push_back( poolNov, "NOV"  );
        empPoolC.push_back( poolDec, "DEC"  );
        empPoolC.push_back( poolDRD, "d.rd" );
        empPoolC.push_back( poolDHO, "d.ho" );
        empPoolC.push_back( poolDRH, "d.rh" );
        empPoolC.push_back( poolDCC, "dcc"  );
        empPoolC.attr( "class" ) = "data.frame";
        empPoolC.attr( "row.names" ) = Rcpp::IntegerVector::create( NA_INTEGER, XLENGTH( poolID ) );
        Rcpp::Rcout << "Employee pool successfully compressed.\n";
      }
    }
  }

  hasManHours = Rcpp::LogicalVector ( listTC.length(), FALSE );
  for ( i = 0; i < listTC.length(); i++ )
  {
    if ( availableHours( listTC[i] ) > 0 )
    {
      hasManHours[i] = TRUE;
    }
    else
    {
      Rcpp::Rcout << Rcpp::as<Rcpp::StringVector>( Rcpp::as<Rcpp::S4>( listTC[i] ).slot( "ID" ) )
                  << " is fully spent.\n";
    }
  }
  // Retain personnel pool with available man hours
  if ( which( hasManHours ).length() < listTC.length() )
  {
    listTC = listTC[hasManHours];
    Rcpp::StringVector reqID       = Rcpp::as<Rcpp::StringVector  >( empReqC["ID"             ] )[hasManHours];
    Rcpp::StringVector reqCostCode = Rcpp::as<Rcpp::StringVector  >( empReqC["costCode"       ] )[hasManHours];
    Rcpp::StringVector reqClass    = Rcpp::as<Rcpp::StringVector  >( empReqC["personnelClass" ] )[hasManHours];
    empReqC = Rcpp::DataFrame::create(
      Rcpp::Named( "ID"               ) = reqID,
      Rcpp::Named( "costCode"         ) = reqCostCode,
      Rcpp::Named( "personnelClass"   ) = reqClass,
      Rcpp::Named( "stringsAsFactors" ) = false
    );
  }

  costCode = mhDB["costCode"];
  id       = mhDB["ID"      ];
  maxReg   = mhDB["maxReg"  ];
  mh       = mhDB["mh"      ];
  mhType   = mhDB["mhType"  ];
  month    = mhDB["month"   ];
  np       = mhDB["np"      ];
  sal      = mhDB["sal"     ];
  scheme   = mhDB["scheme"  ];
  status   = mhDB["status"  ];

  // Remove NA values at the bottom
  Rcpp::LogicalVector withValues = !Rcpp::is_na( mh );
  costCode = costCode[withValues];
  id       = id      [withValues];
  maxReg   = maxReg  [withValues];
  mh       = mh      [withValues];
  mhType   = mhType  [withValues];
  month    = month   [withValues];
  np       = np      [withValues];
  sal      = sal     [withValues];
  scheme   = scheme  [withValues];
  status   = status  [withValues];

  mhDB = Rcpp::DataFrame::create(
    Rcpp::Named( "ID"               ) = id,
    Rcpp::Named( "mh"               ) = mh,
    Rcpp::Named( "mhType"           ) = mhType,
    Rcpp::Named( "month"            ) = month,
    Rcpp::Named( "np"               ) = np,
    Rcpp::Named( "costCode"         ) = costCode,
    Rcpp::Named( "sal"              ) = sal,
    Rcpp::Named( "scheme"           ) = scheme,
    Rcpp::Named( "status"           ) = status,
    Rcpp::Named( "maxReg"           ) = maxReg,
    Rcpp::Named( "stringsAsFactors" ) = false
  );

  Rcpp::List out = Rcpp::List::create( empReqC, empPoolC, listTC, listRC, mhDB );
  Rcpp::Rcout << "\n\n***********************"
              << "**** End asignPool ****"
              << "***********************\n\n";
  return out;
}
