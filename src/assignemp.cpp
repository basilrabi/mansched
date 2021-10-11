#include "assignmh.h"
#include "assignemp.h"
#include "isreg.h"
#include "isrf.h"
#include "mhdb.h"
#include "template.h"

Rcpp::StringVector mhTypeA = { "reg" };
Rcpp::StringVector mhTypeB = { "reg", "regOT" };
Rcpp::StringVector mhTypeC = { "reg", "regOT",
                               "rd", "rdOT",
                               "sh", "shOT",
                               "lh", "lhOT",
                               "nh", "nhOT",
                               "rs", "rsOT",
                               "rl", "rlOT",
                               "rn", "rnOT"};

void assignEmployee ( Rcpp::S4 empT,
                      Rcpp::S4 empR,
                      Rcpp::DataFrame mhDB,
                      bool selfAssign )
{
  int i;
  Rcpp::IntegerVector mh = Rcpp::as<Rcpp::IntegerVector>( mhDB["mh"] );
  Rcpp::IntegerVector mhAssigned ( 12 );
  if ( selfAssign )
  {
    mhAssigned = assignSelfMH( empR.slot( "reg" ) );
  }
  else
  {
    mhAssigned = assignMH( empT.slot( "reg" ), empR.slot( "reg" ) );
  }
  for ( i = 0; i < 12; i++ )
    mh[i] = mhAssigned[i];
}

void assignNonStaff ( Rcpp::S4 empT,
                      Rcpp::S4 empR,
                      Rcpp::DataFrame mhDB,
                      bool selfAssign )
{
  assignEmployee( empT, empR, mhDB, selfAssign );
  Rcpp::IntegerVector mh = Rcpp::as<Rcpp::IntegerVector>( mhDB["mh"] );
  Rcpp::IntegerVector mhAssigned ( 12 );
  if ( selfAssign )
  {
    mhAssigned = assignSelfMH( empR.slot( "regOT" ) );
  }
  else
  {
    mhAssigned = assignMH( empT.slot( "regOT" ), empR.slot( "regOT" ) );
  }
  for ( int i = 0; i < 12; i++ )
    mh[i + 12] = mhAssigned[i];
}

void assignOperationPersonnel ( Rcpp::S4 empT,
                                Rcpp::S4 empR,
                                Rcpp::DataFrame mhDB,
                                bool selfAssign )
{
  assignNonStaff( empT, empR, mhDB, selfAssign );
  Rcpp::IntegerVector mh = Rcpp::as<Rcpp::IntegerVector>( mhDB["mh"] );

  Rcpp::IntegerVector lh ( 12 );
  Rcpp::IntegerVector nh ( 12 );
  Rcpp::IntegerVector sh ( 12 );
  Rcpp::IntegerVector rd ( 12 );
  Rcpp::IntegerVector rl ( 12 );
  Rcpp::IntegerVector rn ( 12 );
  Rcpp::IntegerVector rs ( 12 );

  Rcpp::IntegerVector lhOT ( 12 );
  Rcpp::IntegerVector nhOT ( 12 );
  Rcpp::IntegerVector shOT ( 12 );
  Rcpp::IntegerVector rdOT ( 12 );
  Rcpp::IntegerVector rlOT ( 12 );
  Rcpp::IntegerVector rnOT ( 12 );
  Rcpp::IntegerVector rsOT ( 12 );

  if ( selfAssign)
  {
    lh = assignSelfMH( empR.slot( "lh" ) );
    nh = assignSelfMH( empR.slot( "nh" ) );
    sh = assignSelfMH( empR.slot( "sh" ) );
    rd = assignSelfMH( empR.slot( "rd" ) );
    rl = assignSelfMH( empR.slot( "rl" ) );
    rn = assignSelfMH( empR.slot( "rn" ) );
    rs = assignSelfMH( empR.slot( "rs" ) );

    lhOT = assignSelfMH( empR.slot( "lhOT" ) );
    nhOT = assignSelfMH( empR.slot( "nhOT" ) );
    shOT = assignSelfMH( empR.slot( "shOT" ) );
    rdOT = assignSelfMH( empR.slot( "rdOT" ) );
    rlOT = assignSelfMH( empR.slot( "rlOT" ) );
    rnOT = assignSelfMH( empR.slot( "rnOT" ) );
    rsOT = assignSelfMH( empR.slot( "rsOT" ) );
  }
  else
  {
    lh = assignMH( empT.slot( "lh" ), empR.slot( "lh" ) );
    nh = assignMH( empT.slot( "nh" ), empR.slot( "nh" ) );
    sh = assignMH( empT.slot( "sh" ), empR.slot( "sh" ) );
    rd = assignMH( empT.slot( "reg" ), empR.slot( "rd" ) );
    rl = assignMH( empT.slot( "lh" ), empR.slot( "rl" ) );
    rn = assignMH( empT.slot( "nh" ), empR.slot( "rn" ) );
    rs = assignMH( empT.slot( "sh" ), empR.slot( "rs" ) );

    lhOT = assignMH( empT.slot( "lhOT" ), empR.slot( "lhOT" ) );
    nhOT = assignMH( empT.slot( "nhOT" ), empR.slot( "nhOT" ) );
    shOT = assignMH( empT.slot( "shOT" ), empR.slot( "shOT" ) );
    rdOT = assignMH( empT.slot( "regOT" ), empR.slot( "rdOT" ) );
    rlOT = assignMH( empT.slot( "lhOT" ), empR.slot( "rlOT" ) );
    rnOT = assignMH( empT.slot( "nhOT" ), empR.slot( "rnOT" ) );
    rsOT = assignMH( empT.slot( "shOT" ), empR.slot( "rsOT" ) );
  }

  // If a non-regular RF is assigned in a special holiday, add 8 hours per
  // special holiday assigned in holHours
  // TODO: Unit Test
  if ( !isReg( empR ) && isRF( empR ) )
  {
    if ( ( Rcpp::sum( sh ) + Rcpp::sum( rs ) ) > 0 )
    {
      Rcpp::IntegerVector shA = Rcpp::clone( sh );
      Rcpp::IntegerVector shB = Rcpp::clone( rs );
      Rcpp::LogicalVector shAi = shA == 0;
      Rcpp::LogicalVector shBi = shB == 0;
      shA = shA / 8;
      shB = shB / 8;
      shA = shA + 1;
      shB = shB + 1;
      shA = shA * 8;
      shB = shB * 8;
      shA[shAi] = 0;
      shB[shBi] = 0;
      Rcpp::IntegerVector holHours = Rcpp::as<Rcpp::IntegerVector>( empR.slot( "holHours" ) );
      holHours = holHours + shA + shB;
    }
  }
  for ( int i = 0; i < 12; i++ )
  {
    mh[ 24 + i] = rd  [i];
    mh[ 36 + i] = rdOT[i];
    mh[ 48 + i] = sh  [i];
    mh[ 60 + i] = shOT[i];
    mh[ 72 + i] = lh  [i];
    mh[ 84 + i] = lhOT[i];
    mh[ 96 + i] = nh  [i];
    mh[108 + i] = nhOT[i];
    mh[120 + i] = rs  [i];
    mh[132 + i] = rsOT[i];
    mh[144 + i] = rl  [i];
    mh[156 + i] = rlOT[i];
    mh[168 + i] = rn  [i];
    mh[180 + i] = rnOT[i];
  }
}

void assignProductionPersonnel ( Rcpp::S4 empT,
                                 Rcpp::S4 empR,
                                 Rcpp::DataFrame mhDB,
                                 bool selfAssign )
{
  assignOperationPersonnel( empT, empR, mhDB, selfAssign );
  Rcpp::NumericVector mh = Rcpp::clone( Rcpp::as<Rcpp::NumericVector>( mhDB["mh"] ) );
  Rcpp::NumericVector np = Rcpp::as<Rcpp::NumericVector>( mhDB["np"] );
  if ( isReg( empR ) )
  {
    np = Rcpp::ceil( ( mh / 2.0 ) + 0.5 );
  }
  else
  {
    np = Rcpp::ceil( ( mh / 3.0 ) + 0.5 );
  }
}


Rcpp::DataFrame assignEmp ( Rcpp::S4 empT,
                            Rcpp::S4 empR,
                            bool selfAssign = false,
                            bool debug = false )
{
  Rcpp::String empClass = Rcpp::as<Rcpp::String>( empT.attr( "class" ) );
  Rcpp::StringVector idT = Rcpp::as<Rcpp::StringVector>( empT.slot( "ID" ) );
  Rcpp::StringVector idR = Rcpp::as<Rcpp::StringVector>( empR.slot( "ID" ) );
  if ( empClass != Rcpp::as<Rcpp::String>( empR.attr( "class" ) ) )
  {
    Rcpp::Rcout << idT
                << " is "
                << Rcpp::as<Rcpp::StringVector>( empT.slot( "class" ) )
                << " while "
                << idR
                << " is "
                << Rcpp::as<Rcpp::StringVector>( empR.attr( "class" ) )
                << "!\n";
    Rcpp::stop( "empT and empR have different classes" );
  }

  if ( empClass == "Operator" )
  {
    Rcpp::StringVector equipT = Rcpp::as<Rcpp::StringVector>( empT.slot( "equipment" ) );
    Rcpp::StringVector equipR = Rcpp::as<Rcpp::StringVector>( empR.slot( "equipment" ) );

    if ( debug )
      Rcpp::Rcout << "Assigning an operator with equipment "
                  << equipT[0]
                  << ".\n";

    Rcpp::DataFrame mhdbEmp = mhdbInitEmployee( mhTypeC, empR.slot( "ID" ), empT.slot( "ID" ), empT.slot( "costCenter" ), empR.slot( "dcc" ), selfAssign, equipT[0] );
    if ( Rcpp::is_true( Rcpp::all( isElement( equipT, equipR ) ) )  )
    {
      assignProductionPersonnel( empT, empR, mhdbEmp, selfAssign );
    }
    else
    {
      Rcpp::Rcout << "Employee "
                  << idR
                  << " does not match any equipment required by "
                  << idT
                  << ".\n" ;
      Rcpp::stop( "Unauthorized equipemnt!" );
    }
    return mhdbFilter( mhdbEmp );
  }
  if ( empClass == "Supervisor" || empClass == "Laborer" )
  {
    Rcpp::DataFrame mhdbEmp = mhdbInitEmployee( mhTypeC, empR.slot( "ID" ), empT.slot( "ID" ), empT.slot( "costCenter" ), empR.slot( "dcc" ), selfAssign, NA_STRING );
    assignProductionPersonnel( empT, empR, mhdbEmp, selfAssign );
    return mhdbFilter( mhdbEmp );
  }
  if ( empClass == "Technical" )
  {
    Rcpp::DataFrame mhdbEmp = mhdbInitEmployee( mhTypeC, empR.slot( "ID" ), empT.slot( "ID" ), empT.slot( "costCenter" ), empR.slot( "dcc" ), selfAssign, NA_STRING );
    assignOperationPersonnel( empT, empR, mhdbEmp, selfAssign );
    return mhdbFilter( mhdbEmp );
  }
  else if ( empClass == "Clerk" )
  {
    Rcpp::DataFrame mhdbEmp = mhdbInitEmployee( mhTypeB, empR.slot( "ID" ), empT.slot( "ID" ), empT.slot( "costCenter" ), empR.slot( "dcc" ), selfAssign, NA_STRING );
    assignNonStaff( empT, empR, mhdbEmp, selfAssign );
    return mhdbFilter( mhdbEmp );
  }
  else if ( empClass == "DivisionManager" ||
            empClass == "GroupManager" ||
            empClass == "DepartmentManager" ||
            empClass == "SectionHead" )
  {
    Rcpp::DataFrame mhdbEmp = mhdbInitEmployee( mhTypeA, empR.slot( "ID" ), empT.slot( "ID" ), empT.slot( "costCenter" ), empR.slot( "dcc" ), selfAssign, NA_STRING );
    assignEmployee( empT, empR, mhdbEmp, selfAssign );
    return mhdbFilter( mhdbEmp );
  }
  else
  {
    Rcpp::stop( "Unknown employee class." );
  }

}
