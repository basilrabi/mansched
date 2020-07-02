#include "isreg.h"
#include "normemp.h"
#include "mhdb.h"

Rcpp::StringVector mhTypeOTB = { "regOT" };;
Rcpp::StringVector mhTypeOTC = { "regOT",
                                 "rd", "rdOT",
                                 "sh", "shOT",
                                 "lh", "lhOT",
                                 "nh", "nhOT",
                                 "rs", "rsOT",
                                 "rl", "rlOT",
                                 "rn", "rnOT"};

void normEmployee ( Rcpp::S4 emp )
{
  Rcpp::StringVector costCenter = Rcpp::as<Rcpp::StringVector>( emp.slot( "costCenter" ) );
  costCenter[0] = "0-0";
}

void normNonStaff ( Rcpp::S4 emp, Rcpp::DataFrame mhDB )
{
  normEmployee( emp );
  Rcpp::IntegerVector regOT = Rcpp::as<Rcpp::IntegerVector>( emp.slot( "regOT" ) );
  Rcpp::IntegerVector mh = Rcpp::as<Rcpp::IntegerVector>( mhDB["mh"] );
  Rcpp::IntegerVector mhAssigned ( 12 );
  for ( int i = 0; i < 12; i++ )
  {
    mh[i] = regOT[i];
    regOT[i] = 0;
  }
}

void normOperationPersonnel ( Rcpp::S4 emp, Rcpp::DataFrame mhDB )
{
  normNonStaff( emp, mhDB );
  Rcpp::IntegerVector mh = Rcpp::as<Rcpp::IntegerVector>( mhDB["mh"] );

  Rcpp::IntegerVector lh = emp.slot( "lh" );
  Rcpp::IntegerVector nh = emp.slot( "nh" );
  Rcpp::IntegerVector sh = emp.slot( "sh" );
  Rcpp::IntegerVector rd = emp.slot( "rd" );
  Rcpp::IntegerVector rl = emp.slot( "rl" );
  Rcpp::IntegerVector rn = emp.slot( "rn" );
  Rcpp::IntegerVector rs = emp.slot( "rs" );
  Rcpp::IntegerVector lhOT = emp.slot( "lhOT" );
  Rcpp::IntegerVector nhOT = emp.slot( "nhOT" );
  Rcpp::IntegerVector shOT = emp.slot( "shOT" );
  Rcpp::IntegerVector rdOT = emp.slot( "rdOT" );
  Rcpp::IntegerVector rlOT = emp.slot( "rlOT" );
  Rcpp::IntegerVector rnOT = emp.slot( "rnOT" );
  Rcpp::IntegerVector rsOT = emp.slot( "rsOT" );

  for ( int i = 0; i < 12; i++ )
  {
    mh[ 12 + i] = rd  [i];
    mh[ 24 + i] = rdOT[i];
    mh[ 36 + i] = sh  [i];
    mh[ 48 + i] = shOT[i];
    mh[ 60 + i] = lh  [i];
    mh[ 72 + i] = lhOT[i];
    mh[ 96 + i] = nhOT[i];
    mh[108 + i] = rs  [i];
    mh[120 + i] = rsOT[i];
    mh[132 + i] = rl  [i];
    mh[144 + i] = rlOT[i];
    mh[156 + i] = rn  [i];
    mh[168 + i] = rnOT[i];

    rd  [i] = 0;
    rdOT[i] = 0;
    sh  [i] = 0;
    shOT[i] = 0;
    lh  [i] = 0;
    lhOT[i] = 0;
    nhOT[i] = 0;
    rs  [i] = 0;
    rsOT[i] = 0;
    rl  [i] = 0;
    rlOT[i] = 0;
    rn  [i] = 0;
    rnOT[i] = 0;

    if ( isReg( emp ) )
    {
      mh[84 + i] = nh[i];
      nh[i] = 0;
    }
  }
}


Rcpp::DataFrame normEmp ( Rcpp::S4 emp )
{
  Rcpp::String empClass = Rcpp::as<Rcpp::String>( emp.attr( "class" ) );
  Rcpp::String id = Rcpp::as<Rcpp::String>( emp.slot( "ID" ) );
  Rcpp::String cc = NA_STRING;

  if ( empClass == "Operator" )
  {
    Rcpp::StringVector equip = Rcpp::as<Rcpp::StringVector>( emp.slot( "equipment" ) );
    equip = equip[0];
    Rcpp::DataFrame mhdbEmp = mhdbInitEmployee( mhTypeOTC, id, id, cc );
    normOperationPersonnel( emp, mhdbEmp );
    return mhdbEmp;
  }
  if ( empClass == "Supervisor" ||
       empClass == "Laborer" ||
       empClass == "Technical" )
  {
    Rcpp::DataFrame mhdbEmp = mhdbInitEmployee( mhTypeOTC, id, id, cc );
    normOperationPersonnel( emp, mhdbEmp );
    return mhdbEmp;
  }
  else if ( empClass == "Clerk" )
  {
    Rcpp::DataFrame mhdbEmp = mhdbInitEmployee( mhTypeOTB, id, id, cc );
    normNonStaff( emp, mhdbEmp );
    return mhdbEmp;
  }
  else if ( empClass == "DivisionManager" ||
            empClass == "GroupManager" ||
            empClass == "DepartmentManager" ||
            empClass == "SectionHead" )
  {
    Rcpp::DataFrame mhdbEmp = mhdbBlank( emp );
    return mhdbEmp;
  }
  else
  {
    Rcpp::stop( "Unknown employee class." );
  }

}
