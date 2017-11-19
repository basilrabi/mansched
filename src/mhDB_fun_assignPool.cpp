#include <Rcpp.h>
using namespace Rcpp;

#define asVI as<IntegerVector>
#define asDF as<DataFrame>

Environment pkg      = Environment::namespace_env("mansched");

Function getHours = pkg["getHours"];
Function getSum     ("sum");
Function getClass   ("class");

LogicalVector hasAviHours(List listR, S4 empT) {

  R_xlen_t vecLength = listR.length();

  LogicalVector z (vecLength);
  DataFrame     hoursR;
  DataFrame     hoursT = asDF(getHours(empT));
  IntegerVector tempCol (12);

  for (R_xlen_t i = 0; i < vecLength; i++) {

    hoursR = asDF(getHours(listR[i]));

    if (as<int>(getSum(hoursR)) < 1) {
      z[i] = false;
      continue;
    }

    hoursR["reg"  ] = asVI(hoursR["reg"  ]) + asVI(hoursR["rd"]);
    hoursR["sh"   ] = asVI(hoursR["sh"   ]) + asVI(hoursR["rd"]);
    hoursR["lh"   ] = asVI(hoursR["lh"   ]) + asVI(hoursR["rd"]);
    hoursR["nh"   ] = asVI(hoursR["nh"   ]) + asVI(hoursR["rd"]);
    hoursR["regOT"] = asVI(hoursR["regOT"]) + asVI(hoursR["rd"]);
    hoursR["shOT" ] = asVI(hoursR["shOT" ]) + asVI(hoursR["rd"]);
    hoursR["lhOT" ] = asVI(hoursR["lhOT" ]) + asVI(hoursR["rd"]);
    hoursR["nhOT" ] = asVI(hoursR["nhOT" ]) + asVI(hoursR["rd"]);

    for (unsigned int j = 0; j < 16; j++) {
      tempCol = asVI(hoursR[j]);
      for (unsigned int k = 0; k < 12; k++) {
        if (asVI(hoursT[j])[k] == 0)
          tempCol[k] = 0;
      }
      hoursR[j] = tempCol;
    }

    if (as<int>(getSum(hoursR)) > 0) {
      z[i] = true;
    } else {
      z[i] = false;
    }

  }

  return z;
}

//' Assign pool to requirement2 (WIP)
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
List assignPool2(DataFrame empReq              ,
                 DataFrame empPool             ,
                 List      listT               ,
                 List      listR               ,
                 String    prioStat = NA_STRING,
                 bool      prioCode = false     ) {

  String tempClass;
  String tempCostCode;
  String tempEquip;

  unsigned int dLength = empReq.nrows();

  CharacterVector ID       (1);
  IntegerVector   mh       (1);
  CharacterVector mhType   (1);
  IntegerVector   month    (1);
  IntegerVector   np       (1);
  CharacterVector costCode (1);
  CharacterVector sal      (1);
  CharacterVector scheme   (1);
  CharacterVector status   (1);
  IntegerVector   maxReg   (1);

  ID       [0] = NA_STRING ;
  mh       [0] = NA_INTEGER;
  mhType   [0] = NA_STRING ;
  month    [0] = NA_INTEGER;
  np       [0] = NA_INTEGER;
  costCode [0] = NA_STRING ;
  sal      [0] = NA_STRING ;
  scheme   [0] = NA_STRING ;
  status   [0] = NA_STRING ;
  maxReg   [0] = NA_INTEGER;

  DataFrame mhDB = DataFrame::create(Named("ID"      ) = clone(ID      ),
                                     Named("mh"      ) = clone(mh      ),
                                     Named("mhType"  ) = clone(mhType  ),
                                     Named("month"   ) = clone(month   ),
                                     Named("np"      ) = clone(np      ),
                                     Named("costCode") = clone(costCode),
                                     Named("sal"     ) = clone(sal     ),
                                     Named("scheme"  ) = clone(scheme  ),
                                     Named("status"  ) = clone(status  ),
                                     Named("maxReg"  ) = clone(maxReg  ));

  for (R_xlen_t i = 0; i < dLength; i++) {

    if (getSum(getHours(listT[i])) == 0)
      continue;

    if(listR.length() < 1) {
      listR = R_NilValue;
      break;
    }

    tempClass = as<String>(getClass(listT[i]));

    if (tempClass == "Operator") {
      tempEquip = as<String>(as<S4>(listT[i]).slot("equipment"));
    }  else {
      tempEquip = NA_STRING;
    }

    tempCostCode = as<String>(as<S4>(listT[i]).slot("costCode"));

    empPool["hasAviHours"] = hasAviHours(listR, as<S4>(listT[i]));



  }

  return List::create(empReq, empPool, listT, listR, mhDB);
}
