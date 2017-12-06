#include <Rcpp.h>
using namespace Rcpp;

#define asCV as<CharacterVector>
#define asDF as<DataFrame>
#define asIV as<IntegerVector>
#define asLV as<LogicalVector>
#define asST as<std::string>

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

    hoursR["reg"  ] = asIV(hoursR["reg"  ]) + asIV(hoursR["rd"]);
    hoursR["sh"   ] = asIV(hoursR["sh"   ]) + asIV(hoursR["rd"]);
    hoursR["lh"   ] = asIV(hoursR["lh"   ]) + asIV(hoursR["rd"]);
    hoursR["nh"   ] = asIV(hoursR["nh"   ]) + asIV(hoursR["rd"]);
    hoursR["regOT"] = asIV(hoursR["regOT"]) + asIV(hoursR["rd"]);
    hoursR["shOT" ] = asIV(hoursR["shOT" ]) + asIV(hoursR["rd"]);
    hoursR["lhOT" ] = asIV(hoursR["lhOT" ]) + asIV(hoursR["rd"]);
    hoursR["nhOT" ] = asIV(hoursR["nhOT" ]) + asIV(hoursR["rd"]);

    for (unsigned int j = 0; j < 16; j++) {
      tempCol = asIV(hoursR[j]);
      for (unsigned int k = 0; k < 12; k++) {
        if (asIV(hoursT[j])[k] == 0)
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

LogicalVector matchClass(List listR, CharacterVector prioStat) {

  R_xlen_t      vecLength  = listR.length();
  unsigned int  testLength = prioStat.length();
  LogicalVector z            (vecLength);

  if (prioStat[0] == NA_STRING) {
    z = rep(LogicalVector(true), vecLength);
  } else {
    for (R_xlen_t i = 0; i < vecLength; i++) {
      for (unsigned int j = 0; j < testLength; j++) {
        if (asST(prioStat[j]) == asST(as<S4>(listR[i]).slot("status"))) {
          z[i] = true;
          break;
        }
      }
    }
  }

  return z;
}

LogicalVector matchEquip(std::string tempClass,
                         std::string equipment,
                         List        listR     ) {

  R_xlen_t        vecLength   = listR.length();
  CharacterVector empEquip    = NA_STRING;
  LogicalVector   z             (vecLength);

  if (tempClass != "Operator") {
    z = rep(LogicalVector(true), vecLength);
  } else {
    for (R_xlen_t i = 0; i < vecLength; i++) {
      if (asST(getClass(as<S4>(listR[i]))) == "Operator") {
        empEquip = asCV(as<S4>(listR[i]).slot("equipment"));
        for (unsigned int j = 0; j < empEquip.length(); j++) {
          if (equipment == asST(empEquip[j])) {
            z[i] = true;
            break;
          }
        }
      }
    }
  }

  return z;
}

LogicalVector matchCostCode(std::string tempCostCode,
                            List        listR,
                            bool        prioCode) {

  R_xlen_t        vecLength   = listR.length();
  CharacterVector empCostCode = NA_STRING;
  LogicalVector   z             (vecLength);

  if (!prioCode) {
    z = rep(LogicalVector(true), vecLength);
  } else {
    for (R_xlen_t i = 0; i < vecLength; i++) {
      empCostCode = asCV(as<S4>(listR[i]).slot("costCode"));
      for (unsigned int j = 0; j < empCostCode.length(); j++) {
        if (tempCostCode == asST(empCostCode[j])) {
          z[i] = true;
          break;
        }
      }
    }
  }

  return z;
}

LogicalVector getChoice(LogicalVector Ca,
                        LogicalVector Cb,
                        LogicalVector Cc,
                        LogicalVector Cd) {

  R_xlen_t      pLength = Ca.length();
  LogicalVector z         (pLength);
  LogicalVector tempZ     (4);

  for (R_xlen_t i = 0; i < pLength; i++) {

    tempZ[0] = Ca[i];
    tempZ[1] = Cb[i];
    tempZ[2] = Cc[i];
    tempZ[3] = Cd[i];

    z[i] = is_true(all(tempZ));
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
List assignPool2(DataFrame       empReq              ,
                 DataFrame       empPool             ,
                 List            listT               ,
                 List            listR               ,
                 CharacterVector prioStat = NA_STRING,
                 bool            prioCode = false     ) {

  String tempClass;
  String tempCostCode;
  String tempEquip;

  R_xlen_t rLength = empReq.nrows();
  // R_xlen_t pLength = empReq.nrows();

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

  for (R_xlen_t i = 0; i < rLength; i++) {

    if (i % 50 == 0){
      checkUserInterrupt();
    }

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

    empPool["hasAviHours"  ] = hasAviHours(listR, as<S4>(listT[i]));
    empPool["matchClass"   ] = matchClass(listR, prioStat);
    empPool["matchEquip"   ] = matchEquip(tempClass, tempEquip, listR);
    empPool["matchCostCode"] = matchCostCode(tempCostCode, listR, prioCode);

    empPool["choice"] = getChoice(empPool["hasAviHours"],
                            empPool["matchClass"],
                                   empPool["matchEquip"],
                                          empPool["matchCostCode"]);



  }

  return List::create(empReq, empPool, listT, listR, mhDB);
}
