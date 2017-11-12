#include <Rcpp.h>
using namespace Rcpp;

//' Assign pool to requirement2
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

  return List::create(empReq, empPool, listT, listR, mhDB);
}
