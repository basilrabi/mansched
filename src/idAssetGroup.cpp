#include <Rcpp.h>
#include <boost/regex.hpp>
#include <algorithm>

using namespace Rcpp;

// String to upper
//
// Converts the string to uppercase
//
// @param x a character string
// @return a character string without lower case
std::string str_to_upper(std::string x) {

  std::string data = x;
  std::transform(data.begin(), data.end(), data.begin(), ::toupper);

  return data;
}

String concat(IntegerVector x) {
  String z;

  CharacterVector y = as<CharacterVector>(x);
  z = collapse(y);

  return z;
}

// Get letter count
//
// Gets the frequency per letter of a string
//
// @param x a character string
// @return a string of at least 26 length having only digits as characters
String getLetterCount(std::string x) {

  unsigned int i;
  std::string tempChar;

  std::transform(x.begin(), x.end(), x.begin(), ::toupper);

  // 26-element vector representing the alphabet counts
  IntegerVector letterCounts {0,0,0,0,0,0,0,0,0,0,0,0,0,
                              0,0,0,0,0,0,0,0,0,0,0,0,0};

  letterCounts.names() = CharacterVector({
    "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
    "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y" ,"Z"});

  for (i = 0; i < x.length(); i++) {
    tempChar = x[i];

    if (boost::regex_match(tempChar, boost::regex("[[:upper:]]")))
      letterCounts[letterCounts.findName(tempChar)]++;
  }

  return concat(letterCounts);
}

// Is the string a subset?
//
// Test whether the string \code{x} is a subset of another string.
//
// @param x a character string to be tested
// @param y a string return by \code{\link{getLetterCount}}
//
//   \code{y} comes from a string wherein the string \code{x} might be a subset.
// @return logical value
bool isSubset(std::string x, std::string y) {

  int z;
  std::string tempStr;
  tempStr = getLetterCount(x);

  for (unsigned int i = 0; i < 26; i++) {

    z = (int) y[i] - (int) tempStr[i];

    if (z < 0) {
      return false;
    }
  }

  return true;
}

//' Identify asset group
//'
//' Identifies the asset group of a given equipment (or entity) which is the
//'   recipient of the item withdrawn from warehouse.
//'   This data can be seen from warehouse issuance slips.
//'
//'   Below are the identified groups:
//'   \itemize{
//'   \item Contractors
//'     \describe{
//'     \item{HPK}{}
//'     \item{NBC}{Nickel Base Corporation}
//'     \item{PACE}{PrinceAce Corporation}
//'     \item{SDMC}{Southernphil Development and Marketing Corporation
//'       (FITZ-SDMC)}
//'     \item{SMCC}{Sumitomo-Mitsui Construction Company}
//'     \item{TACOPHIL}{}
//'     \item{TRGC}{Tango Romeo General Construction}
//'     }
//'   \item Equipment
//'     \describe{
//'     \item{CT}{bulldozer}
//'     \item{DT}{dump truck}
//'     \item{FASTCRAFT}{}
//'     \item{FL}{fuel lorry}
//'     \item{FORKLIFT}{}
//'     \item{FS}{flying squid}
//'     \item{FT}{fire truck}
//'     \item{FTR}{farm tractors}
//'     \item{GS}{power generator set}
//'     \item{MC}{motorcycle}
//'     \item{ML}{man lift}
//'     \item{MOC}{mobile crusher}
//'     \item{MT}{maintenance truck}
//'     \item{MWL}{mini wheeled pay loader}
//'     \item{PATROLBOAT}{}
//'     \item{PB}{pump boat}
//'     \item{RG}{road grader}
//'     \item{SB}{service bus}
//'     \item{SP}{service pickup}
//'     \item{ST}{service truck}
//'     \item{TL}{tracked pay loader}
//'     \item{TWL}{tower light}
//'     \item{TX}{tracked excavator}
//'     \item{VC}{vibrating compactor}
//'     \item{WL}{wheeled pay loader}
//'     \item{WP}{water pump}
//'     \item{WTL}{water lorry}
//'     \item{WX}{wheeled excavator}
//'     \item{YBM}{Yoshida Boring Machine}
//'     }
//'   \item Fixed assets
//'     \describe{
//'     \item{ADMIN}{Admin building}
//'     \item{EQ}{Engineer's Quarters}
//'     \item{CIP}{construction in-progress}
//'     }
//'   \item Other parties
//'     \describe{
//'     \item{LGU}{local government unit}
//'     \item{MKTG}{marketing}
//'     \item{SECURITY}{}
//'     }
//'   \item Unknown
//'     \itemize{
//'     \item CM
//'     \item GT
//'     \item LM
//'     \item PMT
//'     \item WC
//'     \item ZZZ
//'     }
//'   }
//'
//' @param x a character vector representing the equipment
//' @return a character vector representing the asset group
//' @export
// [[Rcpp::export]]
StringVector idAssetGroup(StringVector x) {

  R_xlen_t n = x.length();
  StringVector equip(clone(x));
  String a, b;
  std::string testChar;

  // declare getLetterCount constants to avoid repeating in the loop
  String glc_MC        = getLetterCount("CM");
  String glc_CT        = getLetterCount("CT");
  String glc_DT1       = getLetterCount("DT");
  String glc_DT2       = getLetterCount("D");
  String glc_DT3       = getLetterCount("DTUP");
  String glc_DT4       = getLetterCount("DTPDT");
  String glc_DT5       = getLetterCount("DTT");
  String glc_EQ        = getLetterCount("EQ");
  String glc_FL        = getLetterCount("FL");
  String glc_FORKLIFT  = getLetterCount("FORKLIFT");
  String glc_FT        = getLetterCount("FT");
  String glc_FTR       = getLetterCount("FTR");
  String glc_GS        = getLetterCount("GS");
  String glc_GT        = getLetterCount("GT");
  String glc_LM        = getLetterCount("LM");
  String glc_MDT       = getLetterCount("MDT");
  String glc_MT        = getLetterCount("MT");
  String glc_MWL       = getLetterCount("MWL");
  String glc_PB        = getLetterCount("PB");
  String glc_PMT       = getLetterCount("PMT");
  String glc_RG        = getLetterCount("RG");
  String glc_SB        = getLetterCount("SB");
  String glc_SECURITY  = getLetterCount("SECURITY");
  String glc_SP        = getLetterCount("SP");
  String glc_ST        = getLetterCount("ST");
  String glc_TL        = getLetterCount("TL");
  String glc_TX1       = getLetterCount("TX");
  String glc_TX2       = getLetterCount("TZ");
  String glc_TX3       = getLetterCount("X");
  String glc_VC        = getLetterCount("VC");
  String glc_WC        = getLetterCount("WC");
  String glc_WL1       = getLetterCount("WL");
  String glc_WL2       = getLetterCount("W");
  String glc_WL3       = getLetterCount("WLRGCT");
  String glc_WP1       = getLetterCount("WP");
  String glc_WP2       = getLetterCount("WTLWP");
  String glc_WTL1      = getLetterCount("WT");
  String glc_WTL2      = getLetterCount("WTL");
  String glc_WX        = getLetterCount("WX");
  String glc_YBM       = getLetterCount("YBM");


  for (R_xlen_t i = 0; i < n; i++) {

    if (x[i] == NA_STRING)
      continue;

    a = x[i];
    b = x[i];

    a = str_to_upper(a);
    testChar = a.get_cstring();
    b = getLetterCount(b);

    if (boost::regex_match(testChar, boost::regex(".*TACOPHIL.*"))) {
      equip[i] = "TACOPHIL";
    } else if (boost::regex_match(testChar, boost::regex(".*PACE.*")) ||
      boost::regex_match(testChar, boost::regex(".*PRI?N?CE *ACE.*"))) {
      // PrinceAce Corporation
      equip[i] = "PACE";
    } else if (boost::regex_match(testChar, boost::regex(".*BRGY.*")) ||
      boost::regex_match(testChar, boost::regex(".*BGRY.*"))) {
      // Local Government Unit
      equip[i] = "LGU";
    } else if (boost::regex_match(testChar, boost::regex(".*SMCC.*"))) {
      // Sumitomo-Mitsui Construction Company
      equip[i] = "SMCC";
    } else if (boost::regex_match(testChar, boost::regex(".*ADMIN.*"))) {
      // Admin Building
      equip[i] = "ADMIN";
    } else if (boost::regex_match(testChar, boost::regex(".*CIK?P.*"))) {
      // Construction in progress
      equip[i] = "CIP";
    } else if (b == glc_MC) {
      if (boost::regex_match(testChar, boost::regex(".*MC.*"))) {
        // Motorcycle
        equip[i] = "MC";
      } else {
        // Not described in shop data
        equip[i] = "CM";
      }
    } else if (b == glc_CT) {
      // Bulldozer
      equip[i] = "CT";
    } else if (b == glc_DT1 ||
      b == glc_DT2 ||
      b == glc_DT3 ||
      b == glc_DT4 ||
      b == glc_DT5) {
      // Dump truck
      equip[i] = "DT";
    } else if (b == glc_EQ ||
      (boost::regex_match(testChar, boost::regex(".*EQ\\s*[[:digit:]].*")))) {
      // Engineer's quarters
      equip[i] = "EQ";
    } else if (isSubset("FASTCRAFT", b)) {
      equip[i] = "FASTCRAFT";
    } else if (b == glc_FL) {
      // Fuel lorry
      equip[i] = "FL";
    } else if (b == glc_FORKLIFT) {
      equip[i] = "FORKLIFT";
    } else if (boost::regex_match(testChar, boost::regex("^F\\s*S.*")) ||
      boost::regex_match(testChar, boost::regex(".*SQUID.*"))) {
      // Flying Squid
      equip[i] = "FS";
    } else if (b == glc_FT) {
      // Fire truck
      equip[i] = "FT";
    } else if (b == glc_FTR) {
      // Farm tractors
      equip[i] = "FTR";
    } else if (b == glc_GS ||
      boost::regex_match(testChar, boost::regex(".*GEN\\s*SET.*")) ||
      boost::regex_match(testChar, boost::regex(".*GENERATOR.*")) ||
      boost::regex_match(testChar, boost::regex("^GS\\s*\\d*.*"))) {
      // Power generator set
      equip[i] = "GS";
    } else if (b == glc_GT) {
      // Not described in shop data
      equip[i] = "GT";
    } else if (boost::regex_match(testChar, boost::regex(".*HPK.*"))) {
      equip[i] = "HPK";
    } else if (b == glc_LM) {
      if (boost::regex_match(testChar, boost::regex(".*L\\s*M.*"))) {
        // Not described in shop data
        equip[i] = "LM";
      } else {
        // Man lift
        equip[i] = "ML";
      }
    } else if (boost::regex_match(testChar, boost::regex(".*MANLIFT.*"))) {
      equip[i] = "ML";
    } else if (b == glc_MDT) {
      // Mini dump truck
      equip[i] = "MDT";
    } else if (boost::regex_match(testChar, boost::regex(".*MA?R?KE?TI?N?G.*")) ||
      boost::regex_match(testChar, boost::regex(".*MKGT.*"))) {
      // Supplies during shipment loading
      equip[i] = "MKTG";
    } else if (boost::regex_match(testChar, boost::regex(".*CRU?SHE?E?R.*"))) {
      // Mobile crusher
      equip[i] = "MOC";
    } else if (b == glc_MT) {
      // Maintenance truck
      equip[i] = "MT";
    } else if (b == glc_MWL) {
      // Mini wheel loader
      equip[i] = "MWL";
    } else if (boost::regex_match(testChar, boost::regex(".*NBC.*"))) {
      // Nickel Base Corporation
      equip[i] = "NBC";
    } else if (boost::regex_match(testChar, boost::regex(".*PATROL\\s*BOAT.*"))) {
      equip[i] = "PATROLBOAT";
    } else if (b == glc_PB ||
      boost::regex_match(testChar, boost::regex(".*PUMP\\s*BOAT.*"))) {
      equip[i] = "PB";
    } else if (b == glc_PMT) {
      // Not described in shop data
      equip[i] = "PMT";
    } else if (b == glc_RG) {
      // Road grader
      equip[i] = "RG";
    } else if (b == glc_SB) {
      // Service bus
      equip[i] = "SB";
    } else if (boost::regex_match(testChar, boost::regex(".*SD(M|N)\\s*C.*")) ||
      boost::regex_match(testChar, boost::regex(".*SMDC.*"))) {
      // Southernphil Development and Marketing Corporation (FITZ-SDMC)
      equip[i] = "SDMC";
    } else if (b == glc_SECURITY) {
      equip[i] = "SECURITY";
    } else if (b == glc_SP) {
      // Service pickup
      equip[i] = "SP";
    } else if (b == glc_ST) {
      // Service truck
      equip[i] = "ST";
    } else if (b == glc_TL) {
      // Tracked loader
      equip[i] = "TL";
    } else if (boost::regex_match(testChar, boost::regex(".*TRG?C.*"))) {
      // Tango Romeo ?General ?Construction
      equip[i] = "TRGC";
    } else if (b == glc_TX1 ||
      boost::regex_match(testChar, boost::regex("^TX\\s+.*")) ||
      b == glc_TX2 ||
      b == glc_TX3) {
      // Tracked excavator
      equip[i] = "TX";
    } else if (b == glc_VC) {
      // Vibrating compactor
      equip[i] = "VC";
    } else if (b == glc_WC) {
      // Not described in shop data
      equip[i] = "WC";
    } else if (b == glc_WL1 ||
      b == glc_WL2 ||
      b == glc_WL3) {
      // Wheeled pay loader
      equip[i] = "WL";
    } else if (b == glc_WP1 ||
      boost::regex_match(testChar, boost::regex(".*H2(0|O)\\s*PUMP.*")) ||
      b == glc_WP2 ||
      isSubset("WATER PUMP", b)) {
      equip[i] = "WP";
    } else if (b == glc_WTL2) {
      if (boost::regex_match(testChar, boost::regex(".*WTL.*")) ||
          boost::regex_match(testChar, boost::regex(".*WLT.*"))) {
        // Water truck
        equip[i] = "WTL";
      } else if (boost::regex_match(testChar, boost::regex(".*TWL.*"))) {
        // Tower light
        equip[i] = "TWL";
      } else {
        // Not yet classified
        equip[i] = "ZZZ";
      }
    } else if (b == glc_WTL1) {
      // Water truck
      equip[i] = "WTL";
    } else if (boost::regex_match(testChar, boost::regex(".*WX.*")) ||
      b == glc_WX) {
      // Wheeled excavator
      equip[i] = "WX";
    } else if (b == glc_YBM ||
      boost::regex_match(testChar, boost::regex(".*YBM.*"))) {
      // Yoshida boring machine
      equip[i] = "YBM";
    } else {
      // Not yet classified
      equip[i] = "ZZZ";
    }

  }

  return equip;
}
