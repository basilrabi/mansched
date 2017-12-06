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
    } else if (b == getLetterCount("CM")) {
      if (boost::regex_match(testChar, boost::regex(".*MC.*"))) {
        // Motorcycle
        equip[i] = "MC";
      } else {
        // Not described in shop data
        equip[i] = "CM";
      }
    } else if (b == getLetterCount("CT")) {
      // Bulldozer
      equip[i] = "CT";
    } else if (b == getLetterCount("DT") ||
      b == getLetterCount("D") ||
      b == getLetterCount("DTUP") ||
      b == getLetterCount("DTPDT") ||
      b == getLetterCount("DTT")) {
      // Dump truck
      equip[i] = "DT";
    } else if (b == getLetterCount("EQ") ||
      (boost::regex_match(testChar, boost::regex(".*EQ\\s*[[:digit:]].*")))) {
      // Engineer's quarters
      equip[i] = "EQ";
    } else if (isSubset("FASTCRAFT", b)) {
      equip[i] = "FASTCRAFT";
    } else if (b == getLetterCount("FL")) {
      // Fuel lorry
      equip[i] = "FL";
    } else if (b == getLetterCount("FORKLIFT")) {
      equip[i] = "FORKLIFT";
    } else if ((b == getLetterCount("FS") &&
      boost::regex_match(testChar, boost::regex(".*F\\s*S.*"))) ||
      boost::regex_match(testChar, boost::regex(".*SQUID.*"))) {
      // Flying Squid
      equip[i] = "FS";
    } else if (b == getLetterCount("FT")) {
      // Fire truck
      equip[i] = "FT";
    } else if (b == getLetterCount("FTR")) {
      // Farm tractors
      equip[i] = "FTR";
    } else if (b == getLetterCount("GS") ||
      boost::regex_match(testChar, boost::regex(".*GEN\\s*SET.*")) ||
      boost::regex_match(testChar, boost::regex(".*GENERATOR.*")) ||
      boost::regex_match(testChar, boost::regex("^GS\\s*\\d*.*"))) {
      // Power generator set
      equip[i] = "GS";
    } else if (b == getLetterCount("GT")) {
      // Not described in shop data
      equip[i] = "GT";
    } else if (boost::regex_match(testChar, boost::regex(".*HPK.*"))) {
      equip[i] = "HPK";
    } else if (b == getLetterCount("LM")) {
      if (boost::regex_match(testChar, boost::regex(".*L\\s*M.*"))) {
        // Not described in shop data
        equip[i] = "LM";
      } else {
        // Man lift
        equip[i] = "ML";
      }
    } else if (boost::regex_match(testChar, boost::regex(".*MANLIFT.*"))) {
      equip[i] = "ML";
    } else if (b == getLetterCount("MDT")) {
      // Mini dump truck
      equip[i] = "MDT";
    } else if (boost::regex_match(testChar, boost::regex(".*MA?R?KE?TI?N?G.*")) ||
      boost::regex_match(testChar, boost::regex(".*MKGT.*"))) {
      // Supplies during shipment loading
      equip[i] = "MKTG";
    } else if (boost::regex_match(testChar, boost::regex(".*CRU?SHE?E?R.*"))) {
      // Mobile crusher
      equip[i] = "MOC";
    } else if (b == getLetterCount("MT")) {
      // Maintenance truck
      equip[i] = "MT";
    } else if (b == getLetterCount("MWL")) {
      // Mini wheel loader
      equip[i] = "MWL";
    } else if (boost::regex_match(testChar, boost::regex(".*NBC.*"))) {
      // Nickel Base Corporation
      equip[i] = "NBC";
    } else if (boost::regex_match(testChar, boost::regex(".*PATROL\\s*BOAT.*"))) {
      equip[i] = "PATROLBOAT";
    } else if (b == getLetterCount("PB") ||
      boost::regex_match(testChar, boost::regex(".*PUMP\\s*BOAT.*"))) {
      equip[i] = "PB";
    } else if (b == getLetterCount("PMT")) {
      // Not described in shop data
      equip[i] = "PMT";
    } else if (b == getLetterCount("RG")) {
      // Road grader
      equip[i] = "RG";
    } else if (b == getLetterCount("SB")) {
      // Service bus
      equip[i] = "SB";
    } else if (boost::regex_match(testChar, boost::regex(".*SD(M|N)\\s*C.*")) ||
      boost::regex_match(testChar, boost::regex("SMDC"))) {
      // Southernphil Development and Marketing Corporation (FITZ-SDMC)
      equip[i] = "SDMC";
    } else if (b == getLetterCount("SECURITY")) {
      equip[i] = "SECURITY";
    } else if (b == getLetterCount("SP")) {
      // Service pickup
      equip[i] = "SP";
    } else if (b == getLetterCount("ST")) {
      // Service truck
      equip[i] = "ST";
    } else if (b == getLetterCount("TL")) {
      // Tracked loader
      equip[i] = "TL";
    } else if (boost::regex_match(testChar, boost::regex(".*TRG?C.*"))) {
      // Tango Romeo ?General ?Construction
      equip[i] = "TRGC";
    } else if (b == getLetterCount("TX") ||
      boost::regex_match(testChar, boost::regex("\\s*TX\\s*")) ||
      b == getLetterCount("TZ") ||
      b == getLetterCount("X")) {
      // Tracked excavator
      equip[i] = "TX";
    } else if (b == getLetterCount("VC")) {
      // Vibrating compactor
      equip[i] = "VC";
    } else if (b == getLetterCount("WC")) {
      // Not described in shop data
      equip[i] = "WC";
    } else if (b == getLetterCount("WL") ||
      b == getLetterCount("W") ||
      b == getLetterCount("WLRGCT")) {
      // Wheeled pay loader
      equip[i] = "WL";
    } else if (b == getLetterCount("WP") ||
      boost::regex_match(testChar, boost::regex(".*H2(0|O)\\s*PUMP.*")) ||
      b == getLetterCount("WTLWP") ||
      isSubset("WATER PUMP", b)) {
      equip[i] = "WP";
    } else if (b == getLetterCount("WTL")) {
      if (boost::regex_match(testChar, boost::regex(".*WTL.*"))) {
        // Water truck
        equip[i] = "WTL";
      } else if (boost::regex_match(testChar, boost::regex(".*TWL.*"))) {
        // Tower light
        equip[i] = "TWL";
      } else {
        // Not yet classified
        equip[i] = "ZZZ";
      }
    } else if (boost::regex_match(testChar, boost::regex(".*WX.*")) ||
      b == getLetterCount("WX")) {
      // Wheeled excavator
      equip[i] = "WX";
    } else if (b == getLetterCount("YBM") ||
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
