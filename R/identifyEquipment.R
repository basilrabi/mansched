#' Get letter count
#'
#' Gets the frequency per letter of a string
#'
#' @param x a character string
#' @return a string of at least 26 length having only digits as characters
getLetterCount <- function(x) {

  x <- toupper(x)

  letterCounts       <- NULL
  letterCounts[1:26] <- 0L

  for (n in 1:nchar(x)) {

    m <- which(LETTERS == substr(x, n, n))

    letterCounts[m] <- letterCounts[m] + 1L
  }

  letterCounts <- as.character(letterCounts)

  return(paste(letterCounts, collapse = ""))
}

#' Is the string a subset?
#'
#' Test whether the string \code{x} is a subset of another string.
#'
#' @param x a character string to be tested
#' @param y a string return by \code{\link{getLetterCount}}
#'
#'   \code{y} comes from a string wherein the string \code{x} might be a subset.
#' @return logical value
isSubset <- function(x, y) {

  x <- getLetterCount(x)
  x <- as.integer(strsplit(x, "")[[1]])
  y <- as.integer(strsplit(y, "")[[1]])
  z <- y - x

  if (any(z < 0))
    return(FALSE)
  else
    return(TRUE)
}

# Function to identify the equipment group
#   x = string with punctuations and double spaces removed
# Results to be validated for Lawson Data

#' Identify equipment group
#'
#' Identifies fleet type of a given equipment.
#'
#' @param x a character string representing an equipment
#' @return a character string representing an equipment group
#' @export identifyEquipment
#' @importFrom stringr str_detect str_locate
identifyEquipment <- function(x) {

  if (is.na(x))
    return(NA)

  x <- toupper(x)
  y <- getLetterCount(x)

  if (str_detect(x, pattern = "TACOPHIL")) {
    # TACOPHIL
    return("TACOPHIL")
  } else if (str_detect(x, pattern = "PACE ") |
             str_detect(x, pattern = "PRI?N?CE ?ACE")) {
    # PrinceAce Corporation
    return("PACE")
  } else if (str_detect(x, pattern = "BRGY") |
             str_detect(x, pattern = "BGRY")) {
    # Local government unit
    return("LGU")
  } else if (str_detect(x, pattern = "SMCC")) {
    # Sumitomo-Mitsui Construction Company
    return("SMCC")
  } else if (y == getLetterCount("ADMIN")) {
    # Admin building
    return("ADMIN")
  } else if (str_detect(x, pattern = "CIK?P ")) {
    # Construction in progress
    return("CIP")
  } else if (y == getLetterCount("CM")) {
    if (str_detect(x, pattern = "MC")) {
      # Motorcycle
      return("MC")
    } else {
      return("CM")
      # Not described in Shop data
    }
  } else if (y == getLetterCount("CT")) {
    # Dozer
    return("CT")
  } else if (y == getLetterCount("DT") |
             y == getLetterCount("D") |
             y == getLetterCount("DTUP") |
             y == getLetterCount("DTPDT") |
             y == getLetterCount("DTT")) {
    # Dump truck
    return("DT")
  } else if (y == getLetterCount("EQ") |
             str_detect(x, pattern = "EQ [[:digit:]]")) {
    # Engineers' quarters
    return("EQ")
  } else if (isSubset("FASTCRAFT", y)) {
    # Fast Craft
    return("FASTCRAFT")
  } else if (y == getLetterCount("FL")) {
    # Fuel lorry
    return("FL")
  } else if (y == getLetterCount("FORKLIFT")) {
    # Forklift
    return("FORKLIFT")
  } else if ((str_detect(x, pattern = "F ?S ?") &
              str_locate(x, pattern = "F ?S ?")[1] == 1L) |
             str_detect(x, pattern = "SQUID")) {
    # Flying Squid
    return("FS")
  } else if (y == getLetterCount("FT")) {
    # Fire Truck
    return("FT")
  } else if (y == getLetterCount("FTR")) {
    # Farm Tracktors
    return("FTR")
  } else if (y == getLetterCount("GS") |
             str_detect(x, pattern = "GEN ?SET") |
             str_detect(x, pattern = "GS [[:digit:]]")|
             str_detect(x, pattern = "GENERATOR")) {
    # Generator set
    return("GS")
  } else if (y == getLetterCount("GT")) {
    # Unkown and not described in Shop data
    return("GT")
  } else if (str_detect(x, pattern = "HPK")) {
    # HPK
    return("HPK")
  } else if (y == getLetterCount("LM")) {
    if (str_detect(x, pattern = "LM")) {
      return("LM")
      # Unkown and not described in Shop data
    } else {
      return("ML")
      # Man lift
    }
  } else if (str_detect(x, pattern = "MANLIFT")) {
    # Man lift
    return("ML")
  } else if (y == getLetterCount("MDT")) {
    # Mini dump truck
    return("MDT")
  } else if (str_detect(x, pattern = "MKTG") |
             str_detect(x, pattern = "MKGT") |
             str_detect(x, pattern = "MARKETING")) {
    # Supplies during shipment loading
    return("MKTG")
  } else if (str_detect(x, pattern = "CRU?SHE?E?R")) {
    # Mobile Crusher
    return("MOC")
  } else if (y == getLetterCount("MT")) {
    # Maintenance Truck
    return("MT")
  } else if (y == getLetterCount("MWL")) {
    # Mini Wheel Loader
    return("MWL")
  } else if (str_detect(x, pattern = "NBC")) {
    # Nickel Base Corporation
    return("NBC")
  } else if (str_detect(x, pattern = "PATROL ?BOAT")) {
    # Patrol Boat
    return("PATROLBOAT")
  } else if (y == getLetterCount("PB") |
             str_detect(x, pattern = "PUMP ?BOAT")) {
    # Pump Boat
    return("PB")
  } else if (y == getLetterCount("PMT")) {
    # Unknown and not described in Shop data
    return("PMT")
  } else if (y == getLetterCount("RG")) {
    # Road grader
    return("RG")
  } else if (y == getLetterCount("SB")) {
    # Service bus
    return("SB")
  } else if (str_detect(x, pattern = "SDM ?C") |
             str_detect(x, pattern = "SDNC") |
             str_detect(x, pattern = "SMDC")) {
    # FTIZ SDMC
    return("SDMC")
  } else if (y == getLetterCount("SECURITY")) {
    # Security
    return("SECURITY")
  } else if (y == getLetterCount("SP")) {
    # Service pickup
    return("SP")
  } else if (y == getLetterCount("ST")) {
    # Service truck
    return("ST")
  } else if (y == getLetterCount("TL")) {
    # Tracked Loader
    return("TL")
  } else if (str_detect(x, pattern = "TRG?C")) {
    # Tango Romeo
    return("TRGC")
  } else if (y == getLetterCount("TX")        |
             str_detect(x, pattern = " ?TX ") |
             y == getLetterCount("TZ")        |
             y == getLetterCount("X")) {
    # Tracked excavator
    return("TX")
  } else if (y == getLetterCount("VC")) {
    # Vibrating compactor
    #   In Warehouse data, there is Vibrator Compactor but no fleet number.
    #   This was not included.
    return("VC")
  } else if (y == getLetterCount("WC")) {
    # Unknown and not described in Shop data
    return("WC")
  } else if (y == getLetterCount("WL") |
             y == getLetterCount("W")  |
             y == getLetterCount("WLRGCT")) {
    # Wheel loader
    return("WL")
  } else if (y == getLetterCount("WP")           |
             str_detect(x, pattern = "H2O PUMP") |
             str_detect(x, pattern = "H20 PUMP") |
             y == getLetterCount("WTLWP")        |
             isSubset("WATER PUMP", y)) {
    # Water pump
    return("WP")
  } else if (y == getLetterCount("WTL")) {
    if (str_detect(x, pattern = "WTL")) {
      # Water truck
      return("WTL")
    } else if (str_detect(x, pattern = "TWL")) {
      # Tower light
      return("TWL")
    } else {
      return("ZZZ")
    }
  } else if (str_detect(x, pattern = "WX") | y == getLetterCount("WX")) {
    # Wheeled excavator
    return("WX")
  } else if (y == getLetterCount("YBM") |
             str_detect(x, pattern = "YBM ")) {
    # Yoshida Boring Machine
    return("YBM")
  } else {
    return("ZZZ")
  }
}
