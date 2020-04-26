#' @import methods
NULL

#' Compute RF Signing Bonus
#'
#' Signing bonus is given to Rank-and-File employees at the year of the start
#'   of a new collective bargaining agreement.
#'
#' @param theObject \code{\link{Employee-class}} object
#' @return a \code{\link{data.frame}} with 12 rows and 3 columns although
#'   only one row may have a value since the cost is charged only in one month.
#'
#'   Each row represents a month. The columns are:
#'   \describe{
#'      \item{ID}{character string representing the unique identifier of the
#'        real employee}
#'      \item{month}{integer value representing the month}
#'      \item{signingBonus}{numeric value defining the cost charged for that
#'        month}
#'   }
#' @export getSigningBonus
setGeneric(
  name = "getSigningBonus",
  def  = function(theObject) {
    standardGeneric("getSigningBonus")
  }
)

#' @describeIn getSigningBonus Compute cost multiplier
setMethod(
  f          = "getSigningBonus",
  signature  = "Employee",
  definition = function(theObject) {

    signingBonus <- getCM(theObject)

    if (theObject@status != "reg")
      signingBonus$allow <- 0

    return(signingBonus)
  }
)

#' @describeIn getSigningBonus No signing bonus for Staff
setMethod(
  f          = "getSigningBonus",
  signature  = "Staff",
  definition = function(theObject) {

    signingBonus <- callNextMethod(theObject)
    signingBonus$signingBonus <- 0

    return(signingBonus[, c("month", "ID", "signingBonus")])
  }
)

#' @describeIn getSigningBonus Compute monthly cost
setMethod(
  f          = "getSigningBonus",
  signature  = "NonStaff",
  definition = function(theObject) {

    signingBonus <- callNextMethod(theObject)
    signingBonus$bonus <- 0

    if (isRF(theObject))
      signingBonus$bonus[signingBonus$month == 5L] <- 13000

    signingBonus$signingBonus <- round(signingBonus$allow * signingBonus$bonus,
                                       digits = 2)

    return(signingBonus[, c("month", "ID", "signingBonus")])
  }
)

#' Compute Seasonal Employee Signing Bonus
#'
#' Signing bonus to be given to all seasonal employees at the start of
#'   employment.
#'
#' @param theObject \code{\link{Employee-class}} object
#' @return a \code{\link{data.frame}} with 12 rows and 3 columns.
#'
#'   Each row represents a month. The columns are:
#'   \describe{
#'      \item{ID}{character string representing the unique identifier of the
#'        real employee}
#'      \item{month}{integer value representing the month}
#'      \item{signingBonus}{numeric value defining the cost charged for that
#'        month}
#'   }
#' @export getSigningBonusSea
setGeneric(
  name = "getSigningBonusSea",
  def  = function(theObject) {
    standardGeneric("getSigningBonusSea")
  }
)

#' @describeIn getSigningBonusSea Compute seasonal employee's signing bonus
setMethod(
  f          = "getSigningBonusSea",
  signature  = "Employee",
  definition = function(theObject) {

    signingBonus <- getCM(theObject)
    signingBonus$signingBonus <- 0

    if (theObject@status %in% c("sea")) {
      monthStart <- as.integer(lubridate::month(as.Date(theObject@cBegin)))
      signingBonus$signingBonus[signingBonus$month == monthStart] <-
        seasonalSigningBonus
    }

    return(signingBonus[, c("month", "ID", "signingBonus")])
  }
)
