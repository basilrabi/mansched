#' @import methods
NULL

#' Compute retention bonus
#'
#' Retention bonus to be given to all seasonal employees at the end of their
#'   employment contract
#'
#' @param theObject \code{\link{Employee-class}} object
#' @return a \code{\link{data.frame}} with 12 rows and 3 columns.
#'
#'   Each row represents a month. The columns are:
#'   \describe{
#'      \item{ID}{character string representing the unique identifier of the
#'        real employee}
#'      \item{month}{integer value representing the month}
#'      \item{retentionBonus}{numeric value defining the cost charged for that
#'        month}
#'   }
#' @importFrom lubridate month
#' @export getRetentionBonus
setGeneric(
  name = "getRetentionBonus",
  def  = function(theObject) {
    standardGeneric("getRetentionBonus")
  }
)

#' @describeIn getRetentionBonus Compute retention bonus
setMethod(
  f          = "getRetentionBonus",
  signature  = "Employee",
  definition = function(theObject) {

    rb <- getCM(theObject)
    rb$retentionBonus <- 0

    if (theObject@status %in% c("sea")) {

      monthStart <- as.integer(lubridate::month(as.Date(theObject@cBegin)))
      monthEnd <- as.integer(lubridate::month(as.Date(theObject@cEnd)))
      retentionFactor <- 1
      retentionBonus <- 20930

      duration <- monthEnd - monthStart
      if (duration < 6)
        retentionFactor <- (1 + duration) / 7

      retentionBonus <- (retentionBonus * retentionFactor) - seasonalSigningBonus
      if (retentionBonus < 0)
        retentionBonus <- 0

      rb$retentionBonus[rb$month == monthEnd] <- round(retentionBonus, digits = 2)
    }

    return(rb[, c("month", "ID", "retentionBonus")])
  }
)
