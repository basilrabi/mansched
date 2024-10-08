#' @import methods
NULL

#' Compute the monthly food allowance of employee
#'
#' This is a subset of \code{link{getAllowance}} as requested by Reyna.
#'
#' @param theObject \code{\link{Employee-class}} object
#' @return a \code{\link{data.frame}} with 12 rows and 3 columns representing
#'   the total allowances the employee receives per month
#'
#'   Each row represents a month. The columns are:
#'   \describe{
#'      \item{ID}{character string representing the unique identifier of the
#'        real employee}
#'      \item{month}{integer value representing the month}
#'      \item{allowance}{numeric value defining the total allowances to be received
#'        by the employee}
#'   }
#' @export getFoodAllowance
setGeneric(
  name = "getFoodAllowance",
  def  = function(theObject) {
    standardGeneric("getFoodAllowance")
  }
)

#' @describeIn getFoodAllowance Compute allowance multiplier
setMethod(
  f          = "getFoodAllowance",
  signature  = "Employee",
  definition = function(theObject) {
    return(getCM(theObject))
  }
)

#' @describeIn getFoodAllowance Compute monthly food allowance
setMethod(
  f          = "getFoodAllowance",
  signature  = "Staff",
  definition = function(theObject) {

    allowance <- callNextMethod(theObject)
    cost      <- 0

    # Allowances
    food <- 2200
    lighting <- 0
    housing <- 0

    if (theObject@forecast) {
      lighting <- 0
      housing <- 0
    }

    if (!theObject@inHouse)
      cost <- cost + food + lighting

    cost <- cost + housing
    allowance$allowance <- round(allowance$allow * cost, digits = 2)

    return(allowance[, c("month", "ID", "allowance")])
  }
)

#' @describeIn getFoodAllowance Compute monthly food allowance
setMethod(
  f          = "getFoodAllowance",
  signature  = "NonStaff",
  definition = function(theObject) {

    allowance <- callNextMethod(theObject)
    cost      <- 0

    if (isRF(theObject)) {
      rf_housing <- 0
      rf_lighting <- 0
      if (theObject@forecast) {
        rf_housing <- 0
        rf_lighting <- 0
      }

      if (isReg(theObject = theObject)) {
        cost <- cost + rf_lighting
        cost <- cost + rf_housing
      }
    } else {

      food <- 2100
      lighting <- 0
      housing <- 0

      if (theObject@forecast) {
        lighting <- 0
        housing <- 0
      }

      cost <- cost + food

      if (isReg(theObject)) {
        cost <- cost + housing
        if (!theObject@inHouse)
          cost <- cost + lighting
      }
    }

    allowance$allowance <- round(allowance$allow * cost, digits = 2)

    return(allowance[, c("month", "ID", "allowance")])
  }
)
