#' @import methods
NULL

#' Compute the monthly allowance of employee
#'
#' Depending on the attributes of the employee, his or her monthly allowances
#'   may include the following:
#'   \enumerate{
#'     \item food
#'     \item lighting
#'     \item housing
#'   }
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
#' @export getAllowance
setGeneric(
  name = "getAllowance",
  def  = function(theObject) {
    standardGeneric("getAllowance")
  }
)

#' @describeIn getAllowance Compute allowance multiplier
setMethod(
  f          = "getAllowance",
  signature  = "Employee",
  definition = function(theObject) {
    return(getCM(theObject))
  }
)

#' @describeIn getAllowance Compute monthly allowance
setMethod(
  f          = "getAllowance",
  signature  = "Staff",
  definition = function(theObject) {

    allowance <- callNextMethod(theObject)
    cost      <- 0

    # Allowances
    food <- 2100
    lighting <- 1150
    housing <- 1150

    if (!theObject@inHouse)
      cost <- cost + food + lighting

    cost <- cost + housing
    allowance$allowance <- round(allowance$allow * cost, digits = 2)

    return(allowance[, c("month", "ID", "allowance")])
  }
)

#' @describeIn getAllowance Compute monthly allowance
setMethod(
  f          = "getAllowance",
  signature  = "NonStaff",
  definition = function(theObject) {

    allowance <- callNextMethod(theObject)
    cost      <- 0

    if (isRF(theObject)) {

      if (isReg(theObject = theObject)) {
        cost <- cost + 750 # for lighting
        cost <- cost + 850 # for housing
      }
    } else {

      foodForTechSup <- 2000

      # Food allowance in 2018
      if (theObject@forecast)
        foodForTechSup <- 2000

      cost <- cost + foodForTechSup

      if (isReg(theObject = theObject)) {

        cost <- cost + 1150 # for housing

        if (!theObject@inHouse) {
          cost <- cost + 1150 # for lighting
        }
      }
    }

    allowance$allowance <- round(allowance$allow * cost, digits = 2)

    return(allowance[, c("month", "ID", "allowance")])
  }
)
