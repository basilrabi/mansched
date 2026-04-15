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
    allowance$allowance <- round(allowance$allow * 2200, digits = 2)
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
      cost <- 0
    } else {
      cost <- 2100
    }
    allowance$allowance <- round(allowance$allow * cost, digits = 2)
    return(allowance[, c("month", "ID", "allowance")])
  }
)
