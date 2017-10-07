#' @import methods
NULL

# theObject <- listR[[1]]

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

    if(!theObject@inHouse)
      cost <- cost + 1900 + 1100 # for food and lighting

    cost <- cost + 1100 # for housing

    allowance$allowance <- round(allowance$allow * cost, digits = 2)

    return(allowance[, colnames(allowance) %in% c("month", "ID", "allowance")])
  }
)

#' @describeIn getAllowance Compute monthly allowance
setMethod(
  f          = "getAllowance",
  signature  = "Non Staff",
  definition = function(theObject) {

    allowance <- callNextMethod(theObject)
    cost      <- 0

    if (isRF(theObject = theObject)) {

      if (isReg(theObject = theObject)) {

        cost <- cost + 700 # for lighting
        cost <- cost + 800 # for housing

      }
    } else {

      cost <- cost + 1800 # for food

      if (isReg(theObject = theObject)) {

        cost <- cost + 1100 # for housing

        if (!theObject@inHouse) {
          cost <- cost + 1100 # for lighting
        }

      }

    }

    allowance$allowance <- round(allowance$allow * cost, digits = 2)

    return(allowance[, colnames(allowance) %in% c("month", "ID", "allowance")])
  }
)
