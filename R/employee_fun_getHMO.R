#' @import methods
NULL

#' Compute the monthly cost for health and maintenance organization
#'
#' HMO privilege is only available to regular employees. The higher the job
#'   grade of the employee, the higher the privilege per dependent.
#'
#' @param theObject \code{\link{Employee-class}} object
#' @return a \code{\link{data.frame}} with 12 rows and 3 columns representing
#'   the total HMO costs budgeted for the employee per month
#'
#'   Each row represents a month. The columns are:
#'   \describe{
#'      \item{ID}{character string representing the unique identifier of the
#'        real employee}
#'      \item{month}{integer value representing the month}
#'      \item{hmo}{numeric value defining the total costs budgeted to the
#'        employee per month}
#'   }
#' @export getHMO
setGeneric(
  name = "getHMO",
  def  = function(theObject) {
    standardGeneric("getHMO")
  }
)

#' @describeIn getHMO Compute cost multiplier
setMethod(
  f          = "getHMO",
  signature  = "Employee",
  definition = function(theObject) {

    hmo <- getCM(theObject)

    if (theObject@status != "reg")
      hmo$allow <- 0

    return(hmo)
  }
)

#' @describeIn getHMO Compute premium list
setMethod(
  f          = "getHMO",
  signature  = "Staff",
  definition = function(theObject) {

    hmo <- callNextMethod(theObject)

    principal <- 19363.3
    dependent <- principal
    if (theObject@forecast) {
      principal <- 17603
      dependent <- principal
    }


    employeeHMO <- principal / 12
    dependentHMO <- dependent * theObject@dependents / 12

    cost <- employeeHMO + dependentHMO
    hmo$hmo <- round(hmo$allow * cost, digits = 2)

    return(hmo[, c("month", "ID", "hmo")])
  }
)

#' @describeIn getHMO Compute monthly cost
setMethod(
  f          = "getHMO",
  signature  = "NonStaff",
  definition = function(theObject) {

    hmo <- callNextMethod(theObject)

    if (isRF(theObject)) {
      principal <- 13773.1
      dependent <- principal
      if (theObject@forecast) {
        principal <- 12521
        dependent <- principal
      }
    } else {
      principal <- 14550.8
      dependent <- principal
      if (theObject@forecast) {
        principal <- 13228
        dependent <- principal
      }
    }
    employeeHMO <- principal / 12
    dependentHMO <- dependent / 12

    cost <- employeeHMO + (dependentHMO * theObject@dependents)
    hmo$hmo <- round(hmo$allow * cost, digits = 2)

    return(hmo[, c("month", "ID", "hmo")])
  }
)
