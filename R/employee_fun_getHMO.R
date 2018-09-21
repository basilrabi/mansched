#' @import methods
NULL

#' Compute the monthly cost for health and maintenance organization
#'
#' HMO privilage is only available to regular employees. The higher the job
#'   grade of the employee, the higher the privilage per dependent.
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

    if (theObject@status != "reg" | theObject@forecast)
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

    employeeHMO <- 15676.25 / 12
    dependentHMO <- 15676.25 * theObject@dependents / 12
    groupLife <-  6490 / 12

    return(list(hmo, employeeHMO, dependentHMO, groupLife))
  }
)

#' @describeIn getHMO Compute monthly cost
setMethod(
  f          = "getHMO",
  signature  = "DivisionManager",
  definition = function(theObject) {

    hmoList <- callNextMethod(theObject)

    hmo <- hmoList[[1]]
    employeeHMO <- hmoList[[2]]
    dependentHMO <- hmoList[[3]]
    groupLife  <- 12980 / 12

    cost <- employeeHMO + dependentHMO + groupLife
    hmo$hmo <- round(hmo$allow * cost, digits = 2)

    return(hmo[, c("month", "ID", "hmo")])
  }
)

#' @describeIn getHMO Compute monthly cost
setMethod(
  f          = "getHMO",
  signature  = "GroupManager",
  definition = function(theObject) {

    hmoList <- callNextMethod(theObject)

    hmo <- hmoList[[1]]
    employeeHMO <- hmoList[[2]]
    dependentHMO <- hmoList[[3]]
    groupLife  <- 12980 / 12

    cost <- employeeHMO + dependentHMO + groupLife
    hmo$hmo <- round(hmo$allow * cost, digits = 2)

    return(hmo[, c("month", "ID", "hmo")])
  }
)

#' @describeIn getHMO Compute monthly cost
setMethod(
  f          = "getHMO",
  signature  = "DepartmentManager",
  definition = function(theObject) {

    hmoList <- callNextMethod(theObject)

    hmo <- hmoList[[1]]
    employeeHMO <- hmoList[[2]]
    dependentHMO <- hmoList[[3]]
    groupLife  <- 12980 / 12

    cost <- employeeHMO + dependentHMO + groupLife
    hmo$hmo <- round(hmo$allow * cost, digits = 2)

    return(hmo[, c("month", "ID", "hmo")])
  }
)

#' @describeIn getHMO Compute monthly cost
setMethod(
  f          = "getHMO",
  signature  = "SectionHead",
  definition = function(theObject) {

    hmoList <- callNextMethod(theObject)

    hmo <- hmoList[[1]]
    employeeHMO <- hmoList[[2]]
    dependentHMO <- hmoList[[3]]
    groupLife  <- hmoList[[4]]

    cost <- employeeHMO + dependentHMO + groupLife
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
      employeeHMO <- 11150 / 12
      dependentHMO <- 11150 / 12
      groupLife <-  1947 / 12
    } else {
      employeeHMO <- 11778.75 / 12
      dependentHMO <- 11778.75 / 12
      groupLife <-  3894 / 12
    }

    cost <- employeeHMO + (dependentHMO * theObject@dependents) + groupLife
    hmo$hmo <- round(hmo$allow * cost, digits = 2)

    return(hmo[, c("month", "ID", "hmo")])
  }
)
