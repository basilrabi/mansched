#' @import methods
NULL

#' Compute the monthly cost for group life insurance premium
#'
#' Life insurance privilege is only available to regular and probationary
#'   employees. The higher the job grade of the employee, the higher the
#'   privilege enjoyed.
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
#'      \item{gl}{numeric value defining the total costs budgeted to the
#'        employee per month}
#'   }
#' @export getGroupLife
setGeneric(
  name = "getGroupLife",
  def  = function(theObject) {
    standardGeneric("getGroupLife")
  }
)

#' @describeIn getGroupLife Compute cost multiplier
setMethod(
  f          = "getGroupLife",
  signature  = "Employee",
  definition = function(theObject) {

    gl <- getCM(theObject)

    if (!theObject@status %in% c("reg", "pro"))
      gl$allow <- 0

    return(gl)
  }
)

#' @describeIn getGroupLife Compute premium list
setMethod(
  f          = "getGroupLife",
  signature  = "Staff",
  definition = function(theObject) {

    gl <- callNextMethod(theObject)
    groupLife <-  8131.2 / 12
    if (theObject@forecast)
      groupLife <-  6400 / 12

    gl$gl <- round(gl$allow * groupLife, digits = 2)

    return(gl[, c("month", "ID", "gl")])
  }
)

#' @describeIn getGroupLife Compute monthly cost
setMethod(
  f          = "getGroupLife",
  signature  = "NonStaff",
  definition = function(theObject) {

    gl <- callNextMethod(theObject)

    if (isRF(theObject)) {
      groupLife <-  2439.36 / 12
      if (theObject@forecast)
        groupLife <- 1920 / 12

    } else {
      groupLife <-  4878.72 / 12
      if (theObject@forecast)
        groupLife <-  3840 / 12
    }

    gl$gl <- round(gl$allow * groupLife, digits = 2)

    return(gl[, c("month", "ID", "gl")])
  }
)
