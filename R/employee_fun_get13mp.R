#' @import methods
NULL

#' Compute monthly accrued 13th month pay
#'
#' 13th month pay is pro-rated to the number of months the employee is employed
#'   for the year. This is then distributed equally to all months of employment.
#'
#' @param theObject \code{\link{Employee-class}} object
#' @param sal a numeric vector of length 2
#'
#'   This contains 2 monthly salaries of the employee. The first one is salary
#'   before annual increase while the second one is the adjusted salary.
#' @return a \code{\link{data.frame}} with 12 rows and 3 columns representing
#'   the accrued 13th month pay per month
#'
#'   Each row represents a month. The columns are:
#'   \describe{
#'      \item{ID}{character string representing the unique identifier of the
#'        real employee}
#'      \item{month}{integer value representing the month}
#'      \item{mp}{numeric value defining the accrued 13th month pay}
#'   }
#' @export get13mp
setGeneric(
  name = "get13mp",
  def  = function(theObject, sal) {
    standardGeneric("get13mp")
  }
)

#' @describeIn get13mp Compute 13 month pay
setMethod(
  f          = "get13mp",
  signature  = "Employee",
  definition = function(theObject, sal) {

    sched <- getCM(theObject)

    if (theObject@status != "reg") {
      cost <- sal[1]
    } else {
      cost <- sal[2]
    }

    if (isRF(theObject)) {
      # If the employee is RF, multiply daily wage by 26 days
      cost <- cost * 26
    }

    sched$cost <- cost / 12
    sched$mp   <- round(sched$cost * sched$allow, digits = 2)

    return(sched[, c("month", "ID", "mp")])
  }
)
