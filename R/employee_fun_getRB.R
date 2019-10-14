#' @import methods
NULL

#' Compute retention bonus
#'
#' Retention bonus to be given to all seasonal employees.
#'
#' @param theObject \code{\link{Employee-class}} object
#' @return a \code{\link{data.frame}} with 12 rows and 3 columns although
#'   only either the 4th or 10th row may have a value since the cost is charged
#'   only in April and in October.
#'
#'   Each row represents a month. The columns are:
#'   \describe{
#'      \item{ID}{character string representing the unique identifier of the
#'        real employee}
#'      \item{month}{integer value representing the month}
#'      \item{rb}{numeric value defining the cost charged for that month}
#'   }
#' @export getRB
setGeneric(
  name = "getRB",
  def  = function(theObject) {
    standardGeneric("getRB")
  }
)

#' @describeIn getRB Summarize gift certificate cost
setMethod(
  f          = "getRB",
  signature  = "Employee",
  definition = function(theObject) {

    rb <- getCM(theObject)
    rb$rb <- 0

    # TODO: include to forecast in 2020
    if (theObject@status %in% c("sea") & !theObject@forecast) {
      rb$rb[rb$month ==  4L] <-  4186
      rb$rb[rb$month == 10L] <- 16744
    }
    rb$benefits <- round(rb$allow * rb$rb, digits = 2)

    return(rb[, c("month", "ID", "benefits")])
  }
)
