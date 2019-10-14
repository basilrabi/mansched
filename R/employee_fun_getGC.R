#' @import methods
NULL

#' Compute gift certificate cost
#'
#' Gift certificates amounting to 1500 PhP are usually given to employee on
#'   December for non-seasonal employees. For seasonal employees, the gift
#'   certificates are awarded in October.
#'
#' @param theObject \code{\link{Employee-class}} object
#' @return a \code{\link{data.frame}} with 12 rows and 3 columns although
#'   only either the 10th or 12th row may have a value since the cost is charged
#'   only in October (for seasonal employees) and in December (for regular and
#'   probationary employees).
#'
#'   Each row represents a month. The columns are:
#'   \describe{
#'      \item{ID}{character string representing the unique identifier of the
#'        real employee}
#'      \item{month}{integer value representing the month}
#'      \item{benefits}{numeric value defining the cost charged for that month}
#'   }
#' @export getGC
setGeneric(
  name = "getGC",
  def  = function(theObject) {
    standardGeneric("getGC")
  }
)

#' @describeIn getGC Summarize gift certificate cost
setMethod(
  f          = "getGC",
  signature  = "Employee",
  definition = function(theObject) {

    gc <- getCM(theObject)
    gc$gc <- 0
    gc$benefits <- gc$gc

    if (theObject@status %in% c("reg")) {
      gc$gc[gc$month == 12L] <- 1500
      gc$benefits <- round(gc$allow * gc$gc, digits = 2)
    }

    if (theObject@status %in% c("sea", "pro")) {

      if (sum(gc$allow) > 10) {
        multiplier <- 10
      } else {
        multiplier <- sum(gc$allow)
      }
      gc$gc[gc$month == 11L] <- (3000 / 10) * multiplier
      gc$benefits <- gc$gc
    }

    return(gc[, c("month", "ID", "benefits")])
  }
)
