#' @import methods
NULL

#' Compute labor day t-shirt cost
#'
#' During labor day (May 1), all regular employees are given a shirt worth
#'   390 Php. This cost is charged in May.
#'
#' @param theObject \code{\link{Employee-class}} object
#' @return a \code{\link{data.frame}} with 12 rows and 3 columns although
#'   only the 5th row may have a value since the cost is charged only in May
#'
#'   Each row represents a month. The columns are:
#'   \describe{
#'      \item{ID}{character string representing the unique identifier of the
#'        real employee}
#'      \item{month}{integer value representing the month}
#'      \item{benefits}{numeric value defining the cost charged for that month}
#'   }
#' @export getLaborDayShirt
setGeneric(
  name = "getLaborDayShirt",
  def  = function(theObject) {
    standardGeneric("getLaborDayShirt")
  }
)

#' @describeIn getLaborDayShirt Summarize shirt cost
setMethod(
  f          = "getLaborDayShirt",
  signature  = "Employee",
  definition = function(theObject) {

    shirt <- getCM(theObject)
    shirt$shirt <- 0

    if (!theObject@forecast & isReg(theObject))
      shirt$shirt[shirt$month == 5L] <- 390

    shirt$benefits <- round(shirt$allow * shirt$shirt, digits = 2)

    return(shirt[, c("month", "ID", "benefits")])
  }
)
