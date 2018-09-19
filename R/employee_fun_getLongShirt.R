#' @import methods
NULL

#' Compute cost for long shirts issued
#'
#' Taganito Mine issues working long shirts to all in-house employees.
#'
#' @param theObject \code{\link{Employee-class}} object
#' @return a \code{\link{data.frame}} with 12 rows and 3 columns representing
#'   the distributed cost of working long shirts per month
#'
#'   Each row represents a month. The columns are:
#'   \describe{
#'      \item{ID}{character string representing the unique identifier of the
#'        real employee}
#'      \item{month}{integer value representing the month}
#'      \item{ls}{numeric value defining the cost charged for that month}
#'   }
#' @export getLongShirt
setGeneric(
  name = "getLongShirt",
  def  = function(theObject) {
    standardGeneric("getLongShirt")
  }
)

#' @describeIn getLongShirt Compute distributed cost for working long shirts
#'   throughout the year
setMethod(
  f          = "getLongShirt",
  signature  = "Employee",
  definition = function(theObject) {

    price <- 1170
    longShirt  <- getCM(theObject)

    if (theObject@forecast | theObject@status == "age")
      price <- 0

    longShirt$ls <- round(longShirt$allow * price / 12, digits = 2)

    return(longShirt[, c("month", "ID", "ls")])
  }
)
