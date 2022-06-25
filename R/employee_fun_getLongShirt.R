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
#'      \item{benefits}{numeric value defining the cost charged for that month}
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

    price <- 1400

    longShirt  <- getCM(theObject)

    if (theObject@status == "age")
      longShirt$benefits <- 0

    if (theObject@status == "reg")
      longShirt$benefits <- round(longShirt$allow * price / 12, digits = 2)

    if (theObject@status == "pro") {
      longShirt$benefits <- round(
        longShirt$allow * price / sum(longShirt$allow), digits = 2
      )
    }

    if (theObject@status == "sea") {
      longShirt$benefits <- 0
      longShirt$benefits[min(which(longShirt$allow > 0))] <- price
    }

    return(longShirt[, c("month", "ID", "benefits")])
  }
)
