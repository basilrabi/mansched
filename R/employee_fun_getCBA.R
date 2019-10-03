#' @import methods
NULL

#' Compute cost for CBA provisions
#'
#' Taganito Mine provides items to all regular employees as agreed upon with
#'   the labor union.
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
#' @export getCBA
setGeneric(
  name = "getCBA",
  def  = function(theObject) {
    standardGeneric("getCBA")
  }
)

#' @describeIn getCBA Compute pro-rated cost for CBA provisions
#'   throughout the year
setMethod(
  f          = "getCBA",
  signature  = "Employee",
  definition = function(theObject) {

    uniform <- 1750
    maong <- 800
    packBag <- 1000
    waterJug <- 400
    if (theObject@forecast) {
      uniform <- 1600
      maong <- 700
      packBag <- 980
      waterJug <- 380
    }
    price <- 2 * uniform + maong + packBag + waterJug
    cba <- getCM(theObject)

    if (theObject@status != "reg")
      price <- 0

    cba$benefits <- round(cba$allow * price / 12, digits = 2)

    return(cba[, c("month", "ID", "benefits")])
  }
)
