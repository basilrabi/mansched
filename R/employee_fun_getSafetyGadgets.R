#' @import methods
NULL

#' Compute cost for safety gadgets
#'
#' Taganito Mine provides steel toe shoes, umbrella, raincoats, and rain boots
#'   to all in-house employees.
#'
#' @param theObject \code{\link{Employee-class}} object
#' @return a \code{\link{data.frame}} with 12 rows and 3 columns representing
#'   the pro-rated cost of safety gadgets per month
#'
#'   Each row represents a month. The columns are:
#'   \describe{
#'      \item{ID}{character string representing the unique identifier of the
#'        real employee}
#'      \item{month}{integer value representing the month}
#'      \item{gadgets}{numeric value defining the cost charged for that month}
#'   }
#' @export getSafetyGadgets
setGeneric(
  name = "getSafetyGadgets",
  def  = function(theObject) {
    standardGeneric("getSafetyGadgets")
  }
)

#' @describeIn getSafetyGadgets Compute price and cost multplier
setMethod(
  f          = "getSafetyGadgets",
  signature  = "Employee",
  definition = function(theObject) {

    shoes <- rep(2800 / 12, times = 12L)
    # Umbrella, raincoat, rain boots
    others <- rep(2000 / 12, times = 12L)
    sg <- getCM(theObject)

    if (theObject@status == "age") {
      shoes <-  shoes * 0
      others <- others * 0
    }

    if (theObject@field)
      shoes <- shoes * 1.5

    sg$sg <- round(sg$allow * (shoes + others), digits = 2)

    return(sg[, c("month", "ID", "sg")])
  }
)
