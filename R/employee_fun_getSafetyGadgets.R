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

    boots <- 400
    raincoat <- 1600
    shoes <- 2750
    umbrella <- 400
    if (theObject@forecast) {
      boots <- 300
      umbrella <- 300
    }

    if (theObject@field)
      shoes <- shoes * 1.5

    cost <- boots + raincoat + shoes + umbrella
    if (theObject@status == "age")
      cost <- 0

    monthly_cost <- rep(cost / 12, times = 12L)
    sg <- getCM(theObject)
    sg$sg <- round(sg$allow * monthly_cost, digits = 2)

    return(sg[, c("month", "ID", "sg")])
  }
)
