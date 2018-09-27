#' @import methods
NULL

#' Compute the monthly rice subsidy of employee
#'
#' For every month, 1 sack of rice is awarded to the employee.
#'   Once sack costs around 2,500 PhP.
#'
#' @param theObject \code{\link{Employee-class}} object
#' @return a \code{\link{data.frame}} with 12 rows and 3 columns representing
#'   the rice subsidy the employee receives per month
#'
#'   Each row represents a month. The columns are:
#'   \describe{
#'      \item{ID}{character string representing the unique identifier of the
#'        real employee}
#'      \item{month}{integer value representing the month}
#'      \item{riceSub}{numeric value defining the rice subsidy to be received
#'        by the employee}
#'   }
#' @export getRiceSub
setGeneric(
  name = "getRiceSub",
  def  = function(theObject) {
    standardGeneric("getRiceSub")
  }
)

#' @describeIn getRiceSub Compute rice subsidy throughout the year
setMethod(
  f          = "getRiceSub",
  signature  = "Employee",
  definition = function(theObject) {

    rice <- 2500

    if (theObject@forecast & theObject@status != "age")
      rice <- 0

    if (grepl("BARGE MAINTENANCE", theObject@name))
      rice <- 0

    if (grepl("ORE BREAKER", theObject@name)) {
      if (!theObject@forecast) {
        rice <- rice * 0.5
      } else {
        rice <- 0
      }
    }

    riceSub         <- getCM(theObject)
    riceSub$riceSub <- round(riceSub$allow * rice, digits = 2)

    return(riceSub[, c("month", "ID", "riceSub")])
  }
)
