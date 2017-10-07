#' @import methods
NULL

#' Strip all over time hours from an employee
#'
#' This function returns an \code{\link{Employee-class}} object with over time
#'   hours removed (see \code{\link{premium}}). The cost code is also changed
#'   into a dummy cost code \code{"0-0"}.
#'
#' @param theObject an \code{\link{Employee-class}} object
#' @return an \code{\link{Employee-class}} object with no over time man hour
#'   type and a dummy cost code.
#' @export normEmp
setGeneric(
  name = "normEmp",
  def = function(theObject) {
    standardGeneric("normEmp")
  }
)

#' @describeIn normEmp Assign dummy cost code
setMethod(
  f = "normEmp",
  signature = "Employee",
  definition = function(theObject) {
    theObject@costCode <- "0-0"
    return(theObject)
  }
)

#' @describeIn normEmp Assign zero to regOT
setMethod(
  f = "normEmp",
  signature = "Non Staff",
  definition = function(theObject) {

    theObject <- callNextMethod(theObject)
    theObject@regOT <- rep(0L, times = 12)

    return(theObject)
  }
)

#' @describeIn normEmp Assign zero to \code{rd}, \code{sh}, \code{lh},
#'   \code{rs}, \code{rl}, \code{rn}, \code{rdOT}, \code{shOT}, \code{lhOT},
#'   \code{nhOT}, \code{rsOT}, \code{rlOT}, \code{rnOT}. If the employee is
#'   regular, also assign zero to \code{nh}.
setMethod(
  f = "normEmp",
  signature = "Operation Personnel",
  definition = function(theObject) {

    theObject <- callNextMethod(theObject)

    zero <- rep(0L, times = 12)

    theObject@rd   <- zero
    theObject@sh   <- zero
    theObject@lh   <- zero
    theObject@rs   <- zero
    theObject@rl   <- zero
    theObject@rn   <- zero
    theObject@rdOT <- zero
    theObject@shOT <- zero
    theObject@lhOT <- zero
    theObject@nhOT <- zero
    theObject@rsOT <- zero
    theObject@rlOT <- zero
    theObject@rnOT <- zero

    if (isReg(theObject = theObject)) {
      theObject@nh <- zero
    }

    return(theObject)
  }
)

#' @describeIn normEmp Take only one equipment
setMethod(
  f = "normEmp",
  signature = "Operator",
  definition = function(theObject) {

    theObject <- callNextMethod(theObject)
    theObject@equipment <- theObject@equipment[1]

    return(theObject)
  }
)
