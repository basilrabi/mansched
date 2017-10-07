#' @import methods
NULL

#' isReg
#'
#' Is the employee regular?
#'
#' @param theObject an \code{\link{Employee-class}} object
#' @return logical value
#' @export isReg
setGeneric(
  name = "isReg",
  def  = function(theObject) {
    standardGeneric("isReg")
  }
)

#' @describeIn isReg Throws an error if employment status is not valid.
setMethod(
  f          = "isReg",
  signature  = "Employee",
  definition = function(theObject) {
    if (!theObject@status %in% validEmpStatus)
      stop("Invalid employment status!")
    if (theObject@status == "reg")
      return(TRUE)
    else
      return(FALSE)
  }
)
