#' @import methods
NULL

#' isRF
#'
#' Is the employee rank and file?
#'
#' @param theObject an \code{\link{Employee-class}} object
#' @return logical value
#' @export isRF
setGeneric(
  name = "isRF",
  def = function(theObject) {
    standardGeneric("isRF")
  }
)

#' @describeIn isRF Set \code{NA} as default value
setMethod(
  f = "isRF",
  signature = "Employee",
  definition = function(theObject) {
    return(NA)
  }
)

#' @describeIn isRF All staff are not rank and file
setMethod(
  f = "isRF",
  signature = "Staff",
  definition = function(theObject) {
    return(FALSE)
  }
)

#' @describeIn isRF All \code{\link{Non Staff-class}} objects have isRF slot
setMethod(
  f = "isRF",
  signature = "Non Staff",
  definition = function(theObject) {
    if (length(theObject@isRF) != 1)
      stop("Check isRF slot of the object!")

    return(theObject@isRF)
  }
)

#' @describeIn isRF Clerks may or may not be rank and file
setMethod(
  f = "isRF",
  signature = "Clerk",
  definition = function(theObject) {
    return(callNextMethod(theObject))
  }
)

#' @describeIn isRF Technical personnel are not rank and file
setMethod(
  f = "isRF",
  signature = "Technical",
  definition = function(theObject) {
    isRF <- callNextMethod(theObject)

    if (isRF)
      warning("A Technical-class was declared rank and file.")

    return(FALSE)
  }
)

#' @describeIn isRF Supervisors are not rank and file
setMethod(
  f = "isRF",
  signature = "Supervisor",
  definition = function(theObject) {
    isRF <- callNextMethod(theObject)

    if (isRF)
      warning("A Supervisor-class was declared rank and file.")

    return(FALSE)
  }
)

#' @describeIn isRF Laborers are rank and file
setMethod(
  f = "isRF",
  signature = "Laborer",
  definition = function(theObject) {
    isRF <- callNextMethod(theObject)

    if (!isRF)
      warning("A Laborer-class was not declared rank and file.")

    return(TRUE)
  }
)

#' @describeIn isRF Operators are rank and file
setMethod(
  f = "isRF",
  signature = "Operator",
  definition = function(theObject) {
    isRF <- callNextMethod(theObject)

    if (!isRF)
      warning("An Operator-class was not declared rank and file.")

    return(TRUE)
  }
)
