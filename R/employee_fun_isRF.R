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
  def  = function(theObject) {
    standardGeneric("isRF")
  }
)

#' @describeIn isRF Set \code{NA} as default value
setMethod(
  f          = "isRF",
  signature  = "Employee",
  definition = function(theObject) {
    return(NA)
  }
)

#' @describeIn isRF All staff are not rank and file
setMethod(
  f          = "isRF",
  signature  = "Staff",
  definition = function(theObject) {
    return(FALSE)
  }
)

#' @describeIn isRF All \code{\link{NonStaff-class}} objects have isRF slot
setMethod(
  f          = "isRF",
  signature  = "NonStaff",
  definition = function(theObject) {

    RF <- theObject@isRF

    if (length(RF) != 1)
      stop("Length of isRF slot != 1.")

    if (is.na(RF))
      stop(paste("isRF slot of ",
                 theObject@ID,
                 " is not initialized."))

    return(theObject@isRF)
  }
)

#' @describeIn isRF Clerks may or may not be rank and file
setMethod(
  f          = "isRF",
  signature  = "Clerk",
  definition = function(theObject) {
    return(callNextMethod(theObject))
  }
)

#' @describeIn isRF Technical personnel are not rank and file
setMethod(
  f          = "isRF",
  signature  = "Technical",
  definition = function(theObject) {
    RF <- callNextMethod(theObject)

    if (RF)
      stop(paste(theObject@ID, "was declared rank and file."))

    return(FALSE)
  }
)

#' @describeIn isRF Supervisors are not rank and file
setMethod(
  f          = "isRF",
  signature  = "Supervisor",
  definition = function(theObject) {

    RF <- callNextMethod(theObject)

    if (RF)
      stop(paste(theObject@ID, "was declared rank and file."))

    return(FALSE)
  }
)

#' @describeIn isRF Laborers are rank and file
setMethod(
  f          = "isRF",
  signature  = "Laborer",
  definition = function(theObject) {
    RF <- callNextMethod(theObject)

    if (!RF)
      stop(paste(theObject@ID, "was not declared rank and file."))

    return(TRUE)
  }
)

#' @describeIn isRF Operators are rank and file
setMethod(
  f          = "isRF",
  signature  = "Operator",
  definition = function(theObject) {
    RF <- callNextMethod(theObject)

    if (!RF)
      stop(paste(theObject@ID, "was not declared rank and file."))

    return(TRUE)
  }
)
