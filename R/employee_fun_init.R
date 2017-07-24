#' @import methods
NULL

#' Initialize real employee
#'
#' Initializes employee data and its working hours. This function can be
#'   applied only to real employees. Real employees represent the actual
#'   manpower pool.
#'
#' @param theObject \code{\link{Employee-class}} object
#' @param ID character string representing the employee's unique identifier
#' @param name character vector representing the name of employee
#' @param designation character string representing designation of employee
#' @param attendance numeric value used to account for the absenteeism of the
#'   employee
#' @param costCode character vector containing the cost codes wherein the
#'   employee will be charged
#' @param status character string representing the employment status of the
#'   employee
#'
#'   The accepted values are \code{"reg"} (regular), \code{"pro"}
#'   (probationary), and \code{"sea"} (seasonal).
#' @param hol a \code{\link{data.frame}} returned by \code{\link{getHol}}
#'
#'   This contains all the holidays in the year to be budgeted.
#' @param isRF logical value \cr
#'   Is the employee rank and file?
#' @param equipment character vector representing the equipment types which the
#'   employee is authorized to operate
#' @export initREmployee
setGeneric(
  name = "initREmployee",
  def = function(theObject,
                 ID,
                 name,
                 designation,
                 attendance = 1,
                 costCode = "NONE",
                 status = "reg",
                 cBegin,
                 cEnd = NA,
                 inHouse,
                 restday = "Sunday",
                 hol,
                 isRF,
                 equipment) {
    standardGeneric("initREmployee")
  }
)

#' @describeIn initREmployee Initialize ID, name, designation, attendance,
#'   spareFactor, costCode, status, isTheo
setMethod(
  f = "initREmployee",
  signature = 'Employee',
  definition = function(theObject,
                        ID,
                        name,
                        designation,
                        attendance,
                        costCode,
                        status,
                        cBegin,
                        cEnd = NA,
                        inHouse,
                        restday) {
    # Checking of the validity of all arguments must be done prior to calling
    #  initREmployee()

    theObject@ID <- ID
    theObject@name <- name
    theObject@designation <- designation

    # attendance must be <= 1 but preferrabbly > 0.5
    theObject@attendance <- attendance

    # spareFactor must be >= 1
    theObject@spareFactor <- spareFactor

    # Vectorize costCode
    #  This is done under the assumption that white spaces and punctuations are
    #  already removed. Also, all characters are in upper case.
    theObject@costCode <-
      strsplit(x = costCode, split = " ", fixed = TRUE)[[1]]

    # status must be an element of c("reg", "pro", "sea")
    theObject@status <- status

    theObject@isTheo <- isTheo

    # restday must be a valid weekday prior to calling initEmployee
    theObject@restday <- restday
  }
)
