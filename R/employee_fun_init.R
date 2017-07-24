#' @import methods
NULL

#' Initialize employee
#'
#' Initializes employee data and its working hours. This function can be
#'   applied to both real and theoretical employees. Real employees represent
#'   the actual manpower pool while theoretical employees represent the required
#'   manpower and the required man hours.
#'
#' @param theObject \code{\link{Employee-class}} object
#' @param ID character string representing the employee's unique identifier
#' @param name character vector representing the name of employee
#' @param designation character string representing designation of employee
#' @param attendance numeric value used to account for the absenteeism of the
#'   employee
#' @param spareFactor numeric value used as manhours multiplier for theoretical
#'   employees
#'
#'   For an activity involving continious operation, a spare personnel is
#'   usually deployed to prevent accidents caused by fatigue. This factor is
#'   also used to compensate for the absenteeism of employees.
#' @param costCode character vector containing the cost codes wherein the
#'   employee will be charged
#' @param status character string representing the employment status of the
#'   employee
#'
#'   The accepted values are \code{'reg'} (regular), \code{'pro'} (probationary),
#'   and \code{'sea'} (seasonal).
#' @param isTheo logical value showing whether the object is representing a
#'   theoretical employee or not
#' @param hol a \code{\link{data.frame}} similar to \code{\link{holidays}}
#'
#'   This contains all the holidays in the year to be budgeted.
#' @export initEmployee
setGeneric(
  name = 'initEmployee',
  def = function(theObject, ID, name, designation, attendance = 1,
                 spareFactor = 1, costCode = "NONE", status = "reg", cBegin,
                 cEnd = NA, inHouse, isTheo, restday = "Sunday", hol) {
    standardGeneric('initEmployee')
  }
)

#' @describeIn initEmployee Initialize ID, name, designation, attendance,
#'   spareFactor, costCode, status, isTheo
setMethod(
  f = 'initEmployee',
  signature = 'Employee',
  definition = function(theObject, ID, name, designation, attendance,
                        spareFactor, costCode, status, isTheo, hol) {

    # Checking of the validity of all arguments must be done prior to calling
    #  initEmployee()

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
    theObject@costCode <- strsplit(x = costCode, split = ' ', fixed = TRUE)[[1]]

    # status must be an element of c('reg', 'pro', 'sea')
    theObject@status <- status

    theObject@isTheo <- isTheo
  }
)
