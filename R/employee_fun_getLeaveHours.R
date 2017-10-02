#' @import methods
NULL

#' Compute Leave Hours
#'
#' Calculates the authorized leave hours of an employee based on the date of the
#'   start of employment and a year to be budgeted. This is only applicable to
#'   regular employees.
#'
#' @param cBegin character string defining the date wherein the employment
#'   contract of the employee began
#'
#'   The accepted format is \code{"yyyy-mm-dd"}.
#' @param year numeric value representing the year to be computed
#' @param status character string representing the employment status of the
#'   employee
#'
#'   See \code{\link{validEmpStatus}} for accepted values.
#' @return integer value
#' @export getLeaveHours
getLeaveHours <- function(cBegin, year, status) {

  if (!status %in% validEmpStatus)
    stop("Invalid employment status!")

  # 5 days service incentive leave for non-regular employees
  if (status != "reg")
    return(as.integer(5 * 8))

  cBegin <- as.Date(cBegin, origin = "1970-01-01")

  yearBegin <- paste(year, "-01-01", sep = "")
  yearBegin <- as.Date(yearBegin)

  yearEnd <- paste(year, "-12-31", sep = "")
  yearEnd <- as.Date(yearEnd)

  if (cBegin <= yearBegin) {
    days <- as.numeric(difftime(yearEnd, cBegin, units = "days")) + 1
    years <- (days * 10000) %/% 3652422
    leaveDays <- NA
    if (years < 6)
      leaveDays <- 30
    else if (years < 10)
      leaveDays <- 32
    else if (years < 20)
      leaveDays <- 34
    else if (years < 25)
      leaveDays <- 40
    else
      leaveDays <- 44
  } else {
    monthDiff <- which(month.name == months(yearEnd)) -
      which(month.name == months(cBegin)) + 1
    leaveDays <- monthDiff * 2.5
  }


  return(as.integer(leaveDays * 8))
}
