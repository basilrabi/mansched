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
#' @return integer value
#' @export getLeaveHours
getLeaveHours <- function(cBegin, year) {

  cBegin <- "2012-10-15"
  year <- 2018

  cBegin <- as.Date(cBegin)
  year <- paste(year, "-01-01", sep = "")
  year <- as.Date(year)

  days <- as.numeric(difftime(year, cBegin, units = "days"))
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

  return(as.integer(leaveDays * 8))
}
