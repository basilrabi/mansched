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
#' @param cEnd  character string defining the date wherein the employment
#'   contract of the employee will end
#'
#'   The accepted format is \code{"yyyy-mm-dd"}.
#' @param year numeric value representing the year to be computed
#' @param status character string representing the employment status of the
#'   employee
#'
#'   See \code{\link{validEmpStatus}} for accepted values.
#' @return integer value
#' @export getLeaveHours
getLeaveHours <- function(cBegin, cEnd = NA, year, status) {

  if (!status %in% validEmpStatus)
    stop("Invalid employment status!")

  # No leaves for probationary
  if (status == "pro")
    return(0L)

  # 5 days service incentive leave per year for agency and seasonal
  if (status %in% c("sea", "age")) {

    if (is.na(cEnd))
      stop("cEnd must be present for agency and seasonal employees.")

    schedA   <- dates(begin = paste(year, "-01-01", sep = ""),
                      end   = paste(year, "-12-31", sep = ""))
    schedB   <- dates(begin = cBegin, end = cEnd)
    totDays  <- sum(schedB$date %in% schedA$date)
    totDays  <- totDays / nrow(schedA)

    return(as.integer((totDays * 5 * 8) + 0.5))
  }

  cBegin    <- as.Date(cBegin, origin = "1970-01-01")
  yearBegin <- paste(year, "-01-01", sep = "")
  yearBegin <- as.Date(yearBegin)
  yearEnd   <- paste(year, "-12-31", sep = "")
  yearEnd   <- as.Date(yearEnd)

  if (cBegin <= yearBegin) {

    days      <- as.numeric(difftime(yearEnd, cBegin, units = "days")) + 1
    years     <- (days * 10000) %/% 3652422
    leaveDays <- NA

    if (years < 6) {
      leaveDays <- 30
    } else if (years < 10) {
      leaveDays <- 32
    } else if (years < 20) {
      leaveDays <- 34
    } else if (years < 25) {
      leaveDays <- 40
    } else {
      leaveDays <- 44
    }

  } else {

    monthDiff <- which(month.name == months(yearEnd)) -
      which(month.name == months(cBegin)) + 1

    if (length(monthDiff) == 0) {
      stop("Undetermined month, system time language might not be in English.")
    }

    leaveDays <- monthDiff * 2.5
  }


  return(as.integer(leaveDays * 8))
}
