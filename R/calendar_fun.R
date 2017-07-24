#' Create holiday database
#'
#' Creates a schedule for all types of holidays throughout the year in a form of
#'   a database including the weekday of each date.
#'
#' @param hol a \code{\link{data.frame}} similar to \code{\link{holidays}}
#' @param year numeric value representing the year to be computed
#' @return a \code{\link{data.frame}} with 5 columns
#'
#' Each row represents a day from the year computed. The columns are:
#' \describe{
#'   \item{date}{date}
#'   \item{weekday}{day of the week}
#'   \item{is.nh}{logical \cr
#'     Is the date a negotiated holiday?
#'   }
#'   \item{is.lh}{logical \cr
#'     Is the date a legal holiday?
#'   }
#'   \item{is.sh}{logical \cr
#'     Is the date a special holiday?
#'   }
#' }
#' @export getHol
getHol <- function(hol, year) {

  yBegin <- paste(year, '01-01', sep = '-')
  yBegin <- as.Date(yBegin)
  yEnd <- paste(year, '12-31', sep = '-')
  yEnd <- as.Date(yEnd)

  date <- as.Date(yBegin:yEnd, origin = '1970-01-01')
  tempData <- data.frame(date = date)
  tempData$weekday <- weekdays(tempData$date)
  tempData$is.sh <- tempData$is.lh <- tempData$is.nh <- NA

  hol$nMonth <- apply(hol, MARGIN = 1, FUN = function(x){
    which(month.name == x[1])
  })
  hol$date <- paste(year, sprintf('%02i-%02i', hol$nMonth, hol$Day), sep = '-')
  hol$date <- as.Date(hol$date)
  hol <- hol[, c('date', 'Type')]

  # Set Negotiated holidays
  tempDate <- hol$date[which(hol$Type == 'Negotiated')]
  tempData$is.nh[which(tempData$date %in% tempDate)] <- TRUE
  tempData$is.nh[which(!tempData$date %in% tempDate)] <- FALSE

  # Set Legal holidays
  tempDate <- hol$date[which(hol$Type == 'Legal')]
  tempData$is.lh[which(tempData$date %in% tempDate)] <- TRUE
  tempData$is.lh[which(!tempData$date %in% tempDate)] <- FALSE

  # Set Special holidays
  tempDate <- hol$date[which(hol$Type == 'Special')]
  tempData$is.sh[which(tempData$date %in% tempDate)] <- TRUE
  tempData$is.sh[which(!tempData$date %in% tempDate)] <- FALSE

  return(tempData)
}


#' Get Calendar Days
#'
#' Using a date range, the available working days type is computed. The
#' beginning and ending date must be on the same year.
#'
#' @param cBegin character string representing the start date
#'
#'   The accepted format is \code{'yyyy-mm-dd'}.
#' @param cEnd character string representing the end date
#'
#'   The accepted format is \code{'yyyy-mm-dd'}.
#' @param year numeric value representing the year to be budgeted
#' @param hol a \code{\link{data.frame}} return by \code{\link{getHol}}
#' @param restday character string representing the day of the week defined as
#'   the rest day
#' @return a \code{matrix} with 12 rows and 7 columns
#'
#' Each row represents a month while each column represents a man hour type.
#' @importFrom lubridate month
#' @export getCalDays
getCalDays <- function(cBegin, cEnd = NA, year, hol, restday) {

  cBegin <- as.Date(cBegin)
  cBegin.temp <- as.Date(paste(year, '01-01', sep = '-'))

  if (cBegin < cBegin.temp)
    cBegin <- cBegin.temp

  if (is.na(cEnd))
    cEnd <- as.Date(paste(year, '12-31', sep = '-'))

  hol <- hol[which(hol$date >= cBegin & hol$date <= cEnd),]

  hol$is.rd <- FALSE
  hol$is.rd[which(hol$weekday == restday)] <- TRUE
  hol$mdType <- apply(X = hol[,c(3:6)], MARGIN = 1,FUN = function(x) {
    if (sum(x) == 0) {
      return('reg') # regular day
    } else if (sum(x) == 1) {
      if (x[4]) {
        return('rd') # rest day
      } else if (x[2]) {
        return('lh') # legal holiday
      } else if (x[3]) {
        return('sh') # special holiday
      } else if (x[1]) {
        return('nh') # negotiated holiday
      } else {
        return(NA)
      }
    } else if (sum(x) == 2) {
      if (x[4] & x[2]) {
        return('rl') # rest day and legal holiday
      } else if (x[4] & x[3]) {
        return('rs') # rest day and special holiday
      } else if (x[4] & x[1]) {
        return('rn') # rest day and negotiated holidau
      }
    } else
      return(NA)
  })

  hol$month <- lubridate::month(hol$date)
  hol <- hol[,c('month', 'mdType')]
  hol <- table(hol)
  return(tempData)
}
