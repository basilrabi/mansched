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
#' @seealso \code{\link{getCalDays}}
#' @export getHol
getHol <- function(hol, year) {

  yBegin <- paste(year, "01-01", sep = "-")
  yBegin <- as.Date(yBegin)
  yEnd <- paste(year, "12-31", sep = "-")
  yEnd <- as.Date(yEnd)

  date <- as.Date(yBegin:yEnd, origin = "1970-01-01")
  tempData <- data.frame(date = date)
  tempData$weekday <- weekdays(tempData$date)
  tempData$is.sh <- tempData$is.lh <- tempData$is.nh <- NA

  hol$nMonth <- apply(hol, MARGIN = 1, FUN = function(x){
    which(month.name == x[1])
  })
  hol$date <- paste(year, sprintf("%02i-%02i", hol$nMonth, hol$Day), sep = "-")
  hol$date <- as.Date(hol$date)
  hol <- hol[, c("date", "Type")]

  # Set Negotiated holidays
  tempDate <- hol$date[which(hol$Type == "Negotiated")]
  tempData$is.nh[which(tempData$date %in% tempDate)] <- TRUE
  tempData$is.nh[which(!tempData$date %in% tempDate)] <- FALSE

  # Set Legal holidays
  tempDate <- hol$date[which(hol$Type == "Legal")]
  tempData$is.lh[which(tempData$date %in% tempDate)] <- TRUE
  tempData$is.lh[which(!tempData$date %in% tempDate)] <- FALSE

  # Set Special holidays
  tempDate <- hol$date[which(hol$Type == "Special")]
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
#'   The accepted format is \code{"yyyy-mm-dd"}.
#' @param cEnd character string representing the end date
#'
#'   The accepted format is \code{"yyyy-mm-dd"}.
#' @param hol a \code{\link{data.frame}} returned by \code{\link{getHol}}
#' @param restday character string representing the day of the week defined as
#'   the rest day
#' @return a \code{data.frame} with 12 rows and 8 columns
#'
#' Each row represents a month while each column represents a man day type.
#'   The column names are:
#' \describe{
#'  \item{reg}{regular day}
#'  \item{rd}{rest day}
#'  \item{lh}{legal holiday}
#'  \item{sh}{special holiday}
#'  \item{nh}{negotiated holiday}
#'  \item{rl}{rest day and legal holiday}
#'  \item{rs}{rest day abd special holiday}
#'  \item{rn}{rest day and negotiated holiday}
#' }
#' @importFrom lubridate year month
#' @export getCalDays
getCalDays <- function(cBegin, cEnd = NA, hol, restday) {

  # hol <- getHol(holidays, 2018)
  # cBegin <- "2018-06-01"
  # restday <- "Sunday"
  # cEnd <- NA

  year <- lubridate::year(hol$date[1])
  cBegin <- as.Date(cBegin)
  cBegin.temp <- as.Date(paste(year, "01-01", sep = "-"))

  if (cBegin < cBegin.temp)
    cBegin <- cBegin.temp

  if (is.na(cEnd))
    cEnd <- as.Date(paste(year, "12-31", sep = "-"))

  if (cEnd < cBegin)
    stop("cEnd must be later than cBegin!")

  hol <- hol[which(hol$date >= cBegin & hol$date <= cEnd),]

  hol$is.rd <- FALSE
  hol$is.rd[which(hol$weekday == restday)] <- TRUE
  hol$mdType <- apply(X = hol[,c(3:6)], MARGIN = 1,FUN = function(x) {
    if (sum(x) == 0) {
      return("reg") # regular day
    } else if (sum(x) == 1) {
      if (x[4]) {
        return("rd") # rest day
      } else if (x[2]) {
        return("lh") # legal holiday
      } else if (x[3]) {
        return("sh") # special holiday
      } else if (x[1]) {
        return("nh") # negotiated holiday
      } else {
        stop("Somethings wrong with holidays.")
      }
    } else if (sum(x) == 2) {
      if (x[4] & x[2]) {
        return("rl") # rest day and legal holiday
      } else if (x[4] & x[3]) {
        return("rs") # rest day and special holiday
      } else if (x[4] & x[1]) {
        return("rn") # rest day and negotiated holiday
      }
    } else
      stop("Somethings wrong with holidays.")
  })

  hol$month <- lubridate::month(hol$date)
  hol <- hol[,c("month", "mdType")]
  hol <- table(hol)

  hol <- as.data.frame.matrix(hol)

  # Ensure all 12 rows are present
  hol$month <- row.names(hol)
  misMonth <- which(!(1:12) %in% hol$month)
  numRows <- length(misMonth)
  numCols <- length(hol)
  misMonthMat <- matrix(data = 0L,
                        nrow = numRows,
                        ncol = numCols)
  misMonthMat[,7] <- misMonth
  colnames(misMonthMat) <- colnames(hol)
  hol <- rbind(hol, misMonthMat)
  hol$month <- as.integer(hol$month)
  hol <- hol[order(hol$month),]
  hol$month <- NULL

  mhType <- c("reg", "rd", "sh", "lh", "nh", "rs", "rl", "rn")
  mis.mhType <- mhType[which(!mhType %in% colnames(hol))]

  if (length(mis.mhType) > 0) {
    for (i in mis.mhType) {
      tempCMD <- paste("hol$", i, " <- 0L", sep = "")
      eval(parse(text = tempCMD))
    }
  }

  return(hol)
}

#' Compute Man Day Type Probabilities
#'
#' Calculate the probability of occurrence of each man day type for each month.
#'
#' @param hol a \code{\link{data.frame}} returned by \code{\link{getHol}}
#' @return a \code{\link{data.frame}} with 12 rows and 9 columns
#'
#'   Each row represents a month. The first 8 columns represent the probability
#'   of occurence of each man day type while the last column represents the
#'   number of days per month. The column names are as follows:
#'   \describe{
#'     \item{rd}{probability of having a rest day}
#'     \item{sh}{probability of having a special holiday}
#'     \item{lh}{probability of having a legal holiday}
#'     \item{nh}{probability of having a negotiated holiday}
#'     \item{rs}{probability of having a rest day on a special holiday}
#'     \item{rl}{probability of having a rest day on a legal holiday}
#'     \item{rn}{probability of having a rest day on a negotiated holiday}
#'     \item{reg}{probability of having a regular day}
#'     \item{days}{number of days}
#'   }
#' @importFrom lubridate month
#' @export getMDTProb
getMDTProb <- function(hol) {

  if (any(hol$is.nh + hol$is.lh + hol$is.sh > 1))
    stop(paste("Multiple holidays at the same day!",
               "I can't handle that yet!",
               sep = " "))

  hol$type <- apply(hol[,c(3:5)], MARGIN = 1, FUN = function(x) {
    if (x[1])
      return("nh")
    else if (x[2])
      return("lh")
    else if (x[3])
      return("sh")
    else
      return("reg")
  })

  # Get the number of holidays per month
  hol$month <- lubridate::month(hol$date)
  hol <- hol[,c("month", "type")]
  hol <- table(hol)
  sched <- data.frame(month = as.integer(rownames(hol)),
                      reg = hol[, "reg"],
                      sh = hol[, "sh"],
                      lh = hol[, "lh"],
                      nh = hol[, "nh"])
  sched$days <- sched$reg + sched$sh + sched$lh + sched$nh

  # Compute for the probability of rd and all holiday types
  sched$prob.rd <- 1 / 7
  sched$prob.sh <- sched$sh / sched$days
  sched$prob.lh <- sched$lh / sched$days
  sched$prob.nh <- sched$nh / sched$days

  # Compute probability of rest day in a holiday
  sched$prob.rs <- sched$prob.rd * sched$prob.sh
  sched$prob.rl <- sched$prob.rd * sched$prob.lh
  sched$prob.rn <- sched$prob.rd * sched$prob.nh

  # Adjust the probabilities of exclusive rest days and holidays
  sched$prob.rd <- sched$prob.rd - (sched$prob.rs +
                                      sched$prob.rl + sched$prob.rn)
  sched$prob.sh <- sched$prob.sh - sched$prob.rs
  sched$prob.lh <- sched$prob.lh - sched$prob.rl
  sched$prob.nh <- sched$prob.nh - sched$prob.rn

  # Compute for probability of regular day
  sched$prob.reg <- 1 - (sched$prob.rd + sched$prob.sh + sched$prob.lh +
                           sched$prob.nh +sched$prob.rs + sched$prob.rl +
                           sched$prob.rn)

  sched <- sched[,c(7:14, 6)]

  # Truncate column names
  colnames(sched)[1:8] <- gsub(pattern = "prob.",
                          replacement = "",
                          x = colnames(sched)[1:8],
                          fixed = TRUE)
  return(sched)
}
