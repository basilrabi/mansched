#' @import methods
NULL

#' Get cost multiplier
#'
#' Based on the period of employment of the personnel, his or her cost
#'   multipliers throught the year is computed.
#'   This multiplier is used for SSS, 13th month pay, and hospital and medical
#'   expenses.
#'
#' @param theObject \code{\link{Employee-class}} object
#' @return a \code{\link{data.frame}} with 12 rows and 13 columns
#'
#'   Each row represents a month while the columns are:
#'   \describe{
#'     \item{month}{integer value representing the month}
#'     \item{ID}{character string representing the unique identifier of the
#'       employee}
#'     \item{allow}{numeric value of 0 to 1 representing the cost multiplier}
#'   }
#' @importFrom lubridate month
#' @importFrom magrittr "%>%"
#' @importFrom dplyr group_by summarise n
#' @export getCM
setGeneric(
  name = "getCM",
  def = function(theObject) {
    standardGeneric("getCM")
  }
)

#' @describeIn getCM Compute allowance multiplier
setMethod(
  f = "getCM",
  signature = "Employee",
  definition = function(theObject) {
    tempYear <- substr(theObject@cEnd, start = 1, stop = 4)
    sched <- dates(begin = paste(tempYear, "-01-01", sep = ""),
                   end = paste(tempYear, "-12-31", sep = ""))

    sched$month <- as.integer(lubridate::month(sched$date))

    schedEmp <- sched[which(sched$date >= as.Date(theObject@cBegin) &
                              sched$date <= as.Date(theObject@cEnd)),]

    sched <- sched %>%
      dplyr::group_by(month) %>%
      dplyr::summarise(days = n())

    schedEmp <- schedEmp %>%
      dplyr::group_by(month) %>%
      dplyr::summarise(daysEmp = n())

    sched <- dplyr::left_join(sched, schedEmp)
    sched[is.na(sched)] <- 0
    sched$allow <- sched$daysEmp / sched$days
    sched <- as.data.frame(sched)
    sched$ID <- theObject@ID

    return(sched[, colnames(sched) %in% c("month", "ID", "allow")])
  }
)
