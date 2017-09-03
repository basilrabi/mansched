#' @import methods
NULL

# theObject <- listR[[1]]

#' Compute monthly accrued 13th month pay
#'
#' 13th month pay is pro-rated to the number of months the employee is employed
#'   for the year. This is then distributed equally to all months of employment.
#'
#' @param theObject \code{\link{Employee-class}} object
#' @param sal a numeric vector of length 2
#'
#'   This contains 2 hourly salaries of the employee.
#' @return a \code{\link{data.frame}} with 12 rows and 3 columns representing
#'   the accrued 13th month pay per month
#'
#'   Each row represents a month. The columns are:
#'   \describe{
#'      \item{ID}{character string representing the unique identifier of the
#'        real employee}
#'      \item{month}{integer value representing the month}
#'      \item{cost}{numeric value defining the accrued 13th month pay}
#'   }
#' @importFrom lubridate month
#' @importFrom magrittr "%>%"
#' @importFrom dplyr group_by summarise n
#' @export get13mp
setGeneric(
  name = "get13mp",
  def = function(theObject, sal) {
    standardGeneric("get13mp")
  }
)

#' @describeIn get13mp Compute 13 month pay
setMethod(
  f = "get13mp",
  signature = "Employee",
  definition = function(theObject, sal) {

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

    if (theObject@status != "reg") {
      cost <- sal[1]
    } else {
      cost <- sal[2]
    }

    cost <- cost * 8 * 313 / (12 * 12)

    sched$cost <- cost

    sched$mp <- round(sched$cost * sched$allow, digits = 2)

    return(sched[,c(1,5,7)])
  }
)
