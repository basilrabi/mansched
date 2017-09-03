#' @import methods
NULL

# theObject <- listR[[1]]

#' Compute the monthly allowance of employee
#'
#' Depending on the attributes of the employee, his or her monthly allowances
#'   may include the following:
#'   \enumerate{
#'     \item food
#'     \item lighting
#'     \item housing
#'   }
#'
#' @param theObject \code{\link{Employee-class}} object
#' @return a \code{\link{data.frame}} with 12 rows and 3 columns representing
#'   the total allowances the employee receives per month
#'
#'   Each row represents a month. The columns are:
#'   \describe{
#'      \item{ID}{character string representing the unique identifier of the
#'        real employee}
#'      \item{month}{integer value representing the month}
#'      \item{cost}{numeric value defining the total allowances to be received
#'        by the employee}
#'   }
#' @importFrom lubridate month
#' @importFrom magrittr "%>%"
#' @importFrom dplyr group_by summarise n
#' @export getAllowance
setGeneric(
  name = "getAllowance",
  def = function(theObject) {
    standardGeneric("getAllowance")
  }
)

#' @describeIn getAllowance Compute allowance multiplier
setMethod(
  f = "getAllowance",
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

    return(sched[,c(1,4,5)])
  }
)

#' @describeIn getAllowance Compute monthly allowance
setMethod(
  f = "getAllowance",
  signature = "Staff",
  definition = function(theObject) {

    allowance <- callNextMethod(theObject)
    cost <- 0

    if(!theObject@inHouse)
      cost <- cost + 1900 + 1100 # for food and lighting

    cost <- cost + 1100 # for housing

    allowance$allowance <- round(allowance$allow * cost, digits = 2)

    return(allowance[,c(1, 3, 4)])
  }
)

#' @describeIn getAllowance Compute monthly allowance
setMethod(
  f = "getAllowance",
  signature = "Non Staff",
  definition = function(theObject) {

    allowance <- callNextMethod(theObject)
    cost <- 0

    if (isRF(theObject = theObject)) {

      if (isReg(theObject = theObject)) {

        cost <- cost + 700 # for lighting
        cost <- cost + 800 # for housing

      }
    } else {

      cost <- cost + 1800 # for food

      if (isReg(theObject = theObject)) {

        cost <- cost + 1100 # for housing

        if (!theObject@inHouse) {
          cost <- cost + 1100 # for lighting
        }

      }

    }

    allowance$allowance <- round(allowance$allow * cost, digits = 2)

    return(allowance[,c(1, 3, 4)])
  }
)
