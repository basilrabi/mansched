#' @import methods
NULL

#' Get remaining working hours
#'
#' This function returns all remaining man hour types including overtime.
#'
#' @param theObject an \code{\link{Employee-class}} object
#' @return a \code{\link{data.frame}} representing the remaining man hours
#'
#'   Each row represents a month while each column represents a man hour type.
#' @export getHours
setGeneric(
  name = "getHours",
  def  = function(theObject) {
    standardGeneric("getHours")
  }
)

#' @describeIn getHours Save \code{reg} hours
setMethod(
  f          = "getHours",
  signature  = "Employee",
  definition = function(theObject) {

    tempData <- rep(0L, times = 12)

    mh <- data.frame(reg   = theObject@reg,
                     rd    = tempData,
                     lh    = tempData,
                     sh    = tempData,
                     nh    = tempData,
                     rl    = tempData,
                     rs    = tempData,
                     rn    = tempData,
                     regOT = tempData,
                     rdOT  = tempData,
                     lhOT  = tempData,
                     shOT  = tempData,
                     nhOT  = tempData,
                     rlOT  = tempData,
                     rsOT  = tempData,
                     rnOT  = tempData)

    return(mh)
  }
)

#' @describeIn getHours Save \code{regOT}
setMethod(
  f          = "getHours",
  signature  = "NonStaff",
  definition = function(theObject) {

    mh            <- callNextMethod(theObject)
    mh[, "regOT"] <- theObject@regOT

    return(mh)
  }
)

#' @describeIn getHours Save all other man hour type
setMethod(
  f = "getHours",
  signature = "OperationPersonnel",
  definition = function(theObject) {

    mh           <- callNextMethod(theObject)
    mh[, "rd"  ] <- theObject@rd
    mh[, "sh"  ] <- theObject@sh
    mh[, "lh"  ] <- theObject@lh
    mh[, "nh"  ] <- theObject@nh
    mh[, "rs"  ] <- theObject@rs
    mh[, "rl"  ] <- theObject@rl
    mh[, "rn"  ] <- theObject@rn
    mh[, "rdOT"] <- theObject@rdOT
    mh[, "shOT"] <- theObject@shOT
    mh[, "lhOT"] <- theObject@lhOT
    mh[, "nhOT"] <- theObject@nhOT
    mh[, "rsOT"] <- theObject@rsOT
    mh[, "rlOT"] <- theObject@rlOT
    mh[, "rnOT"] <- theObject@rnOT

    return(mh)
  }
)

#' Get remaining working hours of list of \code{\link{Employee-class}} objects
#'
#' This function returns the sum of all remaining man hours of a list of
#'   \code{Employee-class} objects including overtime
#'
#' @param x a list of \code{\link{Employee-class}} objects
#' @return an integer value
#' @export getHoursL
getHoursL <- function(x) {

  if (length(x) == 0)
    return(0L)

  tempData <- sapply(x, FUN = function(x) {
    tempHours <- getHours(x)
    sum(tempHours)
  })

  return(sum(tempData))
}
