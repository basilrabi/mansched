#' @import methods
NULL

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
