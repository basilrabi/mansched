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
#' @export getAllowance
setGeneric(
  name = "getAllowance",
  def = function(theObject) {
    standardGeneric("getAllowance")
  }
)

# setMethod(
#   f = "getAllowance",
#   signature = "Employee",
#   definition = function(theObject) {
#
#     year <- substr(theObject@cEnd, start = 1L, stop = 4L)
#
#
#     if (as.Date(theObject@cBegin) < year) {
#
#     }
#   }
# )
