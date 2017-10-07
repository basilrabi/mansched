#' @import methods
NULL

# theObject <- listR[[1]]

#' Compute monthly hospital and medical expenses budgeted for employee
#'
#' Depending on the employment statys of the employee, his or her monthly
#'   hospital and medical allowances may be the following:
#'   \enumerate{
#'     \item 1,600 PhP monthly (reg)
#'     \item 1,000 PhP at 1st month of employment (pro)
#'     \item 600 PhP at 1st month of employment (sea)
#'   }
#'
#' @param theObject \code{\link{Employee-class}} object
#' @return a \code{\link{data.frame}} with 12 rows and 3 columns representing
#'   the total hospital and medical allowances  receives per month
#'
#'   Each row represents a month. The columns are:
#'   \describe{
#'      \item{ID}{character string representing the unique identifier of the
#'        real employee}
#'      \item{month}{integer value representing the month}
#'      \item{HM}{numeric value defining the total hospital and medical
#'        allowances budgeted to the employee}
#'   }
#' @export getHM
setGeneric(
  name = "getHM",
  def  = function(theObject) {
    standardGeneric("getHM")
  }
)

#' @describeIn getHM Compute allowance multiplier
setMethod(
  f          = "getHM",
  signature  = "Employee",
  definition = function(theObject) {

    sched <- getCM(theObject)

    if (theObject@status != "reg") {

      tempIndex <- min(which(sched$allow >0))
      sched$allow[-tempIndex] <- 0
    }

    if (theObject@status == "reg") {
      cost <- 1600
    } else if (theObject@status == "pro") {
      cost <- 1000
    } else if (theObject@status %in% c("sea", "age")) {
      cost <- 600
    } else {
      stop("Invalid employment status!")
    }

    sched$cost <- cost
    sched$HM   <- round(sched$cost * sched$allow, digits = 2)

    return(sched[, colnames(sched) %in% c("month", "ID", "HM")])
  }
)
