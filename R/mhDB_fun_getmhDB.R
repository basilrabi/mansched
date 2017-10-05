#' Compute man hours database
#'
#' Computes a man hours database throughout the year to be budgeted.
#'
#' @param empReq a \code{\link{data.frame}} similar to \code{empReq} in
#'   \code{\link{initEmpReq}}
#' @param empPool a \code{data.frame} similar to \code{empPool} in
#'   \code{\link{initEmpPool}}
#' @param sched a \code{data.frame} similar to \code{sched} in
#'   \code{\link{initEmpReq}}
#' @param year an integer value representing the year to be budgeted
#' @param hol a \code{data.frame} similar to \code{\link{holidays}}
#' @return a list containing the following:
#'   \enumerate{
#'     \item \code{data.frame} representing the man hours database of the
#'       assigned employees
#'
#'       This is composed of the following columns:
#'       \describe{
#'         \item{ID}{character string representing the unique identifier of the
#'           real employee}
#'         \item{mh}{integer value representing the man hours assigned}
#'         \item{mhType}{character string representing the man hours type
#'           assigned (see \code{\link{assignEmp}})}
#'         \item{month}{integer value representing the month}
#'         \item{np}{integer value representing the man hours with night premium
#'           pay}
#'         \item{costCode}{character string representing accounting cost code
#'           wherein the man hours is charged}
#'         \item{sal}{a single character: \code{"a"} or \code{"b"}
#'
#'           This represents the salary to be applied (see
#'           \code{\link{payA}} and \code{\link{payB}}).}
#'         \item{scheme}{character string defining the salary scheme
#'
#'           This may be \code{"m"} (monthly) or \code{"d"} (daily).}
#'         \item{status}{character string defining the employement status of
#'           the employee}
#'         \item{maxReg}{integer value \cr
#'           Number of hours the employee is required to report to enjoy full
#'           salary.}
#'       }
#'     \item list of \code{\link{Employee-class}} objects representing the
#'       theoretical employees with un-assigned man hours
#'     \item list of \code{\link{Employee-class}} objects representing the
#'       real employees with un-assigned man hours
#'     \item list of \code{\link{Employee-class}} objects representing the
#'       theoretical employees with assigned man hours
#'     \item list of \code{\link{Employee-class}} objects representing the
#'       real employees with assigned man hours
#'     \item \code{\link{data.frame}} containing un-assigned man hours of the
#'       theoretical employees
#'
#'       This is composed of the following columns:
#'       \describe{
#'         \item{month}{integer value representing the month}
#'         \item{ID}{character string representing the employee requirement}
#'         \item{mhType}{man hour type}
#'         \item{mh}{integer value representing the unassigned man hours}
#'       }
#'     \item \code{\link{data.frame}} containing un-assigned man hours of the
#'       real employees
#'
#'       This is composed of the following columns:
#'       \describe{
#'         \item{month}{integer value representing the month}
#'         \item{ID}{character string representing the employee requirement}
#'         \item{mhType}{man hour type}
#'         \item{mh}{integer value representing the unassigned man hours}
#'       }
#'   }
#' @export getmhDB
#' @importFrom tidyr gather
#' @importFrom data.table rbindlist
getmhDB <- function(empReq, empPool, sched, year = NA, hol = NA) {

  # Define global variables
  mhReq     <- NULL

  if (is.na(year)) {
    year <- as.integer(format(Sys.Date() + 365, "%Y"))
    message(sprintf("Using %d as year.", year))
  }

  if (any(is.na(hol[1]))) {
    hol <- mansched::holidays
    message("Using built-in holidays list.")
  }

  tempData <- initEmpPool(empPool = empPool, hol = hol, year)
  listR    <- tempData[[1]]
  listR.a  <- listR
  empPool  <- tempData[[2]]

  tempData <- initEmpReq(empReq = empReq, sched = sched, hol = hol, year = year)
  listT    <- tempData[[1]]
  listT.a  <- listT
  empReq   <- tempData[[2]]

  # Create vector for indexing qualified employees
  empPool$hasAviHours   <- FALSE
  empPool$matchClass    <- FALSE
  empPool$matchEquip    <- FALSE
  empPool$matchCostCode <- FALSE
  empPool$choice        <- FALSE

  tempData <- assignPrio(empReq  = empReq,
                         empPool = empPool,
                         listT   = listT,
                         listR   = listR)

  mhDB   <- tempData[[1]]
  listT  <- tempData[[2]]
  listR  <- tempData[[3]]
  mhReq  <- tempData[[4]]
  mhPool <- tempData[[5]]

  return(list(mhDB, listT.a, listR.a, listT, listR, mhReq, mhPool))
}
