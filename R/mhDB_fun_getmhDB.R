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

  # Fix warning: Undefined global functions or variable
  ID    <- NULL
  mh    <- NULL
  mhReq <- NULL

  if (is.na(year)) {
    year <- as.integer(format(Sys.Date() + 365, "%Y"))
    message(sprintf("Using %d as year.", year))
  }

  if (any(is.na(hol[1]))) {
    hol <- mansched::holidays
    message("Using built-in holidays list.")
  }

  tempData <- initEmpPool(empPool = empPool, hol = hol, year)
  listR <- tempData[[1]]
  listR.a <- listR
  empPool <- tempData[[2]]

  tempData <- initEmpReq(empReq = empReq, sched = sched, hol = hol, year = year)
  listT <- tempData[[1]]
  listT.a <- listT
  empReq <- tempData[[2]]

  # Create vector for indexing qualified employees
  empPool$hasAviHours   <- FALSE
  empPool$matchClass    <- FALSE
  empPool$matchEquip    <- FALSE
  empPool$matchCostCode <- FALSE

  # Create vector for identifying selected employees to be used
  empPool$choice <- FALSE

  tempData1 <- assignPool(empReq   = empReq,
                          empPool  = empPool,
                          listT    = listT,
                          listR    = listR,
                          prioStat = c("reg", "pro"),
                          prioCode = TRUE)

  empReq  <- tempData1[[1]]
  empPool <- tempData1[[2]]
  listT   <- tempData1[[3]]
  listR   <- tempData1[[4]]

  tempData2 <- assignPool(empReq   = empReq,
                          empPool  = empPool,
                          listT    = listT,
                          listR    = listR,
                          prioStat = c("reg", "pro"))

  empReq  <- tempData2[[1]]
  empPool <- tempData2[[2]]
  listT   <- tempData2[[3]]
  listR   <- tempData2[[4]]

  tempData3 <- assignPool(empReq   = empReq,
                          empPool  = empPool,
                          listT    = listT,
                          listR    = listR)

  empReq  <- tempData3[[1]]
  empPool <- tempData3[[2]]
  listT   <- tempData3[[3]]
  listR   <- tempData3[[4]]

  mhDB <- data.table::rbindlist(l = list(tempData1[[5]],
                                         tempData2[[5]],
                                         tempData3[[5]]))

  mhDB <- as.data.frame(mhDB)

  # Assign excess regular hours to a dummy cost code

  mhPool <- NULL

  ## Create a theoretical employee list
  if (length(listR) > 0) {

    listTN <- lapply(listR, FUN = normEmp)

    mhPool <- lapply(listTN, FUN = function(x) {

      mh <- as.data.frame(getHours(x))
      mh$month <- 1:12
      mh$ID <- x@ID

      return(mh)
    })

    mhPool <- data.table::rbindlist(mhPool)

    mhPool <- mhPool %>%
      tidyr::gather(key = "mhType",
                    value = mh,
                    -month,
                    -ID)

    mhPool <- mhPool[mhPool$mh > 0,]
    mhPool <- as.data.frame(mhPool)

    if (nrow(mhPool) > 0) {

      for (i in 1:length(listTN)) {

        if (sum(getHours(listTN[[i]])) > 0) {

          suppressMessages(
            tempData <- assignEmp(empT = listTN[[i]], empR = listR[[i]])
          )

          listTN[[i]] <- tempData[[2]]
          listR[[i]] <- tempData[[3]]
          tempData[[1]]$np <- 0L
          mhDB <- dfAppend(mhDB, tempData[[1]])

        }

        if (sum(getHours(listTN[[i]])) != 0)
          stop("Something went wrong. :(")

      }

    } else {
      mhPool <- NULL
    }

  }

  # Remove NA values at the bottom
  if (any(is.na(mhDB[,1]))) {

    index <- which(is.na(mhDB[,1]))
    mhDB  <- mhDB[-index,]

  }

  if (length(listT) > 0) {

    mhReq <- lapply(listT, FUN = function(x) {

      mh <- as.data.frame(getHours(x))
      mh$month <- 1:12
      mh$ID <- x@ID

      return(mh)
    })

    mhReq <- data.table::rbindlist(mhReq)

    mhReq <- mhReq %>%
      tidyr::gather(key = "mhType",
                    value = mh,
                    -month,
                    -ID)

    mhReq <- mhReq[mhReq$mh > 0,]
    mhReq <- as.data.frame(mhReq)

  }

  return(list(mhDB, listT.a, listR.a, listT, listR, mhReq, mhPool))
}
