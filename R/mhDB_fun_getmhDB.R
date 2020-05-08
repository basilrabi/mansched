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
#' @param forecast logical value \cr
#'   Compute cost for forecast?
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
getmhDB <- function(empReq,
                    empPool,
                    sched,
                    year = NA,
                    hol = NA,
                    forecast = FALSE) {

  # Define global variables
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

  tempData <- initEmpPool(empPool  = empPool,
                          hol      = hol,
                          year     = year,
                          forecast = forecast)
  listR    <- tempData[[1]]
  listR.a  <- listR
  empPool  <- tempData[[2]]

  tempData <- initEmpReq(empReq = empReq, sched = sched, hol = hol, year = year)
  listT    <- tempData[[1]]
  listT.a  <- listT
  empReq   <- tempData[[2]]

  # Assign only Employee-class objects that are present both in the pool and
  #   the requirement

  classPool <- unique(empPool$personnelClass)
  classReq <- unique(empReq$personnelClass)
  personnelClass <- intersect(classPool, classReq)

  indexPool <- which(empPool$personnelClass %in% personnelClass)
  indexReq <- which(empReq$personnelClass %in% personnelClass)

  # Separate Employee-class objects that cannot be assigned
  u.empPool <- empPool[-indexPool,]
  u.empReq  <- empReq[-indexReq,]
  u.listR   <- listR[-indexPool]
  u.listT   <- listT[-indexReq]

  empPool <- empPool[indexPool,]
  empReq  <- empReq[indexReq,]
  listR   <- listR[indexPool]
  listT   <- listT[indexReq]

  personnelSet <- lapply(personnelClass, FUN = function(x) {
    iP <- which(empPool$personnelClass == x)
    iR <- which(empReq$personnelClass == x)
    return(list(empReq[iR,], empPool[iP,], listT[iR], listR[iP]))
  })

  cat("Assigning employees.\n")

  assignedData <- lapply(X = personnelSet, FUN = function(x) {
    assignPrio(empReq  = x[[1]],
               empPool = x[[2]],
               listT   = x[[3]],
               listR   = x[[4]])
  })

  mhDB   <- lapply(assignedData, FUN = function(x) {x[[1]]})
  mhReq  <- lapply(assignedData, FUN = function(x) {x[[4]]})
  mhPool <- lapply(assignedData, FUN = function(x) {x[[5]]})
  mhDB   <- as.data.frame(data.table::rbindlist(mhDB  , use.names = TRUE))
  mhReq  <- as.data.frame(data.table::rbindlist(mhReq , use.names = TRUE))
  mhPool <- as.data.frame(data.table::rbindlist(mhPool, use.names = TRUE))

  if (nrow(mhDB) < 1)
    mhDB <- NULL

  if (nrow(mhReq) < 1)
    mhReq <- NULL

  if (nrow(mhPool) < 1)
    mhPool <- NULL

  listR <- unlist(lapply(assignedData, FUN = function(x) {x[[3]]}))
  listT <- unlist(sapply(assignedData, FUN = function(x) {x[[2]]}))

  if (length(u.listR) > 0) {

    listTN   <- lapply(u.listR, FUN = normEmp)

    u.mhPool <- lapply(listTN, FUN = function(x) {
      mh <- as.data.frame(getHours(x))
      mh$month <- 1:12
      mh$ID <- x@ID
      return(mh)
    })

    u.mhPool <- data.table::rbindlist(u.mhPool, use.names = TRUE)
    u.mhPool <- u.mhPool %>%
      tidyr::gather(key = "mhType",
                    value = mh,
                    -month,
                    -ID)

    mhPool <- data.table::rbindlist(list(mhPool, u.mhPool), use.names = TRUE)
    mhPool <- mhPool[mhPool$mh > 0,]
    mhPool <- as.data.frame(mhPool)

    if (nrow(mhPool) > 0) {

      for (i in 1:length(listTN)) {

        if (sum(getHours(listTN[[i]])) > 0) {

          tempData         <- assignEmp(empT = listTN[[i]], empR = listR[[i]])
          listTN[[i]]      <- tempData[[2]]
          listR[[i]]       <- tempData[[3]]
          tempData[[1]]$np <- 0L
          mhDB             <- dfAppend(mhDB, tempData[[1]])
        }

        if (sum(getHours(listTN[[i]])) != 0)
          stop("All man hours in listTN not assigned!")
      }

    } else {
      mhPool <- NULL
    }

  }

  if (length(u.listT) > 0) {

    u.mhReq <- lapply(u.listT, FUN = function(x) {

      mh       <- as.data.frame(getHours(x))
      mh$month <- 1:12
      mh$ID    <- x@ID

      return(mh)
    })

    u.mhReq <- data.table::rbindlist(u.mhReq, use.names = TRUE)
    u.mhReq <- u.mhReq %>%
      tidyr::gather(key = "mhType", value = mh, -month, -ID)
    mhReq <- data.table::rbindlist(list(mhReq, u.mhReq), use.names = TRUE)
    mhReq <- mhReq[mhReq$mh > 0, ]
    mhReq <- as.data.frame(mhReq)
  }

  return(list(mhDB, listT.a, listR.a, listT, listR, mhReq, mhPool))
}
