#' Assign pool by priority
#'
#' This is used in \code{\link{getmhDB}}.
#'   Assignment is done in the following order:
#'   \enumerate{
#'     \item Any employee with a designated cost code is assigned.
#'     \item Any regular or probationary employee is assigned.
#'     \item All employees are assigned.
#'   }
#'
#' @param listT list of theoretical employees created from empReq
#' @param listR list of real employees created fro empPool
#' @return a list containing the following:
#'   \enumerate{
#'     \item man hour database resulting from the assignment
#'        This is also merged and described well in \code{\link{getmhDB}}.
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
#'     \item man hour database resulting from discarding OT hours. The structure
#'       is similar to what is being returned by \code{\link{getmhDB}}.
#'   }
#' @export assignPrio
#' @importFrom data.table rbindlist
#' @importFrom dplyr "%>%"
#' @importFrom tidyr gather
assignPrio <- function(listT, listR) {

  # Define global variables
  ID              <- NULL
  discardedOT_00  <- NULL
  discardedOT_dcc <- NULL
  mh              <- NULL
  mhPool          <- NULL
  tempData1       <- NULL
  tempData2       <- NULL
  tempData3       <- NULL

  tempData1 <- assignPool(listT, listR, prioCode = TRUE)
  listT   <- tempData1[[1]]
  listR   <- tempData1[[2]]

  if (length(listT) > 0 & length(listR) > 0) {
    tempData2 <- assignPool(listT, listR, prioStat = c("reg", "pro"))
    listT   <- tempData2[[1]]
    listR   <- tempData2[[2]]

    if (length(listT) > 0 & length(listR) > 0) {
      tempData3 <- assignPool(listT, listR)
      listT   <- tempData3[[1]]
      listR   <- tempData3[[2]]
    }
  }

  tempData4 <- data.frame(ID       = NA,
                          reqID    = NA,
                          mh       = NA,
                          mhType   = NA,
                          month    = NA,
                          np       = NA,
                          costCode = NA)

  if (length(listR) > 0) {
    poolDCC <- sapply(listR, function(x){x@dcc})
    # If there are any dump cost centers assigned, assign now.
    if (!all(is.na(poolDCC))) {
      tempIndex <- which(!is.na(poolDCC))
      listR.dcc <- listR[tempIndex]
      listR <- listR[-tempIndex]

      # Drop overtime hours
      discardedOT_dcc <- lapply(listR.dcc, normEmp) %>%
        data.table::rbindlist()
      aviHours <- sapply(listR.dcc, FUN = function(x) {sum(getHours(x))})
      listR.dcc <- listR.dcc[which(aviHours > 0)]

      for (i in listR.dcc) {
        i@costCode <- i@dcc
        tempMHDB  <- assignEmp(empT = i, empR = i, selfAssign = TRUE)
        tempData4 <- dfAppend(tempData4, tempMHDB)
      }
    }
  }

  mhDB <- data.table::rbindlist(l = list(
    tempData1[[3]], tempData2[[3]], tempData3[[3]], tempData4
  ), use.names = TRUE) %>% as.data.frame()

  # Assign excess regular hours to a dummy cost code
  if (length(listR) > 0) {
    listTN <- listR
    discardedOT_00  <- lapply(listTN, normEmp) %>%
      data.table::rbindlist()
    mhPool <- lapply(listTN, FUN = function(x) {
      mh       <- as.data.frame(getHours(x))
      mh$month <- 1:12
      mh$ID    <- x@ID
      return(mh)
    }) %>%
      data.table::rbindlist() %>%
      tidyr::gather(key   = "mhType",
                    value = mh,
                    -month,
                    -ID)
    mhPool <- mhPool[mhPool$mh > 0,]
    mhPool <- as.data.frame(mhPool)

    if (nrow(mhPool) > 0) {

      for (i in listTN) {
        if (sum(getHours(i)) > 0) {
          tempData    <- assignEmp(empT = i, empR = i, selfAssign = TRUE)
          tempData$np <- 0L
          mhDB        <- dfAppend(mhDB, tempData)
        }
      }
    } else {
      mhPool <- NULL
    }
  } else {
    listR <- NULL
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
    }) %>%
      data.table::rbindlist() %>%
      tidyr::gather(key = "mhType", value = mh, -month,-ID) %>%
      dplyr::filter(mh > 0) %>%
      as.data.frame()
  } else {
    listT <- NULL
    mhReq <- NULL
  }

  discarded <- data.table::rbindlist(list(discardedOT_dcc, discardedOT_00))
  if (nrow(discarded) < 1)
    discarded <- NULL

  return(list(mhDB, listT, listR, mhReq, mhPool, discarded))
}
