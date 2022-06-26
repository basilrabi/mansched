#' Assign pool by priority
#'
#' This is used in \code{\link{getmhDB}}.
#'   Assignment is done in the following order:
#'   \enumerate{
#'     \item Any employee with a designated cost center is assigned.
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
#' @importFrom dplyr "%>%" bind_rows filter mutate select
#' @importFrom tidyr gather
assignPrio <- function(listT, listR) {

  ID <-
    costCenter <-
    discardedOT_00 <-
    discardedOT_dcc <-
    mh <-
    mhPool <-
    mhType <-
    partialDccLess <-
    tempData1 <-
    tempData2 <-
    tempData3 <- NULL

  message("1/3 run. Priority: with cost centers")
  tempData1 <- assignPool(listT, listR, prioCode = TRUE)
  listT <- tempData1[[1]]
  listR <- tempData1[[2]]

  if (length(listT) > 0 & length(listR) > 0) {
    message("2/3 run. Priority: regular and probationary")
    tempData2 <- assignPool(listT, listR, prioStat = c("reg", "pro"))
    listT <- tempData2[[1]]
    listR <- tempData2[[2]]

    if (length(listT) > 0 & length(listR) > 0) {
      message("3/3 run. Priority: none")
      tempData3 <- assignPool(listT, listR)
      listT <- tempData3[[1]]
      listR <- tempData3[[2]]
    }
  }

  tempData4 <- data.frame(ID = as.character(NA),
                          reqID = as.character(NA),
                          mh = as.integer(NA),
                          mhType = as.character(NA),
                          month = as.integer(NA),
                          np = as.numeric(NA),
                          costCenter = as.character(NA),
                          equipment = as.character(NA),
                          stringsAsFactors = FALSE)

  if (length(listR) > 0) {
    hasDCC <- sapply(listR, function(x) {
      if (any(x@dcc != "NA"))
        return(TRUE)
      return(FALSE)
    })
    # If there are any dump cost centers assigned, assign now.
    if (any(hasDCC)) {
      listR.dcc <- listR[hasDCC]
      listR <- listR[!hasDCC]

      # Drop overtime hours
      discardedOT_dcc <- lapply(listR.dcc, normEmp) %>%
        data.table::rbindlist() %>%
        dplyr::filter(mh > 0)
      aviHours <- sapply(listR.dcc, FUN = function(x) {sum(getHours(x))})
      listR.dcc <- listR.dcc[aviHours > 0]

      tempData4 <- lapply(listR.dcc, function(x) {
        x@costCenter <- sapply(X = x@dcc, FUN = function(y) {
          if (any(y == "NA"))
            return("0-0")
          return(y)
        })
        return(assignEmp(empT = x, empR = x, selfAssign = TRUE))
      }) %>%
        data.table::rbindlist() %>%
        dplyr::bind_rows(tempData4)
      partialDccLess <- dplyr::filter(tempData4, costCenter == "0-0") %>%
        dplyr::select(month, ID, mhType, mh)
    }
  }

  mhDB <- data.table::rbindlist(l = list(
    tempData1[[3]], tempData2[[3]], tempData3[[3]], tempData4
  ), use.names = TRUE) %>% as.data.frame()

  # Assign excess regular hours to a dummy cost center
  if (length(listR) > 0) {
    listTN <- copy(listR)
    discardedOT_00  <- lapply(listTN, normEmp) %>%
      data.table::rbindlist() %>%
      dplyr::filter(mh > 0)
    mhPool <- lapply(listTN, FUN = function(x) {
      mh <- as.data.frame(getHours(x))
      mh$month <- 1:12
      mh$ID <- x@ID
      return(mh)
    }) %>%
      data.table::rbindlist() %>%
      tidyr::gather(key = "mhType", value = mh, -month, -ID)
    mhPool <- mhPool[mhPool$mh > 0,]
    mhPool <- as.data.frame(mhPool)

    if (nrow(mhPool) > 0) {
      mhDB <- lapply(listTN, function(x) {
        tempData <- assignEmp(empT = x, empR = x, selfAssign = TRUE) %>%
          dplyr::mutate(np = 0)
        return(tempData)
      }) %>%
        data.table::rbindlist() %>%
        dplyr::bind_rows(mhDB)
      if (!is.null(partialDccLess)) {
        mhPool <- dplyr::bind_rows(mhPool, partialDccLess)
      }
    } else if (!is.null(partialDccLess)) {
      mhPool <- partialDccLess
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
