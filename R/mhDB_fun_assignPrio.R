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
#' @param empReq passed from \code{\link{getmhDB}}
#' @param empPool passed from \code{\link{getmhDB}}
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
#'   }
#' @export assignPrio
assignPrio <- function(empReq, empPool, listT, listR) {

  # Define global variables
  ID        <- NULL
  mh        <- NULL
  tempData1 <- NULL
  tempData2 <- NULL
  tempData3 <- NULL

  tempData1 <- assignPool(empReq   = empReq,
                          empPool  = empPool,
                          listT    = listT,
                          listR    = listR,
                          prioCode = TRUE)

  empReq  <- tempData1[[1]]
  empPool <- tempData1[[2]]
  listT   <- tempData1[[3]]
  listR   <- tempData1[[4]]

  if (length(listT) > 0 & length(listR) > 0) {

    tempData2 <- assignPool(empReq   = empReq,
                            empPool  = empPool,
                            listT    = listT,
                            listR    = listR,
                            prioStat = c("reg", "pro"))

    empReq  <- tempData2[[1]]
    empPool <- tempData2[[2]]
    listT   <- tempData2[[3]]
    listR   <- tempData2[[4]]

  }

  if (length(listT) > 0 & length(listR) > 0) {

    tempData3 <- assignPool(empReq   = empReq,
                            empPool  = empPool,
                            listT    = listT,
                            listR    = listR)

    empReq  <- tempData3[[1]]
    empPool <- tempData3[[2]]
    listT   <- tempData3[[3]]
    listR   <- tempData3[[4]]

  }

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

      mh       <- as.data.frame(getHours(x))
      mh$month <- 1:12
      mh$ID    <- x@ID

      return(mh)
    })

    mhPool <- data.table::rbindlist(mhPool)

    mhPool <- mhPool %>%
      tidyr::gather(key   = "mhType",
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

  return(list(mhDB, listT, listR, mhReq, mhPool))
}