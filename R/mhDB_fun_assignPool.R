#' Assign pool to requirement
#'
#' This is used in \code{\link{assignPrio}}.
#'
#' @param empReq passed from \code{\link{assignPrio}}
#' @param empPool passed from \code{\link{assignPrio}}
#' @param listT list of theoretical employees created from empReq
#' @param listR list of real employees created fro empPool
#' @param prioStat character vector defining the employee status that are
#'   prioritized in assigning man hours
#' @param prioCode logical value \cr
#'   Is cost code prioritized in assigning?
#' @return a list containing the following:
#'   \enumerate{
#'     \item remaining empReq
#'     \item remaining empPool
#'     \item remaining listT
#'     \item remaining listR
#'     \item man hour database resulting from the assignment
#'
#'       This is also merged and described well in \code{\link{getmhDB}}.
#'   }
#' @export assignPool
assignPool <- function(empReq,
                       empPool,
                       listT,
                       listR,
                       prioStat = NA,
                       prioCode = FALSE) {

  mhDB <- data.frame(ID       = NA,
                     mh       = NA,
                     mhType   = NA,
                     month    = NA,
                     np       = NA,
                     costCode = NA,
                     sal      = NA,
                     scheme   = NA,
                     status   = NA,
                     maxReg   = NA)

  for(i in 1:length(empReq[,1])) {

    if (sum(getHours(listT[[i]])) == 0)
      next

    tempClass <- class(listT[[i]])

    if (tempClass == "Operator") {
      tempEquip <- listT[[i]]@equipment
    } else {
      tempEquip <- NA
    }

    tempCostCode <- listT[[i]]@costCode

    # Get available man hours
    empPool$hasAviHours <- sapply(listR, FUN = function(x) {

      tempHoursR <- getHours(x)

      if (sum(tempHoursR) < 0.1)
        return(FALSE)

      tempHoursT <- getHours(listT[[i]])

      tempHoursR[,"reg"] <- tempHoursR[,"reg"] + tempHoursR[,"rd"]
      tempHoursR[,"sh"]  <- tempHoursR[,"sh"]  + tempHoursR[,"rs"]
      tempHoursR[,"lh"]  <- tempHoursR[,"lh"]  + tempHoursR[,"rl"]
      tempHoursR[,"nh"]  <- tempHoursR[,"nh"]  + tempHoursR[,"rn"]

      tempHoursR[,"regOT"] <- tempHoursR[,"regOT"] + tempHoursR[,"rdOT"]
      tempHoursR[,"shOT"]  <- tempHoursR[,"shOT"]  + tempHoursR[,"rsOT"]
      tempHoursR[,"lhOT"]  <- tempHoursR[,"lhOT"]  + tempHoursR[,"rlOT"]
      tempHoursR[,"nhOT"]  <- tempHoursR[,"nhOT"]  + tempHoursR[,"rnOT"]

      tempHoursR[tempHoursT == 0] <- 0L

      tempHours <- sum(tempHoursR)

      if (tempHours > 0) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    })

    # Get matching Employee-class
    if (any(is.na(prioStat))) {
      empPool$matchClass <- TRUE
    } else {
      empPool$matchClass <- sapply(listR, FUN = function(x) {
        return(x@status %in% prioStat)
      })
    }

    # Get matching equipment
    empPool$matchEquip <- sapply(listR, FUN = function(x) {
      if (tempClass != "Operator") {
        return(TRUE)
      } else {
        if (class(x) != "Operator") {
          return(FALSE)
        } else {
          if (tempEquip %in% x@equipment) {
            return(TRUE)
          } else {
            return(FALSE)
          }
        }
      }
    })

    # Get matching cost code
    if (prioCode) {

      empPool$matchCostCode <- sapply(listR, FUN = function(x) {
        if (tempCostCode %in% x@costCode) {
          return(TRUE)
        } else {
          return(FALSE)
        }
      })

    } else {
      empPool$matchCostCode <- TRUE
    }

    # Filter selections
    empPool$choice <- apply(
      empPool[,colnames(empPool) %in% c("hasAviHours",
                                        "matchClass",
                                        "matchEquip",
                                        "matchCostCode")],
      MARGIN = 1,
      FUN = function(x) {all(x)}
    )

    # Select choice and assign

    index <- which(empPool$choice)

    if (length(index) > 0) {

      if (length(index) > 1) {

        # Randomize
        index <- sample(index)

        if (tempClass == "Operator") {

          nEquip <- sapply(listR[index], FUN = function(x) {
            length(x@equipment)
          })

          tempData <- cbind(index, nEquip)
          tempData <- tempData[order(nEquip),]

          index <- tempData[, 1]
        }

      }

      # Assign
      if (sum(getHours(listT[[i]])) > 0) {

        for (j in index) {

          suppressMessages(
            tempData <- assignEmp2(empT = listT[[i]], empR = listR[[j]])
          )

          listT[[i]] <- tempData[[2]]
          listR[[j]] <- tempData[[3]]

          if (class(tempData[[1]]) != "logical")
            mhDB <- dfAppend(mhDB, tempData[[1]])

          if (sum(getHours(listT[[i]])) == 0)
            break

        }
      }

      remMH <- sapply(listR[index], FUN = function(x) {sum(getHours(x))})

      toRM <- which(remMH < 1)

      if (length(toRM) > 0) {

        toRM    <- index[toRM]
        empPool <- empPool[-toRM,]
        listR   <- listR[-toRM]

      }

    }

  }

  remMH <- sapply(listT, FUN = function(x) {sum(getHours(x))})

  toRM <- which(remMH < 1)

  if (length(toRM) > 0) {

    empReq <- empReq[-toRM,]
    listT  <- listT[-toRM]

  }

  # Remove NA values at the bottom
  if (any(is.na(mhDB[,1]))) {

    index <- which(is.na(mhDB[,1]))
    mhDB  <- mhDB[-index,]

  }

  return(list(empReq, empPool, listT, listR, mhDB))
}
