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
#'       assigned employees. This is composed of the following columns:
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
#'         \item{isReg}{logical\cr
#'           Is the employee regular?}
#'         \item{maxReg}{integer value \cr
#'           Number of hours the employee is required to report to enjoy full
#'           salary.}
#'       }
#'     \item list of \code{\link{Employee-class}} objects representing the
#'       theoretical employees with assigned man hours
#'     \item list of \code{\link{Employee-class}} objects representing the
#'       real employees with assigned man hours
#'   }
#' @export getmhDB
getmhDB <- function(empReq, empPool, sched, year = NA, hol = NA) {

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
  empPool <- tempData[[2]]

  tempData <- initEmpReq(empReq = empReq, sched = sched, hol = hol, year = year)
  listT <- tempData[[1]]
  empReq <- tempData[[2]]

  # Create vector for indexing qualified employees
  empPool$hasAviHours <- FALSE
  empPool$matchClass <- FALSE
  empPool$matchEquip <- FALSE
  empPool$matchCostCode <- FALSE

  # Create vector for identifying selected employees to be used
  empPool$choice <- FALSE

  # Initialize man hours database
  mhDB <- data.frame(ID = NA,
                     mh = NA,
                     mhType = NA,
                     month = NA,
                     np = NA,
                     costCode = NA,
                     sal = NA,
                     scheme = NA,
                     isReg = NA,
                     maxReg = NA)

  # Assign employees to empReq - priority
  for(i in 1:length(empReq[,1])) {

    if (sum(getHours(listT[[i]])) == 0)
      next

    cat(paste("Priority assigning: ",
              i,
              " out of ",
              length(empReq[,1]),
              " requirements.",
              sep = ""))

    tempClass <- class(listT[[i]])
    if (tempClass == "Operator") {
      tempEquip <- listT[[i]]@equipment
    } else {
      tempEquip <- NA
    }

    tempCostCode <- listT[[i]]@costCode

    # Get employee with available manhours
    empPool$hasAviHours <- sapply(listR, FUN = function(x) {

      tempHoursR <- getHours(x)
      tempHoursT <- getHours(listT[[i]])

      tempHoursR[,"reg"] <- tempHoursR[,"reg"] + tempHoursR[,"rd"]
      tempHoursR[,"sh"] <- tempHoursR[,"sh"] + tempHoursR[,"rs"]
      tempHoursR[,"lh"] <- tempHoursR[,"lh"] + tempHoursR[,"rl"]
      tempHoursR[,"nh"] <- tempHoursR[,"nh"] + tempHoursR[,"rn"]

      tempHoursR[,"regOT"] <- tempHoursR[,"regOT"] + tempHoursR[,"rdOT"]
      tempHoursR[,"shOT"] <- tempHoursR[,"shOT"] + tempHoursR[,"rsOT"]
      tempHoursR[,"lhOT"] <- tempHoursR[,"lhOT"] + tempHoursR[,"rlOT"]
      tempHoursR[,"nhOT"] <- tempHoursR[,"nhOT"] + tempHoursR[,"rnOT"]

      tempHoursR[tempHoursT == 0] <- 0L

      tempHours <- sum(tempHoursR)

      if (tempHours > 0) {

        return(TRUE)

      } else {

        return(FALSE)

      }

    })

    # Get matching Employee-class
    empPool$matchClass <- sapply(listR, FUN = function(x) {
      if (tempClass == class(x))
        return(TRUE)
      else
        return(FALSE)
    })

    # Get matching equipment
    empPool$matchEquip <- sapply(listR, FUN = function(x) {

      if (tempClass != "Operator") {
        return(TRUE)
      } else {

        if (class(x) != "Operator") {
          return(FALSE)
        } else {
          if (tempEquip %in% x@equipment)
            return(TRUE)
          else
            return(FALSE)
        }
      }
    })

    # Get matching cost code
    empPool$matchCostCode <- sapply(listR, FUN = function(x) {
      if (tempCostCode %in% x@costCode)
        return(TRUE)
      else
        return(FALSE)
    })

    # Filter selections
    empPool$choice <- apply(
      empPool[,colnames(empPool) %in% c("hasAviHours",
                                        "matchClass",
                                        "matchEquip",
                                        "matchCostCode")],
      MARGIN = 1,
      FUN = function(x) {
        all(x)
      })

    cat(paste("\nFound ",sum(empPool$choice), " qualified.\n"))
    cat("\n")

    # Select choice and assign

    index <- which(empPool$choice)

    if (length(index) > 0) {

      if (length(index) > 1) {
        # Randomize
        index <- sample(index)
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
    }
  }

  # Assign employees to empReq - non priority
  for(i in 1:length(empReq[,1])) {

    if (sum(getHours(listT[[i]])) == 0)
      next

    cat(paste("Non-priority assigning: ",
              i,
              " out of ",
              length(empReq[,1]),
              " requirements.",
              sep = ""))

    tempClass <- class(listT[[i]])
    if (tempClass == "Operator") {
      tempEquip <- listT[[i]]@equipment
    } else {
      tempEquip <- NA
    }

    tempCostCode <- listT[[i]]@costCode

    # Get employee with available manhours
    empPool$hasAviHours <- sapply(listR, FUN = function(x) {

      tempHoursR <- getHours(x)
      tempHoursT <- getHours(listT[[i]])

      tempHoursR[tempHoursT == 0] <- 0L

      tempHours <- sum(tempHoursR)

      if (tempHours > 0)
        return(TRUE)
      else
        return(FALSE)
    })

    # Get matching Employee-class
    empPool$matchClass <- sapply(listR, FUN = function(x) {
      if (tempClass == class(x))
        return(TRUE)
      else
        return(FALSE)
    })

    # Get matching equipment
    empPool$matchEquip <- sapply(listR, FUN = function(x) {

      if (tempClass != "Operator") {
        return(TRUE)
      } else {

        if (class(x) != "Operator") {
          return(FALSE)
        } else {
          if (tempEquip %in% x@equipment)
            return(TRUE)
          else
            return(FALSE)
        }
      }
    })

    # Get matching cost code
    empPool$matchCostCode <- sapply(listR, FUN = function(x) {
      if (tempCostCode %in% x@costCode)
        return(TRUE)
      else
        return(FALSE)
    })

    # Filter selections
    empPool$choice <- apply(
      empPool[,colnames(empPool) %in% c("hasAviHours",
                                        "matchClass",
                                        "matchEquip",
                                        "matchCostCode")],
      MARGIN = 1,
      FUN = function(x) {
        all(x[1:3],!x[4])
      })

    cat(paste("\nFound ",sum(empPool$choice), " qualified.\n"))
    cat("\n")

    # Select choice and assign
    index <- which(empPool$choice)

    if (length(index) > 0) {

      if (length(index) > 1) {
        # Randomize
        index <- sample(index)
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
            # if (!is.na(tempData[[1]]))
            mhDB <- dfAppend(mhDB, tempData[[1]])

          if (sum(getHours(listT[[i]])) == 0)
            break
        }
      }
    }
  }

  # Assign excess regular hours to a dummy cost code

  ## Create a theoretical employee list
  listTN <- lapply(listR, FUN = normEmp)

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

  # Remove NA values at the bottom
  if (any(is.na(mhDB[,1]))) {
    index <- which(is.na(mhDB[,1]))
    mhDB <- mhDB[-index,]
  }

  mhReq <- lapply(listT, FUN = function(x) {

    mh <- as.data.frame(getHours(x))
    mh$month <- 1:12
    mh$ID <- x@ID

    return(mh)
  })

  mhReq <- data.table::rbindlist(mhReq)
  mhReq <- as.data.frame(mhReq)

  return(list(mhDB, listT, listR, mhReq))
}
