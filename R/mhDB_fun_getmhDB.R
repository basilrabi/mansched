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

  listR <- initEmpPool(empPool = empPool, hol = hol, year)
  listT <- initEmpReq(empReq = empReq, sched = sched, hol = hol, year = year)

  # Create vector for indexing qualified employees
  empPool$hasAviHours <- FALSE
  empPool$matchClass <- FALSE
  empPool$matchEquip <- FALSE
  empPool$matchCostCode <- FALSE

  # Create vector for identifying selected employees to be used
  empPool$choice1 <- FALSE # AviHours + Class + Equip + CostCode
  empPool$choice2 <- FALSE # AviHours + Class + Equip - CostCode

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

  # Assign employees to empReq
  for(i in 1:length(empReq[,1])) {

    tempClass <- class(listT[[i]])
    if (tempClass == "Operator") {
      tempEquip <- listT[[i]]@equipment
    } else {
      tempEquip <- NA
    }

    tempCostCode <- listT[[i]]@costCode

    # Get employee with available manhours
    empPool$hasAviHours <- sapply(listR, FUN = function(x) {
      tempHours <- sum(getHours(x))
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
      if (tempClass != "Operator")
        return(TRUE)
      else {
        if (tempEquip %in% x@equipment)
          return(TRUE)
        else
          return(FALSE)
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
    # choice1 has matched cost code
    empPool$choice1 <- apply(empPool[,c(14:17)], MARGIN = 1, FUN = function(x) {
      all(x)
    })
    # choice2 does not match cost code
    empPool$choice2 <- apply(empPool[,c(14:17)], MARGIN = 1, FUN = function(x) {
      all(x[1:3],!x[4])
    })

    # Select choice1 and assign
    index <- which(empPool$choice1)

    if (length(index) > 0) {

      if (length(index) > 1) {
        # Randomize
        index <- sample(index)
      }

      # Assign
      if (sum(getHours(listT[[i]])) > 0) {
        for (j in index) {
          tempData <- assignEmp(empT = listT[[i]], empR = listR[[j]])
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

    if (sum(getHours(listT[[i]])) > 0) {
      # Select choice2 and assign
      index <- which(empPool$choice2)

      if (length(index) > 0) {

        if (length(index) > 1) {
          # Randomize
          index <- sample(index)
        }

        # Assign
        for (j in index) {
          tempData <- assignEmp(empT = listT[[i]], empR = listR[[j]])
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

  # Assign excess regular hours to a dummy cost code

  ## Create a theoretical employee list
  listTN <- lapply(listR, FUN = normEmp)

  for (i in 1:length(listTN)) {

    # message(paste("Got here, i = ", i, sep = ""))
    # print(listTN[[i]])

    if (sum(getHours(listTN[[i]])) > 0) {
      tempData <- assignEmp(empT = listTN[[i]], empR = listR[[i]])
      listTN[[i]] <- tempData[[2]]
      listR[[i]] <- tempData[[3]]
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

  return(list(mhDB, listT, listR))
}
