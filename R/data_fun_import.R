#' Initialize Employee Pool
#'
#' Initializes the list of the pool of employees and their corresponding
#'   maximum man hours which can be assigned to any activity.
#'
#' @param empPool a \code{data.frame} with 13 columns
#'
#'   Each row represents a real employee. The columns are:
#'   \describe{
#'     \item{ID}{character string representing the employee's unique identifier}
#'     \item{name}{character string representing the employee's name}
#'     \item{designation}{character string representing the employee's
#'       designation}
#'     \item{personnelClass}{character string representing the
#'       \code{\link{Employee-class}} sub-class to be applied}
#'     \item{attendance}{numeric value representing the attendance rate of
#'       employee
#'
#'       This value must be less than or equal to one and more than 0.5.}
#'     \item{equipment}{character string representing the equipment types which
#'       the employee was authorized to operate.
#'
#'       Each equipment type must be separated by a space.}
#'     \item{costCode}{character string representing the preferred cost code
#'       wherein the employee shall be assigned.
#'
#'       Multiple cost codes must be separated by spaces.}
#'     \item{status}{character string representing the employment status of the
#'       employee
#'
#'       Accepted values are \code{"reg"} (regular), \code{"pro"}
#'       (probationary), and \code{'sea'} (seasonal).}
#'     \item{cBegin}{character string representing the date of employment of
#'       personnel
#'
#'       The accepted format is ISO 8601 which expresses a day as
#'       \code{"yyyy-mm-dd"}.}
#'     \item{cEnd}{character string representing the date of termination
#'       employment of the seasonal employee
#'
#'       The accepted format is ISO 8601 which expresses a day as
#'       \code{"yyyy-mm-dd"}.}
#'     \item{inHouse}{logical value \cr
#'       Is the employee's accommodation given by the company?}
#'     \item{restday}{character string representing the day of the week wherein
#'       the employee is not required to report to work}
#'     \item{isRF}{logical value \cr
#'       Is the employee rank and file?}
#'   }
#' @param hol a \code{data.frame} similar to \code{\link{holidays}}
#' @param year integer value representing the year to be budgeted
#' @return list of real employees
#'
#'   Each element of the list is an \code{\link{Employee-class}} object.
#' @importFrom stringr str_to_lower str_to_upper
#' @export initEmpPool
initEmpPool <- function(empPool, hol = NA, year = NA) {

  # Error if any ID is duplicated
  if (anyDuplicated(empPool$ID) > 0) {
    tempData <- empPool$ID[which(duplicated(empPool$ID))]
    stop(paste("Duplicated;",tempData))
  }

  # Remove white spaces (including leading and trailing spaces)
  empPool[,c("personnelClass", "equipment", "costCode")] <- lapply(
    empPool[,c("personnelClass", "equipment", "costCode")],
    FUN = rmWS)

  # Remove space for status
  empPool$status <- rmS(empPool$status)


  # Remove punctuations for costCode
  empPool$costCode <- gsub(pattern = "[[:punct:]]",
                           replacement = "",
                           x = empPool$costCode)

  # Convert to lower case
  empPool[,c("personnelClass", "status")] <- lapply(
    empPool[,c("personnelClass", "status")],
    FUN = stringr::str_to_lower)

  # Change to upper case
  empPool[,c("equipment", "costCode")] <- lapply(
    empPool[,c("equipment", "costCode")],
    FUN = stringr::str_to_upper)

  # Get only the first 3 characters for employee status
  empPool$status <- substr(x = empPool$status, start = 1L, stop = 3L)

  # Check all personnelClass if valid
  if (any(!empPool$personnelClass %in% validEmpClass)) {
    tempIndex <- which(!empPool$Personnel %in% validEmpClass)
    stop(paste("Invalid personnelClass in rows:", tempIndex))
  }

  # Check for acceptable attendance
  tempIndex <- which(!is.na(empPool$attendance))
  if (any(empPool$attendance[tempIndex] > 1))
    stop("attendance must be less than or equal to 1")
  if (any(empPool$attendance[tempIndex] < 0.5))
    stop("Useless employee exists! \\\n attendance must be at least 0.5!")

  # Pre-allocate employee list
  manPool <- rep(list(NA), times = length(empPool[,1]))

  if (is.na(year)) {
    year <- as.integer(format(Sys.Date() + 365, "%Y"))
    message(sprintf("Using %d as year.", year))
  }

  if (any(is.na(hol))) {
    hol <- mansched::holidays
    message("Using built-in holidays.")
  }

  hol <- getHol(hol = hol, year = year)

  for (i in 1:length(empPool[,1])) {
    tempEmp <- createEmp(empPool$personnelClass[i])
    tempEmp <- initREmployee(theObject = tempEmp,
                             ID = empPool$ID[i],
                             name = empPool$name[i],
                             designation = empPool$designation[i],
                             attendance = empPool$attendance[i],
                             costCode = empPool$costCode[i],
                             status = empPool$status[i],
                             cBegin = empPool$cBegin[i],
                             cEnd = empPool$cEnd[i],
                             inHouse = empPool$inHouse[i],
                             restday = empPool$restday[i],
                             hol = hol,
                             RF = empPool$isRF[i],
                             equipment = empPool$equipment[i])
    manPool[[i]] <- tempEmp
  }

  return(manPool)
}

#' Initialize Employee Requirement
#'
#' Initializes the list of the employee requirements and their corresponding man
#'   hours.
#'
#' @param empReq a \code{\link{data.frame}} with 7 columns
#'
#'   Each row represents an employee requirement for an activity. The columns
#'   are:
#'   \describe{
#'     \item{activity}{character string representing the assigned activity
#'
#'       Activities listed in this \code{data.frame} must be present in
#'       \code{workSched}.}
#'     \item{personnelClass}{character string representing the
#'       \code{\link{Employee-class}} sub-class to be applied}
#'     \item{quantity}{integer value representing the number of required
#'       personnel for the employee requirement}
#'     \item{spareFactor}{numeric value of at least 1
#'
#'       This is used as man hours multiplier for deploying a spare personnel.
#'       For an activity involving continuous operation, a spare personnel is
#'       usually deployed to prevent accidents caused by fatigue. The
#'       \code{spareFactor} is also used to compensate for the absenteeism of
#'       employees.}
#'     \item{equipment}{character string representing the equipment type to be
#'       used
#'
#'       This only applies to \code{\link{Operator-class}} personnel.}
#'     \item{OT}{integer value defining the number of working hours scheduled
#'       for more than 8 hours per shift
#'
#'       This only applies to \code{\link{Non Staff-class}} personnel.}
#'     \item{costCode}{character string representing the accounting cost code
#'       wherein all personnel costs of this employee requirement will be
#'       charged}
#'   }
#' @param sched a \code{\link{data.frame}} with 13 columns
#'
#'   Each row represents one activity. The 1st column is vector of character
#'   strings representing the name of activity. The 2nd column up to the 13th
#'   column are all integer vectors representing the number of days an activity
#'   is scheduled for every month. The column names from the 2nd column up to
#'   the 13th column are month names. For office activities or for continuous
#'   activities, their corresponding row may be left blank.
#' @param hol a \code{data.frame} similar to \code{\link{holidays}}
#' @param year integer value representing the year to be budgeted
#' @return list of real employees
#'
#'   Each element of the list is an \code{\link{Employee-class}} object.
#' @importFrom stringr str_to_lower str_to_upper
#' @export initEmpReq
initEmpReq <- function(empReq, sched, hol = NA, year = NA) {

  # Error if scheduled any scheduled activity is duplicated
  if (anyDuplicated(sched$activity) > 0) {
    tempData <- sched$activity[which(duplicated(sched$activity))]
    stop(paste("Duplicated:",tempData))
  }

  # Error if assigned activity is not scheduled
  if (!all(unique(empReq$activity) %in% sched$activity)) {
    tempData <- unique(empReq$activity)
    tempIndex <- which(!tempData %in% sched$activity)
    stop(paste("No schedule:", tempData[tempIndex]))
  }

  # Warning if scheduled activity has no personnel assigned
  if (!all(sched$activity %in% unique(empReq$activity))) {
    tempData <- sched$activity[
      which(!sched$activity %in% unique(empReq$activity))]
    cat(paste("No personnel assigned: ", tempData, "\n", sep = ""))
    warning("Schedules without employees detected!")
  }

  # Error if personnel assignment is duplicated
  if (anyDuplicated(empReq) > 0) {
    tempIndex <- which(duplicated(empReq))
    cat(paste("Duplicate row:",
              empReq$activity[tempIndex],
              empReq$personnelClass[tempIndex],
              empReq$costCode[tempIndex],
              "\n",
              sep = " "))
    stop("Check duplicate!")
  }

  # Trim and remove white spaces
  empReq$personnelClass <- rmWS(empReq$personnelClass)

  # Remove spaces for costCode and equipment
  empReq[,c("equipment", "costCode")] <- lapply(
    empReq[,c("equipment", "costCode")],
    FUN = rmS
  )

  # Remove punctuations for costCode
  empReq$costCode <- gsub(pattern = "[[:punct:]]",
                          replacement = '',
                          x = empReq$costCode)

  # Change to lower case
  empReq$personnelClass <- stringr::str_to_lower(empReq$personnelClass)

  # Change to upper case
  empReq[,c("equipment", "costCode")] <- lapply(
    empReq[,c("equipment", "costCode")],
    FUN = stringr::str_to_upper
  )

  # Check accepted employee class
  if (any(!empReq$personnelClass %in% validEmpClass)) {
    tempIndex <- which(!empReq$personnelClass %in% validEmpClass)
    stop(paste("Invalid employee class in rows:", tempIndex))
  }

  # Initialize premium probabilities
  hol <- getHol(hol = hol, year = year)
  calDays <- getCalDays(cBegin = paste(year,"-01-01", sep = ""),
                        hol = hol,
                        restday = "Sunday")
  mdtProb <- getMDTProb(hol = hol)

  # Pre-allocate list of Employee-class objects
  manReq <- rep(list(NA), times = length(empReq[,1]))

  for (i in 1:length(empReq[,1])) {

    schedIndex <- which(sched$activity == empReq$activity[i])
    tempEmp <- createEmp(empReq$personnelClass[i])

    if (all(is.na(sched[schedIndex,c(2:13)]))) {
      workSched <- NA
    } else {
      workSched <- as.integer(sched[schedIndex,c(2:13)])
      workSched[is.na(workSched)] <- 0L
    }

    if (is.na(empReq$spareFactor[i]))
      empReq$spareFactor[i] <- 1

    if (is.na(empReq$OT[i]))
      empReq$OT[i] <- 0

    tempEmp <- initTEmployee(theObject = tempEmp,
                             ID = paste(empReq$activity[i],
                                        empReq$personnelClass[i],
                                        sep = "-"),
                             costCode = empReq$costCode[i],
                             equipment = empReq$equipment[i],
                             OT = empReq$OT[i],
                             calDays = calDays,
                             mdtProb = mdtProb,
                             spareFactor = empReq$spareFactor[i] *
                               empReq$quantity[i],
                             monthSched = workSched)

    manReq[[i]] <- tempEmp
  }

  return(manReq)
}
