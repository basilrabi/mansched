#' Initialize Employee Pool
#'
#' Initializes the list of the pool of employees and their corresponding
#'   maximum man hours which can be assigned to any activity.
#'
#' @param empPool a \code{data.frame} with 40 columns
#'
#'   Each row represents a real employee. The columns are:
#'   \describe{
#'     \item{ID}{character string representing the employee's unique identifier}
#'     \item{name}{character string representing the employee's name}
#'     \item{designation}{character string representing the employee's
#'       designation}
#'     \item{personnelClass}{character string representing the
#'       \code{\link{Employee-class}} sub-class to be applied}
#'     \item{field}{logical value \cr
#'       Is the employee always in the field?}
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
#'       (probationary), \code{'sea'} (seasonal), and \code{'age'} (agency).}
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
#'     \item{JAN}{number of dependents for the month}
#'     \item{FEB}{number of dependents for the month}
#'     \item{MAR}{number of dependents for the month}
#'     \item{APR}{number of dependents for the month}
#'     \item{MAY}{number of dependents for the month}
#'     \item{JUN}{number of dependents for the month}
#'     \item{JUL}{number of dependents for the month}
#'     \item{AUG}{number of dependents for the month}
#'     \item{SEP}{number of dependents for the month}
#'     \item{OCT}{number of dependents for the month}
#'     \item{NOV}{number of dependents for the month}
#'     \item{DEC}{number of dependents for the month}
#'     \item{d.rd_1}{integer value defining how many rest days the employee can
#'       report to work on January}
#'     \item{d.rd_2}{integer value defining how many rest days the employee can
#'       report to work on February}
#'     \item{d.rd_3}{integer value defining how many rest days the employee can
#'       report to work on March}
#'     \item{d.rd_4}{integer value defining how many rest days the employee can
#'       report to work on April}
#'     \item{d.rd_5}{integer value defining how many rest days the employee can
#'       report to work on May}
#'     \item{d.rd_6}{integer value defining how many rest days the employee can
#'       report to work on June}
#'     \item{d.rd_7}{integer value defining how many rest days the employee can
#'       report to work on July}
#'     \item{d.rd_8}{integer value defining how many rest days the employee can
#'       report to work on August}
#'     \item{d.rd_9}{integer value defining how many rest days the employee can
#'       report to work on September}
#'     \item{d.rd_10}{integer value defining how many rest days the employee can
#'       report to work on October}
#'     \item{d.rd_11}{integer value defining how many rest days the employee can
#'       report to work on November}
#'     \item{d.rd_12}{integer value defining how many rest days the employee can
#'       report to work on December}
#'     \item{d.ho_1}{integer value defining how many holidays the employee can
#'       report to work on January}
#'     \item{d.ho_2}{integer value defining how many holidays the employee can
#'       report to work on February}
#'     \item{d.ho_3}{integer value defining how many holidays the employee can
#'       report to work on March}
#'     \item{d.ho_4}{integer value defining how many holidays the employee can
#'       report to work on April}
#'     \item{d.ho_5}{integer value defining how many holidays the employee can
#'       report to work on May}
#'     \item{d.ho_6}{integer value defining how many holidays the employee can
#'       report to work on June}
#'     \item{d.ho_7}{integer value defining how many holidays the employee can
#'       report to work on July}
#'     \item{d.ho_8}{integer value defining how many holidays the employee can
#'       report to work on August}
#'     \item{d.ho_9}{integer value defining how many holidays the employee can
#'       report to work on September}
#'     \item{d.ho_10}{integer value defining how many holidays the employee can
#'       report to work on October}
#'     \item{d.ho_11}{integer value defining how many holidays the employee can
#'       report to work on November}
#'     \item{d.ho_12}{integer value defining how many holidays the employee can
#'       report to work on December}
#'     \item{d.rh_1}{integer value defining how many rest days on holidays the
#'       employee can report to work on January}
#'     \item{d.rh_2}{integer value defining how many rest days on holidays the
#'       employee can report to work on February}
#'     \item{d.rh_3}{integer value defining how many rest days on holidays the
#'       employee can report to work on March}
#'     \item{d.rh_4}{integer value defining how many rest days on holidays the
#'       employee can report to work on April}
#'     \item{d.rh_5}{integer value defining how many rest days on holidays the
#'       employee can report to work on May}
#'     \item{d.rh_6}{integer value defining how many rest days on holidays the
#'       employee can report to work on June}
#'     \item{d.rh_7}{integer value defining how many rest days on holidays the
#'       employee can report to work on July}
#'     \item{d.rh_8}{integer value defining how many rest days on holidays the
#'       employee can report to work on August}
#'     \item{d.rh_9}{integer value defining how many rest days on holidays the
#'       employee can report to work on September}
#'     \item{d.rh_10}{integer value defining how many rest days on holidays the
#'       employee can report to work on October}
#'     \item{d.rh_11}{integer value defining how many rest days on holidays the
#'       employee can report to work on November}
#'     \item{d.rh_12}{integer value defining how many rest days on holidays the
#'       employee can report to work on December}
#'     \item{VL}{numeric value representing the number of vacation leave
#'       credits}
#'     \item{SL}{numeric value representing the number of sick leave
#'       credits}
#'     \item{a_1}{numeric value representing the attendance rate of
#'       employee on January}
#'     \item{a_2}{numeric value representing the attendance rate of
#'       employee on February}
#'     \item{a_3}{numeric value representing the attendance rate of
#'       employee on March}
#'     \item{a_4}{numeric value representing the attendance rate of
#'       employee on April}
#'     \item{a_5}{numeric value representing the attendance rate of
#'       employee on May}
#'     \item{a_6}{numeric value representing the attendance rate of
#'       employee on June}
#'     \item{a_7}{numeric value representing the attendance rate of
#'       employee on July}
#'     \item{a_8}{numeric value representing the attendance rate of
#'       employee on August}
#'     \item{a_9}{numeric value representing the attendance rate of
#'       employee on September}
#'     \item{a_10}{numeric value representing the attendance rate of
#'       employee on October}
#'     \item{a_11}{numeric value representing the attendance rate of
#'       employee on November}
#'     \item{a_12}{numeric value representing the attendance rate of
#'       employee on December}
#'   }
#' @param hol a \code{data.frame} similar to \code{\link{holidays}}
#' @param year integer value representing the year to be budgeted
#' @param forecast logical value \cr
#'   Compute cost for forecast?
#' @return list of of 2:
#'   \enumerate{
#'     \item a list of real employees\cr
#'       Each element of the list is an \code{\link{Employee-class}} object.
#'     \item sanitized empPool
#'   }
#' @export initEmpPool
#' @importFrom dplyr "%>%"
initEmpPool <- function(empPool, hol = NA, year = NA, forecast = FALSE) {

  a_1     <- NULL
  a_2     <- NULL
  a_3     <- NULL
  a_4     <- NULL
  a_5     <- NULL
  a_6     <- NULL
  a_7     <- NULL
  a_8     <- NULL
  a_9     <- NULL
  a_10    <- NULL
  a_11    <- NULL
  a_12    <- NULL
  d.rd_1  <- NULL
  d.rd_2  <- NULL
  d.rd_3  <- NULL
  d.rd_4  <- NULL
  d.rd_5  <- NULL
  d.rd_6  <- NULL
  d.rd_7  <- NULL
  d.rd_8  <- NULL
  d.rd_9  <- NULL
  d.rd_10 <- NULL
  d.rd_11 <- NULL
  d.rd_12 <- NULL
  d.ho_1  <- NULL
  d.ho_2  <- NULL
  d.ho_3  <- NULL
  d.ho_4  <- NULL
  d.ho_5  <- NULL
  d.ho_6  <- NULL
  d.ho_7  <- NULL
  d.ho_8  <- NULL
  d.ho_9  <- NULL
  d.ho_10 <- NULL
  d.ho_11 <- NULL
  d.ho_12 <- NULL
  d.rh_1  <- NULL
  d.rh_2  <- NULL
  d.rh_3  <- NULL
  d.rh_4  <- NULL
  d.rh_5  <- NULL
  d.rh_6  <- NULL
  d.rh_7  <- NULL
  d.rh_8  <- NULL
  d.rh_9  <- NULL
  d.rh_10 <- NULL
  d.rh_11 <- NULL
  d.rh_12 <- NULL

  # Error if any ID is duplicated
  if (anyDuplicated(empPool$ID) > 0) {

    tempData <- empPool$ID[which(duplicated(empPool$ID))]

    for (i in tempData)
      cat(paste("Duplicated pool: ", i, "\n",sep = ""))

    stop(paste("Duplicated ID in pool!"))
  }

  # Remove white spaces (including leading and trailing spaces)
  empPool[, c("name", "personnelClass", "equipment", "costCode", "dcc")] <-
    lapply(
      empPool[, c("name", "personnelClass", "equipment", "costCode", "dcc")],
      FUN = rmWS)

  empPool[, c("inHouse", "isRF", "field")] <-
    lapply(empPool[, c("inHouse", "isRF", "field")], FUN = as.logical)

  # Remove space for status
  empPool$status <- rmS(empPool$status)

  # Remove leading zeroes for purely numeric characters
  empPool[ ,c("costCode", "dcc")] <- lapply(
    X = empPool[, c("costCode", "dcc")], FUN = rmLead0)

  # Convert to lower case
  empPool[, c("personnelClass", "status")] <- lapply(
    empPool[, c("personnelClass", "status")], FUN = tolower)

  # Remove space
  empPool$personnelClass <- gsub(pattern = " ",
                                 replacement = "",
                                 x = empPool$personnelClass)

  # Change to upper case
  empPool[, c("name", "equipment", "costCode", "dcc")] <-
    lapply(empPool[, c("name", "equipment", "costCode", "dcc")], FUN = toupper)

  # Get only the first 3 characters for employee status
  empPool$status <- substr(x = empPool$status, start = 1L, stop = 3L)

  # Check all personnelClass if valid
  if (any(!empPool$personnelClass %in% validEmpClass)) {

    tempIndex <- which(!empPool$personnelClass %in% validEmpClass)

    for (i in tempIndex)
      cat(paste("Invalid personnelClass in: ", empPool$ID[i], ".\n", sep = ""))

    stop("Invalid personnelClass detected!")
  }

  # Check for acceptable attendance
  empPool <- dplyr::mutate(
    empPool,
    attendance = paste(a_1, a_2, a_3, a_4, a_5, a_6,
                   a_7, a_8, a_9, a_10, a_11, a_12)
  )
  attendance <- strsplit(empPool$attendance, split = " ") %>%
    sapply(function(x) {as.numeric(x)}, simplify = FALSE)
  for (i in 1:nrow(empPool)) {
    if (any(is.na(attendance[[i]]))) {
      stop(paste0("Empty or non-numeric attendance in ",
                  empPool$ID[i]), ".")
    }
    if (any(attendance[[i]] > 1)) {
      stop(paste0("Value higher than 1 in ",
                  empPool$ID[i]), ".")
    }
    if (any(attendance[[i]] < 0)) {
      stop(paste0("Value lower than 0 in ",
                  empPool$ID[i]), ".")
    }
  }

  dependentsCol <- month.abb %>% toupper()
  d.rdCol       <- paste0("d.rd_", 1:12)
  d.hoCol       <- paste0("d.ho_", 1:12)
  d.rhCol       <- paste0("d.rh_", 1:12)
  empPool[, c(d.rdCol, d.hoCol, d.rhCol, dependentsCol)] <-
    lapply(empPool[, c(d.rdCol, d.hoCol, d.rhCol, dependentsCol)],
           FUN = as.integer)

  # Pre-allocate employee list
  manPool <- rep(list(NA), times = length(empPool[, 1]))

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

    field <- empPool$field[i]
    if (is.na(field))
      field <- TRUE

    dependents <- c(
      empPool$JAN[i], empPool$FEB[i], empPool$MAR[i], empPool$APR[i],
      empPool$MAY[i], empPool$JUN[i], empPool$JUL[i], empPool$AUG[i],
      empPool$SEP[i], empPool$OCT[i], empPool$NOV[i], empPool$DEC[i]
    )

    attendance <- as.numeric(c(
      empPool$a_1[i], empPool$a_2[i], empPool$a_3[i], empPool$a_4[i],
      empPool$a_5[i], empPool$a_6[i], empPool$a_7[i], empPool$a_8[i],
      empPool$a_9[i], empPool$a_10[i], empPool$a_11[i], empPool$a_12[i]
    ))

    d.rd <- c(
      empPool$d.rd_1[i], empPool$d.rd_2[i], empPool$d.rd_3[i],
      empPool$d.rd_4[i], empPool$d.rd_5[i], empPool$d.rd_6[i],
      empPool$d.rd_7[i], empPool$d.rd_8[i], empPool$d.rd_9[i],
      empPool$d.rd_10[i], empPool$d.rd_11[i], empPool$d.rd_12[i]
    )

    d.ho <- c(
      empPool$d.ho_1[i], empPool$d.ho_2[i], empPool$d.ho_3[i],
      empPool$d.ho_4[i], empPool$d.ho_5[i], empPool$d.ho_6[i],
      empPool$d.ho_7[i], empPool$d.ho_8[i], empPool$d.ho_9[i],
      empPool$d.ho_10[i], empPool$d.ho_11[i], empPool$d.ho_12[i]
    )

    d.rh <- c(
      empPool$d.rh_1[i], empPool$d.rh_2[i], empPool$d.rh_3[i],
      empPool$d.rh_4[i], empPool$d.rh_5[i], empPool$d.rh_6[i],
      empPool$d.rh_7[i], empPool$d.rh_8[i], empPool$d.rh_9[i],
      empPool$d.rh_10[i], empPool$d.rh_11[i], empPool$d.rh_12[i]
    )

    tempEmp <- createEmp(empPool$personnelClass[i])
    tempEmp <- initREmployee(theObject   = tempEmp,
                             ID          = empPool$ID[i],
                             name        = empPool$name[i],
                             designation = empPool$designation[i],
                             attendance  = attendance,
                             costCode    = empPool$costCode[i],
                             status      = empPool$status[i],
                             cBegin      = empPool$cBegin[i],
                             cEnd        = empPool$cEnd[i],
                             inHouse     = empPool$inHouse[i],
                             restday     = empPool$restday[i],
                             hol         = hol,
                             RF          = empPool$isRF[i],
                             equipment   = empPool$equipment[i],
                             d.rd        = d.rd,
                             d.ho        = d.ho,
                             d.rh        = d.rh,
                             dcc         = empPool$dcc[i],
                             forecast    = forecast,
                             field       = field,
                             dependents  = dependents,
                             VL          = empPool$VL[i],
                             SL          = empPool$SL[i])
    manPool[[i]] <- tempEmp
  }

  return(list(manPool, empPool))
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
#'       This only applies to \code{\link{NonStaff-class}} personnel.}
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
#' @return a list of 2:
#'   \enumerate{
#'     \item a list of theoretical employees\cr
#'       Each element of the list is an \code{\link{Employee-class}} object.
#'       Theoretical employees do not have rest days.
#'     \item sanitized empReq
#'   }
#' @export initEmpReq
initEmpReq <- function(empReq, sched, hol = NA, year = NA) {

  # Error if scheduled any scheduled activity is duplicated
  if (anyDuplicated(sched$activity) > 0) {

    tempData <- sched$activity[which(duplicated(sched$activity))]

    for (i in tempData)
      cat(paste("Duplicated sched: ", i, "\n",sep = ""))

    stop(paste("Duplicated activity in schedule!"))
  }

  # Error if assigned activity is not scheduled
  if (!all(unique(empReq$activity) %in% sched$activity)) {

    tempData  <- unique(empReq$activity)
    tempIndex <- which(!tempData %in% sched$activity)

    for (i in tempIndex)
      cat(paste("No schedule: ", tempData[i], "\n", sep = ""))

    stop("Activities without schedules detected!")
  }

  # Warning if scheduled activity has no personnel assigned
  if (!all(sched$activity %in% unique(empReq$activity))) {

    tempData <- sched$activity[!sched$activity %in% unique(empReq$activity)]

    for (i in tempData)
      cat(paste("No personnel assigned: ", i, "\n", sep = ""))

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
  empReq[, c("equipment", "costCode")] <- lapply(
    empReq[, c("equipment", "costCode")],
    FUN = rmS
  )

  # Remove leading zero
  empReq$costCode <- rmLead0(empReq$costCode)

  # Change to lower case
  empReq$personnelClass <- tolower(empReq$personnelClass)

  # Remove space
  empReq$personnelClass <- gsub(pattern = " ",
                                replacement = "",
                                x = empReq$personnelClass)

  # Change to upper case
  empReq[,c("equipment", "costCode")] <- lapply(
    empReq[,c("equipment", "costCode")], FUN = toupper
  )

  # Check accepted employee class
  if (any(!empReq$personnelClass %in% validEmpClass)) {
    tempIndex <- which(!empReq$personnelClass %in% validEmpClass)
    stop(paste("Invalid employee class in rows:", tempIndex))
  }

  # Initialize premium probabilities
  hol     <- getHol(hol = hol, year = year)

  calDays <- getCalDays(cBegin  = paste(year,"-01-01", sep = ""),
                        hol     = hol,
                        restday = "Sunday")

  mdtProb <- getMDTProb(hol = hol)

  # Pre-allocate list of Employee-class objects
  manReq <- rep(list(NA), times = length(empReq[,1]))

  for (i in 1:length(empReq[,1])) {

    schedIndex <- which(sched$activity == empReq$activity[i])
    tempEmp    <- createEmp(empReq$personnelClass[i])

    if (all(is.na(sched[schedIndex,c(2:13)]))) {
      workSched <- NA
    } else {
      workSched                   <- as.integer(sched[schedIndex,c(2:13)])
      workSched[is.na(workSched)] <- 0L
    }

    if (is.na(empReq$spareFactor[i]))
      empReq$spareFactor[i] <- 1

    if (is.na(empReq$OT[i]))
      empReq$OT[i] <- 0

    tempEmp <- initTEmployee(
      theObject   = tempEmp,
      ID          = paste(empReq$activity[i],
                          empReq$personnelClass[i],
                          sep = "-"),
      costCode    = empReq$costCode[i],
      equipment   = empReq$equipment[i],
      OT          = empReq$OT[i],
      calDays     = calDays,
      mdtProb     = mdtProb,
      spareFactor = empReq$spareFactor[i] * empReq$quantity[i],
      monthSched  = workSched
    )

    manReq[[i]] <- tempEmp
  }

  empReq$equipment[is.na(empReq$equipment)] <- ""

  empReq$merged <- paste(empReq$personnelClass,
                         empReq$equipment,
                         empReq$costCode,
                         sep = "-")

  toMerge   <- unique(empReq$merged)

  mergedReq <- lapply(toMerge, FUN = function(x) {

    tempIndex <- which(empReq$merged == x)
    tempEmp   <- mergeEmp(manReq[tempIndex])
    empTab    <- c(tempEmp@ID,
                   tempEmp@costCode,
                   tolower(as.character(class(tempEmp))))

    return(list(empTab, tempEmp))
  })

  empReqMerged           <- t(sapply(mergedReq, FUN = function(x) {x[[1]]}))
  colnames(empReqMerged) <- c("ID", "costCode", "personnelClass")
  empReqMerged           <- as.data.frame(empReqMerged,
                                          stringsAsFactors = FALSE)

  manReqMerged <- lapply(mergedReq, FUN = function(x) {x[[2]]})

  return(list(manReqMerged, empReqMerged))
}
