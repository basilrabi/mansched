#' @import methods
NULL

#' Initialize real employee
#'
#' Initializes employee data and its working hours. This function can be
#'   applied only to real employees. Real employees represent the actual
#'   manpower pool.
#'
#' @param theObject \code{\link{Employee-class}} object
#' @param ID character string representing the employee's unique identifier
#' @param name character vector representing the name of employee
#' @param designation character string representing designation of employee
#' @param attendance numeric value used to account for the absenteeism of the
#'   employee
#'
#'   This may be used as correction factor to estimate the workable man hours of
#'   an employee.
#' @param costCode character vector containing the cost codes wherein the
#'   employee will be charged
#' @param status character string representing the employment status of the
#'   employee
#'
#'   The accepted values are \code{"reg"} (regular), \code{"pro"}
#'   (probationary), and \code{"sea"} (seasonal).
#' @param cBegin character string defining the date wherein the employment
#'   contract of the employee began
#'
#'   The accepted format is \code{"yyyy-mm-dd"}.
#' @param cEnd character string defining the date wherein the employment
#'   contract of the employee will end
#'
#'   The accepted format is \code{"yyyy-mm-dd"}.
#' @param inHouse logical value \cr
#'   Is the employee's accommodation given by the company?
#' @param restday character string defining the day of the week wherein the
#'   employee is not required to report to work
#' @param hol a \code{\link{data.frame}} returned by \code{\link{getHol}}
#'
#'   This contains all the holidays in the year to be budgeted.
#' @param RF logical value \cr
#'   Is the employee rank and file?
#' @param equipment character string representing the equipment types which the
#'   employee was given authority to operate
#'
#'   Equipment types are separated by spaces and must be in upper case.
#' @param OT integer value defining the budgeted overtime hours
#' @param d.rd integer value defining the number of rest days the employee
#'   is budgeted to report to work per month
#' @param d.ho integer value defining the number of exclusive holidays the
#'   employee is budgeted to report to work per month
#' @param d.rh integer value defining the number of rest days during a holiday
#'   the employee is budgeted to report to work per month
#' @param dcc character string defining the cost code wherein the excess regular
#'   man hours of the employee will be charged
#' @param forecast logical value \cr
#'   Compute cost for forecast?
#' @param field logical value \cr
#'   Is the employee always on the field?
#' @param dependents integer vector of length 12 \cr
#'   Number of dependents for each month.
#' @return an \code{\link{Employee-class}} object
#' @importFrom lubridate year
#' @export initREmployee
setGeneric(
  name = "initREmployee",
  def  = function(theObject,
                  ID,
                  name,
                  designation,
                  attendance = rep(1, times = 12),
                  costCode   = "NONE",
                  status     = "reg",
                  cBegin,
                  cEnd       = NA,
                  inHouse    = FALSE,
                  restday    = "Sunday",
                  hol,
                  RF         = FALSE,
                  equipment,
                  OT         = 3,
                  d.rd       = NA,
                  d.ho       = NA,
                  d.rh       = NA,
                  dcc        = "NA",
                  forecast   = FALSE,
                  field      = TRUE,
                  dependents = NA) {
    standardGeneric("initREmployee")
  }
)

# d.rd - If the value is NA, d.rd defaults to 2.
#      - This is based on the assumptions of Mines group.

# d.ho - If the value is NA, d.ho defaults to the following:
#      - a. 0 if regular
#      - b. 5 if non-regular

# d.rh - If the value is NA, d.rd defaults to 0.

#' @describeIn initREmployee Initialize ID, name, designation, attendance,
#'   costCode, status, cBegin, cEnd, inHouse, restday and calDays
#'   (see \code{\link{getCalDays}})
setMethod(
  f          = "initREmployee",
  signature  = "Employee",
  definition = function(theObject,
                        ID,
                        name,
                        designation,
                        attendance = rep(1, times = 12),
                        costCode   = "NONE",
                        status     = "reg",
                        cBegin,
                        cEnd       = NA,
                        inHouse    = FALSE,
                        hol,
                        d.rd       = NA,
                        d.ho       = NA,
                        d.rh       = NA,
                        dcc        = "NA",
                        forecast   = FALSE,
                        field      = TRUE,
                        dependents = NA) {
    # Checking of the validity of all arguments must be done prior to calling
    #  initREmployee()

    theObject@ID          <- ID
    theObject@name        <- name
    theObject@designation <- designation
    theObject@dcc         <- dcc
    theObject@forecast    <- forecast
    theObject@field       <- field

    if (length(dependents) == 12L & is.integer(dependents)) {
      isNA <- is.na(dependents)
      dependents[isNA] <- 0L
      theObject@dependents  <- dependents
    }

    # attendance must be <= 1 and => 0
    theObject@attendance <- attendance

    # Vectorize costCode
    #  This is done under the assumption that white spaces and punctuation are
    #  already removed. Also, all characters are in upper case.
    theObject@costCode <-
      strsplit(x = costCode, split = " ", fixed = TRUE)[[1]]

    # status must be an element of c("reg", "pro", "sea", "age)
    theObject@status <- status
    theObject@cBegin <- cBegin

    year     <- lubridate::year(hol$date[1])
    tempCEnd <- paste(year, "12-31", sep = "-")

    if (is.na(cEnd)) {
      cEnd <- tempCEnd
    } else {
      if (as.Date(cEnd) > as.Date(tempCEnd))
        cEnd <- tempCEnd
    }

    theObject@cEnd <- cEnd

    if (is.na(inHouse)) {
      theObject@inHouse <- FALSE
    } else {
      theObject@inHouse <- inHouse
    }

    calDays <- getCalDays(cBegin  = theObject@cBegin,
                          cEnd    = theObject@cEnd,
                          hol     = hol,
                          restday = theObject@restday)

    if (is.na(d.rd))
      d.rd <- 2L

    calDays$rd <- assignMH(hoursT = calDays$rd, hoursR = d.rd)$hoursA

    if (theObject@status == "reg") {
      if (is.na(d.ho)) {
        d.ho <- 0L
      }
    } else {
      if (is.na(d.ho))
        d.ho <- 5L
    }

    calDays$sh <- assignMH(hoursT = calDays$sh, hoursR = d.ho)$hoursA
    calDays$lh <- assignMH(hoursT = calDays$lh, hoursR = d.ho)$hoursA
    calDays$nh <- assignMH(hoursT = calDays$nh, hoursR = d.ho)$hoursA

    if (is.na(d.rh))
      d.rh <- 0L

    calDays$rs <- assignMH(hoursT = calDays$rs, hoursR = d.rh)$hoursA
    calDays$rl <- assignMH(hoursT = calDays$rl, hoursR = d.rh)$hoursA
    calDays$rn <- assignMH(hoursT = calDays$rn, hoursR = d.rh)$hoursA

    # holDays may differ between reg employees and non-reg employees
    # This must be confirmed with accounting first

    if (theObject@status == "reg") {

      holDays <- apply(calDays[,c("lh", "sh", "nh")], MARGIN = 1, FUN = sum)
      maxRegF <- calDays[,c("reg")]

    } else {

      holDays <- apply(calDays[ ,c("lh", "sh")],  MARGIN = 1, FUN = sum)
      maxRegF <- apply(calDays[ ,c("reg", "nh")], MARGIN = 1, FUN = sum)

    }

    regDays              <- calDays[ ,c("reg")]
    theObject@holHours   <- holDays * 8L
    theObject@maxReg     <- maxRegF * 8L
    theObject@reg        <- as.integer(regDays* 8 * theObject@attendance)

    theObject@leaveHours <- getLeaveHours(cBegin = theObject@cBegin,
                                          cEnd   = theObject@cEnd,
                                          year   = lubridate::year(hol$date[1]),
                                          status = theObject@status)

    return(list(theObject, calDays))
  }
)

#' @describeIn initREmployee Return only the \code{\link{Staff-class}} object
setMethod(
  f          = "initREmployee",
  signature  = "Staff",
  definition = function(theObject,
                        ID,
                        name,
                        designation,
                        attendance = rep(1, times = 12),
                        costCode   = "NONE",
                        status     = "reg",
                        cBegin,
                        cEnd       = NA,
                        inHouse    = FALSE,
                        hol,
                        d.rd       = NA,
                        d.ho       = NA,
                        d.rh       = NA,
                        dcc        = "NA",
                        forecast   = FALSE,
                        field      = TRUE,
                        dependents = NA) {

    # For all staff, assign a rest day of Sunday for computation purposes.
    # This is because in a theortical Staff, the working days do not include
    # Sundays.
    theObject@restday <- "Sunday"

    tempData <- callNextMethod(theObject   = theObject,
                               ID          = ID,
                               name        = name,
                               designation = designation,
                               attendance  = attendance,
                               costCode    = costCode,
                               status      = status,
                               cBegin      = cBegin,
                               cEnd        = cEnd,
                               inHouse     = inHouse,
                               hol         = hol,
                               d.rd        = d.rd,
                               d.ho        = d.ho,
                               d.rh        = d.rh,
                               dcc         = dcc,
                               forecast    = forecast,
                               field       = field,
                               dependents  = dependents)

    return(tempData[[1]])
  }
)

#' @describeIn initREmployee Initialize regOT
setMethod(
  f          = "initREmployee",
  signature  = "NonStaff",
  definition = function(theObject,
                        ID,
                        name,
                        designation,
                        attendance = rep(1, times = 12),
                        costCode   = "NONE",
                        status     = "reg",
                        cBegin,
                        cEnd       = NA,
                        inHouse    = FALSE,
                        hol,
                        OT         = 3,
                        d.rd       = NA,
                        d.ho       = NA,
                        d.rh       = NA,
                        dcc        = "NA",
                        forecast   = FALSE,
                        field      = TRUE,
                        dependents = NA) {

    if (is.na(OT))
      OT <- 3

    tempData <- callNextMethod(theObject   = theObject,
                               ID          = ID,
                               name        = name,
                               designation = designation,
                               attendance  = attendance,
                               costCode    = costCode,
                               status      = status,
                               cBegin      = cBegin,
                               cEnd        = cEnd,
                               inHouse     = inHouse,
                               hol         = hol,
                               d.rd        = d.rd,
                               d.ho        = d.ho,
                               d.rh        = d.rh,
                               dcc         = dcc,
                               forecast    = forecast,
                               field       = field,
                               dependents  = dependents)

    theObject <- tempData[[1]]
    calDays   <- tempData[[2]]

    theObject@regOT <- as.integer(
      calDays[,c("reg")] * theObject@attendance * OT
    )

    return(list(theObject, calDays))
  }
)

#' @describeIn initREmployee Initialize RF and return only the
#'   \code{\link{Clerk-class}} object
setMethod(
  f          = "initREmployee",
  signature  = "Clerk",
  definition = function(theObject,
                        ID,
                        name,
                        designation,
                        attendance = rep(1, times = 12),
                        costCode   = "NONE",
                        status     = "reg",
                        cBegin,
                        cEnd       = NA,
                        inHouse    = FALSE,
                        hol,
                        RF         = FALSE,
                        OT         = 3,
                        d.rd       = NA,
                        d.ho       = NA,
                        d.rh       = NA,
                        dcc        = "NA",
                        forecast   = FALSE,
                        field      = TRUE,
                        dependents = NA) {

    # For all clerk, assign a rest day of Sunday for computation purposes.
    # This is because in a theortical clerk, the working days do not include
    # Sundays.
    theObject@restday <- "Sunday"

    if (is.na(OT))
      OT <- 3

    if (is.na(RF))
      RF <- FALSE

    tempData <- callNextMethod(theObject   = theObject,
                               ID          = ID,
                               name        = name,
                               designation = designation,
                               attendance  = attendance,
                               costCode    = costCode,
                               status      = status,
                               cBegin      = cBegin,
                               cEnd        = cEnd,
                               inHouse     = inHouse,
                               hol         = hol,
                               OT          = OT,
                               d.rd        = d.rd,
                               d.ho        = d.ho,
                               d.rh        = d.rh,
                               dcc         = dcc,
                               forecast    = forecast,
                               field       = field,
                               dependents  = dependents)

    theObject <- tempData[[1]]
    calDays   <- tempData[[2]]

    theObject@isRF <- RF

    if (theObject@isRF) {

      if (theObject@status == "reg") {

        holDays <- apply(
          calDays[, colnames(calDays) %in% c("lh",
                                             "sh",
                                             "nh",
                                             "rl",
                                             "rs",
                                             "rn")],
          MARGIN = 1,
          FUN = sum
        )

      } else {

        holDays <- apply(
          calDays[, colnames(calDays) %in% c("lh", "rl")],
          MARGIN = 1,
          FUN    = sum
        )

      }

      theObject@holHours <- holDays * 8L
    }

    if (theObject@status != "reg") {
      theObject@reg <- theObject@reg + as.integer(
        calDays[,c("nh")] * theObject@attendance * 8
      )

      theObject@regOT <- theObject@regOT + as.integer(
        calDays[,c("nh")] * theObject@attendance * OT
      )
    }

    return(theObject)
  }
)

#' @describeIn initREmployee Initialize rd, rdOT, sh, shOT, lh, lhOT, nh, nhOT,
#'   rs, rsOT, rl, rlOT, rn, rnOT. Return only the
#'   \code{\link{OperationPersonnel-class}} object.
setMethod(
  f          = "initREmployee",
  signature  = "OperationPersonnel",
  definition = function(theObject,
                        ID,
                        name,
                        designation,
                        attendance = rep(1, times = 12),
                        costCode   = "NONE",
                        status     = "reg",
                        cBegin,
                        cEnd       = NA,
                        inHouse    = FALSE,
                        restday    = "Sunday",
                        hol,
                        OT         = 3,
                        d.rd       = NA,
                        d.ho       = NA,
                        d.rh       = NA,
                        dcc        = "NA",
                        forecast   = FALSE,
                        field      = TRUE,
                        dependents = NA) {

    # restday must be a valid weekday prior to calling initEmployee
    if (is.na(restday))
      restday <- "Sunday"

    theObject@restday <- restday

    if (is.na(OT))
      OT <- 3

    tempData <- callNextMethod(theObject   = theObject,
                               ID          = ID,
                               name        = name,
                               designation = designation,
                               attendance  = attendance,
                               costCode    = costCode,
                               status      = status,
                               cBegin      = cBegin,
                               cEnd        = cEnd,
                               inHouse     = inHouse,
                               restday     = restday,
                               hol         = hol,
                               OT          = OT,
                               d.rd        = d.rd,
                               d.ho        = d.ho,
                               d.rh        = d.rh,
                               dcc         = dcc,
                               forecast    = forecast,
                               field       = field,
                               dependents  = dependents)

    theObject <- tempData[[1]]
    calDays   <- tempData[[2]]

    if ("rd" %in% colnames(calDays)) {
      theObject@rd <- as.integer(
        calDays[,c("rd")] * theObject@attendance * 8
      )
      theObject@rdOT <- as.integer(
        calDays[,c("rd")] * theObject@attendance * OT
      )
    }

    if ("sh" %in% colnames(calDays)) {
      theObject@sh <- as.integer(
        calDays[,c("sh")] * theObject@attendance * 8
      )
      theObject@shOT <- as.integer(
        calDays[,c("sh")] * theObject@attendance * OT
      )
    }

    if ("lh" %in% colnames(calDays)) {
      theObject@lh <- as.integer(
        calDays[,c("lh")] * theObject@attendance * 8
      )
      theObject@lhOT <- as.integer(
        calDays[,c("lh")] * theObject@attendance * OT
      )
    }

    if ("nh" %in% colnames(calDays)) {
      theObject@nh <- as.integer(
        calDays[,c("nh")] * theObject@attendance * 8
      )
      theObject@nhOT <- as.integer(
        calDays[,c("nh")] * theObject@attendance * OT
      )
    }

    if ("rs" %in% colnames(calDays)) {
      theObject@rs <- as.integer(
        calDays[,c("rs")] * theObject@attendance * 8
      )
      theObject@rsOT <- as.integer(
        calDays[,c("rs")] * theObject@attendance * OT
      )
    }

    if ("rl" %in% colnames(calDays)) {
      theObject@rl <- as.integer(
        calDays[,c("rl")] * theObject@attendance * 8
      )
      theObject@rlOT <- as.integer(
        calDays[,c("rl")] * theObject@attendance * OT
      )
    }

    if ("rn" %in% colnames(calDays)) {
      theObject@rn <- as.integer(
        calDays[,c("rn")] * theObject@attendance * 8
      )
      theObject@rnOT <- as.integer(
        calDays[,c("rn")] * theObject@attendance * OT
      )
    }

    return(list(theObject, calDays))
  }
)

#' @describeIn initREmployee Return only the \code{\link{Technical-class}}
#'   object. All \code{Technical-class} personnel are not rank and file.
setMethod(
  f = "initREmployee",
  signature = "Technical",
  definition = function(theObject,
                        ID,
                        name,
                        designation,
                        attendance = rep(1, times = 12),
                        costCode   = "NONE",
                        status     = "reg",
                        cBegin,
                        cEnd       = NA,
                        inHouse    = FALSE,
                        restday    = "Sunday",
                        hol,
                        OT         = 3,
                        d.rd       = NA,
                        d.ho       = NA,
                        d.rh       = NA,
                        dcc        = "NA",
                        forecast   = FALSE,
                        field      = TRUE,
                        dependents = NA) {

    if (is.na(OT))
      OT <- 3

    tempData <- callNextMethod(theObject   = theObject,
                               ID          = ID,
                               name        = name,
                               designation = designation,
                               attendance  = attendance,
                               costCode    = costCode,
                               status      = status,
                               cBegin      = cBegin,
                               cEnd        = cEnd,
                               inHouse     = inHouse,
                               restday     = restday,
                               hol         = hol,
                               OT          = OT,
                               d.rd        = d.rd,
                               d.ho        = d.ho,
                               d.rh        = d.rh,
                               dcc         = dcc,
                               forecast    = forecast,
                               field       = field,
                               dependents  = dependents)

    theObject <- tempData[[1]]

    theObject@isRF <- FALSE
    return(theObject)
  }
)

#' @describeIn initREmployee Return only the
#'   \code{\link{Supervisor-class}} object. All \code{Supervisor-class}
#'   employees are not rank and file
setMethod(
  f = "initREmployee",
  signature = "Supervisor",
  definition = function(theObject,
                        ID,
                        name,
                        designation,
                        attendance = rep(1, times = 12),
                        costCode   = "NONE",
                        status     = "reg",
                        cBegin,
                        cEnd       = NA,
                        inHouse    = FALSE,
                        restday    = "Sunday",
                        hol,
                        OT         = 3,
                        d.rd       = NA,
                        d.ho       = NA,
                        d.rh       = NA,
                        dcc        = "NA",
                        forecast   = FALSE,
                        field      = TRUE,
                        dependents = NA) {

    if (is.na(OT))
      OT <- 3

    tempData <- callNextMethod(theObject   = theObject,
                               ID          = ID,
                               name        = name,
                               designation = designation,
                               attendance  = attendance,
                               costCode    = costCode,
                               status      = status,
                               cBegin      = cBegin,
                               cEnd        = cEnd,
                               inHouse     = inHouse,
                               restday     = restday,
                               hol         = hol,
                               OT          = OT,
                               d.rd        = d.rd,
                               d.ho        = d.ho,
                               d.rh        = d.rh,
                               dcc         = dcc,
                               forecast    = forecast,
                               field       = field,
                               dependents  = dependents)

    theObject      <- tempData[[1]]
    theObject@isRF <- FALSE

    return(theObject)
  }
)

#' @describeIn initREmployee Return only the
#'   \code{\link{Laborer-class}} object. All \code{Laborer-class}
#'   employees are rank and file
setMethod(
  f = "initREmployee",
  signature = "Laborer",
  definition = function(theObject,
                        ID,
                        name,
                        designation,
                        attendance = rep(1, times = 12),
                        costCode   = "NONE",
                        status     = "reg",
                        cBegin,
                        cEnd       = NA,
                        inHouse    = FALSE,
                        restday    = "Sunday",
                        hol,
                        OT         = 3,
                        d.rd       = NA,
                        d.ho       = NA,
                        d.rh       = NA,
                        dcc        = "NA",
                        forecast   = FALSE,
                        field      = TRUE,
                        dependents = NA) {

    if (is.na(OT))
      OT <- 3

    tempData <- callNextMethod(theObject   = theObject,
                               ID          = ID,
                               name        = name,
                               designation = designation,
                               attendance  = attendance,
                               costCode    = costCode,
                               status      = status,
                               cBegin      = cBegin,
                               cEnd        = cEnd,
                               inHouse     = inHouse,
                               restday     = restday,
                               hol         = hol,
                               OT          = OT,
                               d.rd        = d.rd,
                               d.ho        = d.ho,
                               d.rh        = d.rh,
                               dcc         = dcc,
                               forecast    = forecast,
                               field       = field,
                               dependents  = dependents)

    theObject      <- tempData[[1]]
    calDays        <- tempData[[2]]
    theObject@isRF <- TRUE

    if (theObject@status == "reg") {
      holDays <- apply(
        calDays[, colnames(calDays) %in% c("lh", "sh", "nh", "rl", "rs", "rn")],
        MARGIN = 1,
        FUN    = sum
      )
    } else {
      holDays <- apply(
        calDays[, colnames(calDays) %in% c("lh", "rl")], MARGIN = 1, FUN = sum)
    }

    theObject@holHours <- holDays * 8L

    return(theObject)
  }
)

#' @describeIn initREmployee Initialize equipment
#'
#' \code{\link{Operator-class}} personnel may be authorized to operate multiple
#'   types of equipment. In Taganito Mining Corporation, some of the equipment
#'   types are listed in \code{\link{validEquipment}}.
#'
#'   All \code{Operator-class} employees are rank and file.
setMethod(
  f = "initREmployee",
  signature = "Operator",
  definition = function(theObject,
                        ID,
                        name,
                        designation,
                        attendance = rep(1, times = 12),
                        costCode   = "NONE",
                        status     = "reg",
                        cBegin,
                        cEnd       = NA,
                        inHouse    = FALSE,
                        restday    = "Sunday",
                        hol,
                        equipment,
                        OT         = 3,
                        d.rd       = NA,
                        d.ho       = NA,
                        d.rh       = NA,
                        dcc        = "NA",
                        forecast   = FALSE,
                        field      = TRUE,
                        dependents = NA) {

    if (is.na(OT))
      OT <- 3

    tempData <- callNextMethod(theObject   = theObject,
                               ID          = ID,
                               name        = name,
                               designation = designation,
                               attendance  = attendance,
                               costCode    = costCode,
                               status      = status,
                               cBegin      = cBegin,
                               cEnd        = cEnd,
                               inHouse     = inHouse,
                               restday     = restday,
                               hol         = hol,
                               OT          = OT,
                               d.rd        = d.rd,
                               d.ho        = d.ho,
                               d.rh        = d.rh,
                               dcc         = dcc,
                               forecast    = forecast,
                               field       = field,
                               dependents  = dependents)

    theObject      <- tempData[[1]]
    calDays        <- tempData[[2]]
    theObject@isRF <- TRUE

    if (theObject@status == "reg") {

      holDays <- apply(
        calDays[, colnames(calDays) %in% c("lh", "sh", "nh", "rl", "rs", "rn")],
        MARGIN = 1,
        FUN = sum
      )
    } else {
      holDays <- apply(
        calDays[, colnames(calDays) %in% c("lh", "rl")], MARGIN = 1, FUN = sum)
    }

    theObject@holHours <- holDays * 8L

    if (is.na(equipment))
      stop("No equipment authorized")

    # Vectorize equipment
    equipment <- strsplit(x = equipment, split = " ", fixed = TRUE)[[1]]
    equipment <- unique(equipment)

    if (all(equipment %in% validEquipment)) {
      theObject@equipment <- equipment
    } else {
      stop(paste("Invalid equipment at", theObject@ID, sep = " "))
    }

    return(theObject)
  }
)

#' Initialize theoretical employee
#'
#' Initialize employee data and its working hours. This function can be applied
#'   only to theoretical employees. Theoretical employees represent the manpower
#'   requirement for a scheduled activity.
#'
#' @param theObject \code{\link{Employee-class}} object
#' @param ID character string representing the employee's unique identifier
#' @param costCode character string representing the accounting cost code
#'   wherein the employee's man hour cost will be charged
#' @param equipment character string representing the equipment type which the
#'   employee was given authority to operate
#' @param OT integer value defining the budgeted overtime hours
#' @param calDays a \code{\link{data.frame}} returned by
#'   \code{\link{getCalDays}}
#' @param mdtProb a \code{data.frame} returned by \code{\link{getMDTProb}}
#' @param spareFactor numeric value of greater than or equal to 1 used as man
#'   hours multiplier
#' @param monthSched integer vector of length 12
#'
#'   This represents the number of days an activity is scheduled for each month.
#'   This argument is used only for \code{\link{ProductionPersonnel-class}}
#'   objects.
#' @return \code{\link{Employee-class}} object
#' @export initTEmployee
setGeneric(
  name = "initTEmployee",
  def  = function(theObject,
                  ID,
                  costCode,
                  equipment,
                  OT          = 0,
                  calDays,
                  mdtProb,
                  spareFactor = 1,
                  monthSched  = NA) {
    standardGeneric("initTEmployee")
  }
)

#' @describeIn initTEmployee Initialize ID, costCode and spareFactor
setMethod(
  f          = "initTEmployee",
  signature  = "Employee",
  definition = function(theObject,
                        ID,
                        costCode,
                        spareFactor = 1) {
    # Checking of the validity of all arguments must be done prior to calling
    #  initTEmployee()

    if (is.na(spareFactor))
      spareFactor <- 1

    theObject@ID          <- ID
    theObject@costCode    <- costCode
    theObject@spareFactor <- spareFactor

    return(theObject)
  }
)

#' @describeIn initTEmployee Initialize reg
setMethod(
  f          = "initTEmployee",
  signature  = "Staff",
  definition = function(theObject,
                        ID,
                        costCode,
                        calDays,
                        spareFactor = 1) {

    if (is.na(spareFactor))
      spareFactor <- 1

    theObject <- callNextMethod(theObject   = theObject,
                                ID          = ID,
                                costCode    = costCode,
                                spareFactor = spareFactor)

    theObject@reg <- as.integer(calDays[,c("reg")] * 8 * theObject@spareFactor)

    return(theObject)
  }
)

#' @describeIn initTEmployee Initialize reg and regOT
setMethod(
  f          = "initTEmployee",
  signature  = "Clerk",
  definition = function(theObject,
                        ID,
                        costCode,
                        OT          = 0,
                        calDays,
                        spareFactor = 1) {

    if (is.na(OT))
      OT <- 0

    if (is.na(spareFactor))
      spareFactor <- 1

    theObject <- callNextMethod(theObject   = theObject,
                                ID          = ID,
                                costCode    = costCode,
                                spareFactor = spareFactor)

    theObject@reg   <- as.integer(calDays[,c("reg")] * 8 *
                                    theObject@spareFactor)

    theObject@regOT <- as.integer(calDays[,c("reg")] * OT *
                                    theObject@spareFactor)

    return(theObject)
  }
)

#' @describeIn initTEmployee Initialize all man hour type using man day type
#'   probabilities
setMethod(
  f          = "initTEmployee",
  signature  = "OperationPersonnel",
  definition = function(theObject,
                        ID,
                        costCode,
                        OT          = 0,
                        mdtProb,
                        spareFactor = 1,
                        monthSched  = NA) {

    if (is.na(OT))
      OT <- 0

    if (is.na(spareFactor))
      spareFactor <- 1

    theObject <- callNextMethod(theObject   = theObject,
                                ID          = ID,
                                costCode    = costCode,
                                spareFactor = spareFactor)

    d.reg <- mdtProb$reg + mdtProb$rd
    d.sh  <- mdtProb$sh  + mdtProb$rs
    d.lh  <- mdtProb$lh  + mdtProb$rl
    d.nh  <- mdtProb$nh  + mdtProb$rn

    reg <- d.reg * theObject@spareFactor * 8
    sh  <- d.sh  * theObject@spareFactor * 8
    lh  <- d.lh  * theObject@spareFactor * 8
    nh  <- d.nh  * theObject@spareFactor * 8

    regOT <- d.reg * theObject@spareFactor * OT
    shOT  <- d.sh  * theObject@spareFactor * OT
    lhOT  <- d.lh  * theObject@spareFactor * OT
    nhOT  <- d.nh  * theObject@spareFactor * OT

    if (any(is.na(monthSched))) {

      theObject@reg <- as.integer(reg * mdtProb$days + 0.5)
      theObject@sh  <- as.integer(sh  * mdtProb$days + 0.5)
      theObject@lh  <- as.integer(lh  * mdtProb$days + 0.5)
      theObject@nh  <- as.integer(nh  * mdtProb$days + 0.5)

      theObject@regOT <- as.integer(regOT * mdtProb$days + 0.5)
      theObject@shOT  <- as.integer(shOT  * mdtProb$days + 0.5)
      theObject@lhOT  <- as.integer(lhOT  * mdtProb$days + 0.5)
      theObject@nhOT  <- as.integer(nhOT  * mdtProb$days + 0.5)

    } else {

      theObject@reg <- as.integer(reg * monthSched + 0.5)
      theObject@sh  <- as.integer(sh  * monthSched + 0.5)
      theObject@lh  <- as.integer(lh  * monthSched + 0.5)
      theObject@nh  <- as.integer(nh  * monthSched + 0.5)

      theObject@regOT <- as.integer(regOT * monthSched + 0.5)
      theObject@shOT  <- as.integer(shOT  * monthSched + 0.5)
      theObject@lhOT  <- as.integer(lhOT  * monthSched + 0.5)
      theObject@nhOT  <- as.integer(nhOT  * monthSched + 0.5)

    }

    return(theObject)
  }
)

#' @describeIn initTEmployee Initialize equipment
setMethod(
  f          = "initTEmployee",
  signature  = "Operator",
  definition = function(theObject,
                        ID,
                        costCode,
                        equipment,
                        OT          = 0,
                        mdtProb,
                        spareFactor = 1,
                        monthSched  = NA) {

    if (is.na(OT))
      OT <- 0

    if (is.na(spareFactor))
      spareFactor <- 1

    theObject <- callNextMethod(theObject   = theObject,
                                ID          = ID,
                                costCode    = costCode,
                                OT          = OT,
                                mdtProb     = mdtProb,
                                spareFactor = spareFactor,
                                monthSched  = monthSched)

    if (is.na(equipment))
      stop("No equipment assigned")

    if (equipment %in% validEquipment) {
      theObject@equipment <- equipment
    } else {
      stop(paste("Invalid equipment at", theObject@ID, sep = " "))
    }

    return(theObject)
  }
)

#' Create an employee
#'
#' Creates an \code{\link{Employee-class}} object using a switch.
#'
#' @param empClass character string representing the employee sub-class
#'
#'   Only the end nodes are accepted (see vignette).
#' @return an object that is a sub-class of \code{\link{Employee-class}}
#' @export createEmp
createEmp <- function(empClass) {
  switch(empClass,
         "divisionmanager"   = division_manager(),
         "groupmanager"      = group_manager(),
         "departmentmanager" = department_manager(),
         "sectionhead"       = section_head(),
         "clerk"             = clerk(),
         "technical"         = technical(),
         "supervisor"        = supervisor(),
         "laborer"           = laborer(),
         "operator"          = operator())
}
