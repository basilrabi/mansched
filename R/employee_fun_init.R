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
#'   contract of the emplyee will end
#'
#'   The accepted format is \code{"yyyy-mm-dd"}.
#' @param inHouse logical value \cr
#'   Is the employee's accommodation given by the company?
#' @param restday character string defining the day of the week wherein the
#'   employee is not required to report to work
#' @param hol a \code{\link{data.frame}} returned by \code{\link{getHol}}
#'
#'   This contains all the holidays in the year to be budgeted.
#' @param isRF logical value \cr
#'   Is the employee rank and file?
#' @param equipment character string representing the equipment types which the
#'   employee was given authority to operate
#'
#'   Equipment types are separated by spaces and must be in upper case.
#' @param OT integer value defining the budgeted overtime hours
#' @importFrom lubridate year
#' @export initREmployee
setGeneric(
  name = "initREmployee",
  def = function(theObject,
                 ID,
                 name,
                 designation,
                 attendance = 1,
                 costCode = "NONE",
                 status = "reg",
                 cBegin,
                 cEnd = NA,
                 inHouse = FALSE,
                 restday = "Sunday",
                 hol,
                 isRF = FALSE,
                 equipment,
                 OT = 3) {
    standardGeneric("initREmployee")
  }
)

#' @describeIn initREmployee Initialize ID, name, designation, attendance,
#'   costCode, status, cBegin, cEnd, inHouse, restday and calDays
#'   (see \code{\link{getCalDays}})
setMethod(
  f = "initREmployee",
  signature = "Employee",
  definition = function(theObject,
                        ID,
                        name,
                        designation,
                        attendance = 1,
                        costCode = "NONE",
                        status = "reg",
                        cBegin,
                        cEnd = NA,
                        inHouse = FALSE,
                        restday = "Sunday",
                        hol) {
    # Checking of the validity of all arguments must be done prior to calling
    #  initREmployee()

    theObject@ID <- ID
    theObject@name <- name
    theObject@designation <- designation

    # attendance must be <= 1 but preferrabbly > 0.5
    theObject@attendance <- attendance

    # Vectorize costCode
    #  This is done under the assumption that white spaces and punctuations are
    #  already removed. Also, all characters are in upper case.
    theObject@costCode <-
      strsplit(x = costCode, split = " ", fixed = TRUE)[[1]]

    # status must be an element of c("reg", "pro", "sea")
    theObject@status <- status

    theObject@cBegin <- cBegin

    if (is.na(cEnd)) {
      year <- lubridate::year(hol$date[1])
      cEnd <- paste(year, "12-31", sep = "-")
    }

    theObject@cEnd <- cEnd
    theObject@inHouse <- inHouse

    # restday must be a valid weekday prior to calling initEmployee
    theObject@restday <- restday

    calDays <- getCalDays(cBegin = theObject@cBegin,
                          cEnd = theObject@cEnd,
                          hol = hol,
                          restday = theObject@restday)

    # totDays and holDays may differ between reg employees and non-reg employees
    # This must be confirmed with accounting first

    totDays <- apply(calDays[,c("reg", "lh", "sh", "nh")],
                     MARGIN = 1, FUN = sum)

    if (theObject@status == "reg") {
      holDays <- apply(calDays[,c("lh", "sh", "nh")], MARGIN = 1, FUN = sum)
      regDays <- calDays[,c("reg")]
    } else {
      holDays <- calDays[,c("lh")]
      regDays <- apply(calDays[,c("reg", "sh", "nh")], MARGIN = 1, FUN = sum)
    }

    theObject@totHours <- totDays * 8L
    theObject@holHours <- holDays * 8L
    theObject@maxReg <- regDays * 8L

    regDays <- calDays[,c("reg")]
    theObject@reg <- as.integer(regDays* 8 * theObject@attendance)

    return(list(theObject, calDays))
  }
)

#' @describeIn initREmployee Return only the \code{\link{Staff-class}} object
setMethod(
  f = "initREmployee",
  signature = "Staff",
  definition = function(theObject,
                        ID,
                        name,
                        designation,
                        attendance = 1,
                        costCode = "NONE",
                        status = "reg",
                        cBegin,
                        cEnd = NA,
                        inHouse = FALSE,
                        restday = "Sunday",
                        hol) {

    tempData <- callNextMethod(theObject = theObject,
                               ID = ID,
                               name = name,
                               designation = designation,
                               attendance = attendance,
                               costCode = costCode,
                               status = status,
                               cBegin = cBegin,
                               cEnd = cEnd,
                               inHouse = inHouse,
                               restday = restday,
                               hol = hol)
    return(tempData[[1]])
  }
)

#' @describeIn initREmployee Initialize regOT
setMethod(
  f = "initREmployee",
  signature = "Non Staff",
  definition = function(theObject,
                        ID,
                        name,
                        designation,
                        attendance = 1,
                        costCode = "NONE",
                        status = "reg",
                        cBegin,
                        cEnd = NA,
                        inHouse = FALSE,
                        restday = "Sunday",
                        hol,
                        OT = 3) {

    tempData <- callNextMethod(theObject = theObject,
                               ID = ID,
                               name = name,
                               designation = designation,
                               attendance = attendance,
                               costCode = costCode,
                               status = status,
                               cBegin = cBegin,
                               cEnd = cEnd,
                               inHouse = inHouse,
                               restday = restday,
                               hol = hol)
    theObject <- tempData[[1]]
    calDays <- tempData[[2]]

    theObject@regOT <- as.integer(
      calDays[,c("reg")] * theObject@attendance * OT
    )

    return(list(theObject, calDays))
  }
)

#' @describeIn initREmployee Initialize isRF and return only the
#'   \code{\link{Clerk-class}} object
setMethod(
  f = "initREmployee",
  signature = "Clerk",
  definition = function(theObject,
                        ID,
                        name,
                        designation,
                        attendance = 1,
                        costCode = "NONE",
                        status = "reg",
                        cBegin,
                        cEnd = NA,
                        inHouse = FALSE,
                        restday = "Sunday",
                        hol,
                        isRF = FALSE,
                        OT = 3) {
    theObject <- callNextMethod(theObject = theObject,
                                ID = ID,
                                name = name,
                                designation = designation,
                                attendance = attendance,
                                costCode = costCode,
                                status = status,
                                cBegin = cBegin,
                                cEnd = cEnd,
                                inHouse = inHouse,
                                restday = restday,
                                hol = hol,
                                OT = OT)[[1]]

    theObject@isRF <- isRF

    return(theObject)
  }
)

#' @describeIn initREmployee Initialize rd, rdOT, sh, shOT, lh, lhOT, nh, nhOT,
#'   rs, rsOT, rl, rlOT, rn, rnOT. Return only the
#'   \code{\link{Operation Personnel-class}} object.
setMethod(
  f = "initREmployee",
  signature = "Operation Personnel",
  definition = function(theObject,
                        ID,
                        name,
                        designation,
                        attendance = 1,
                        costCode = "NONE",
                        status = "reg",
                        cBegin,
                        cEnd = NA,
                        inHouse = FALSE,
                        restday = "Sunday",
                        hol,
                        OT = 3) {
    tempData <- callNextMethod(theObject = theObject,
                               ID = ID,
                               name = name,
                               designation = designation,
                               attendance = attendance,
                               costCode = costCode,
                               status = status,
                               cBegin = cBegin,
                               cEnd = cEnd,
                               inHouse = inHouse,
                               restday = restday,
                               hol = hol,
                               OT = OT)
    theObject <- tempData[[1]]
    calDays <- tempData[[2]]

    zero <- rep(0L, times = 12)

    if ("rd" %in% colnames(calDays)) {
      theObject@rd <- as.integer(
        calDays[,c("rd")] * theObject@attendance * 8
      )
      theObject@rdOT <- as.integer(
        calDays[,c("rd")] * theObject@attendance * OT
      )
    } else {
      theObject@rd <- zero
      theObject@rdOT <- zero
    }

    if ("sh" %in% colnames(calDays)) {
      theObject@sh <- as.integer(
        calDays[,c("sh")] * theObject@attendance * 8
      )
      theObject@shOT <- as.integer(
        calDays[,c("sh")] * theObject@attendance * OT
      )
    } else {
      theObject@sh <- zero
      theObject@shOT <- zero
    }

    if ("lh" %in% colnames(calDays)) {
      theObject@lh <- as.integer(
        calDays[,c("lh")] * theObject@attendance * 8
      )
      theObject@lhOT <- as.integer(
        calDays[,c("lh")] * theObject@attendance * OT
      )
    } else {
      theObject@lh <- zero
      theObject@lhOT <- zero
    }

    if ("nh" %in% colnames(calDays)) {
      theObject@nh <- as.integer(
        calDays[,c("nh")] * theObject@attendance * 8
      )
      theObject@nhOT <- as.integer(
        calDays[,c("nh")] * theObject@attendance * OT
      )
    } else {
      theObject@nh <- zero
      theObject@nhOT <- zero
    }

    if ("rs" %in% colnames(calDays)) {
      theObject@rs <- as.integer(
        calDays[,c("rs")] * theObject@attendance * 8
      )
      theObject@rsOT <- as.integer(
        calDays[,c("rs")] * theObject@attendance * OT
      )
    } else {
      theObject@rs <- zero
      theObject@rsOT <- zero
    }

    if ("rl" %in% colnames(calDays)) {
      theObject@rl <- as.integer(
        calDays[,c("rl")] * theObject@attendance * 8
      )
      theObject@rlOT <- as.integer(
        calDays[,c("rl")] * theObject@attendance * OT
      )
    } else {
      theObject@rl <- zero
      theObject@rlOT <- zero
    }

    if ("rn" %in% colnames(calDays)) {
      theObject@rn <- as.integer(
        calDays[,c("rn")] * theObject@attendance * 8
      )
      theObject@rnOT <- as.integer(
        calDays[,c("rn")] * theObject@attendance * OT
      )
    } else {
      theObject@rn <- zero
      theObject@rnOT <- zero
    }

    return(theObject)
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
                        attendance = 1,
                        costCode = "NONE",
                        status = "reg",
                        cBegin,
                        cEnd = NA,
                        inHouse = FALSE,
                        restday = "Sunday",
                        hol,
                        OT = 3) {

    theObject <- callNextMethod(theObject = theObject,
                                ID = ID,
                                name = name,
                                designation = designation,
                                attendance = attendance,
                                costCode = costCode,
                                status = status,
                                cBegin = cBegin,
                                cEnd = cEnd,
                                inHouse = inHouse,
                                restday = restday,
                                hol = hol,
                                OT = OT)

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
                        attendance = 1,
                        costCode = "NONE",
                        status = "reg",
                        cBegin,
                        cEnd = NA,
                        inHouse = FALSE,
                        restday = "Sunday",
                        hol,
                        OT = 3) {

    theObject <- callNextMethod(theObject = theObject,
                                ID = ID,
                                name = name,
                                designation = designation,
                                attendance = attendance,
                                costCode = costCode,
                                status = status,
                                cBegin = cBegin,
                                cEnd = cEnd,
                                inHouse = inHouse,
                                restday = restday,
                                hol = hol,
                                OT = OT)
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
                        attendance = 1,
                        costCode = "NONE",
                        status = "reg",
                        cBegin,
                        cEnd = NA,
                        inHouse = FALSE,
                        restday = "Sunday",
                        hol,
                        OT = 3) {

    theObject <- callNextMethod(theObject = theObject,
                                ID = ID,
                                name = name,
                                designation = designation,
                                attendance = attendance,
                                costCode = costCode,
                                status = status,
                                cBegin = cBegin,
                                cEnd = cEnd,
                                inHouse = inHouse,
                                restday = restday,
                                hol = hol,
                                OT = OT)
    theObject@isRF <- TRUE
    return(theObject)
  }
)

#' @describeIn initREmployee Initialize equipment
#'
#' \code{\link{Operator-class}} personnel may be authorized to operate multiple
#'   types of equipment. In Taganito Mining Corporation, some of the equipment
#'   types are:
#'   \enumerate{
#'     \item CRANE
#'     \item CT (bulldozer)
#'     \item DT (dump truck)
#'     \item FL (fuel truck)
#'     \item FORKLIFT
#'     \item RG (road grader)
#'     \item SB (service bus)
#'     \item SP (service pickup)
#'     \item TT (trailer truck)
#'     \item TX (tracked excavator)
#'     \item VC (vibrating compactor)
#'     \item WL (wheel loader)
#'     \item WTL (water truck)
#'     \item WX (wheeled excavator)
#'   }
#'
#'   All \code{Operator-class} employees are rank and file.
setMethod(
  f = "initREmployee",
  signature = "Operator",
  definition = function(theObject,
                        ID,
                        name,
                        designation,
                        attendance = 1,
                        costCode = "NONE",
                        status = "reg",
                        cBegin,
                        cEnd = NA,
                        inHouse = FALSE,
                        restday = "Sunday",
                        hol,
                        equipment,
                        OT = 3) {

    theObject <- callNextMethod(theObject = theObject,
                                ID = ID,
                                name = name,
                                designation = designation,
                                attendance = attendance,
                                costCode = costCode,
                                status = status,
                                cBegin = cBegin,
                                cEnd = cEnd,
                                inHouse = inHouse,
                                restday = restday,
                                hol = hol,
                                OT =OT)

    theObject@isRF <- TRUE

    if (is.na(equipment))
      stop("No equipment authorized")

    validEquipment <- c("CRANE",
                        "CT",  # dozer
                        "DT",  # dump truck
                        "FL",  # fuel truck
                        "FORKLIFT",
                        "RG",  # road grader
                        "SB",  # service bus
                        "SP",  # service pickup
                        "TT",  # trailer truck, prime mover
                        "TX",  # tracked excavator
                        "VC",  # vibrating compactor
                        "WL",  # wheel loader
                        "WTL", # water truck
                        "WX")  # wheeled excavator

    # Vectorize equipment
    equipment <- strsplit(x = equipment, split = " ", fixed = TRUE)[[1]]

    if (all(equipment %in% validEquipment))
      theObject@equipment <- equipment
    else
      stop("Invalid equipment!")

    return(theObject)
  }
)

#' Initialize theoretical employee
#'
#' Initialize employee data and its working hours. This function can be applied
#'   only to theoretical employees. Theoretica employees represent the manpower
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
#'   This argument is used only for \code{\link{Production Personnel-class}}
#'   objects.
#' @return \code{\link{Employee-class}} object
#' @export initTEmployee
setGeneric(
  name = "initTEmployee",
  def = function(theObject,
                 ID,
                 costCode = "NONE",
                 equipment,
                 OT = 3,
                 calDays,
                 mdtProb,
                 spareFactor = 1,
                 monthSched = NA) {
    standardGeneric("initTEmployee")
  }
)

#' @describeIn initTEmployee Initialize ID, costCode and spareFactor
setMethod(
  f = "initTEmployee",
  signature = "Employee",
  definition = function(theObject,
                        ID,
                        costCode,
                        spareFactor = 1) {
    # Checking of the validity of all arguments must be done prior to calling
    #  initTEmployee()

    theObject@ID <- ID
    theObject@costCode <- costCode
    theObject@spareFactor <- spareFactor

    return(theObject)
  }
)

#' @describeIn initTEmployee Initialize reg
setMethod(
  f = "initTEmployee",
  signature = "Staff",
  definition = function(theObject,
                        ID,
                        costCode,
                        calDays,
                        spareFactor = 1) {

    theObject <- callNextMethod(theObject = theObject,
                                ID = ID,
                                costCode = costCode,
                                spareFactor = spareFactor)
    theObject@reg <- as.integer(calDays[,c("reg")] * 8 * theObject@spareFactor)
    return(theObject)
  }
)

#' @describeIn initTEmployee Initialize reg and regOT
setMethod(
  f = "initTEmployee",
  signature = "Clerk",
  definition = function(theObject,
                        ID,
                        costCode,
                        OT = 3,
                        calDays,
                        spareFactor = 1) {
    theObject <- callNextMethod(theObject = theObject,
                                ID = ID,
                                costCode = costCode,
                                spareFactor = spareFactor)
    theObject@reg <- as.integer(calDays[,c("reg")] * 8 *
                                  theObject@spareFactor)
    theObject@regOT <- as.integer(calDays[,c("reg")] * OT *
                                    theObject@spareFactor)

    return(theObject)
  }
)

#' @describeIn initTEmployee Initialize all man hour type using man day type
#'   probabilities
setMethod(
  f = "initTEmployee",
  signature = "Operation Personnel",
  definition = function(theObject,
                        ID,
                        costCode,
                        OT = 3,
                        mdtProb,
                        spareFactor = 1,
                        monthSched = NA) {
    theObject <- callNextMethod(theObject = theObject,
                                ID = ID,
                                costCode = costCode,
                                spareFactor = spareFactor)

    reg <- mdtProb$reg * theObject@spareFactor * 8
    rd <- mdtProb$rd * theObject@spareFactor * 8
    sh <- mdtProb$sh * theObject@spareFactor * 8
    lh <- mdtProb$lh * theObject@spareFactor * 8
    nh <- mdtProb$nh * theObject@spareFactor * 8
    rs <- mdtProb$rs * theObject@spareFactor * 8
    rl <- mdtProb$rl * theObject@spareFactor * 8
    rn <- mdtProb$rn * theObject@spareFactor * 8

    regOT <- mdtProb$reg * theObject@spareFactor * OT
    rdOT <- mdtProb$rd * theObject@spareFactor * OT
    shOT <- mdtProb$sh * theObject@spareFactor * OT
    lhOT <- mdtProb$lh * theObject@spareFactor * OT
    nhOT <- mdtProb$nh * theObject@spareFactor * OT
    rsOT <- mdtProb$rs * theObject@spareFactor * OT
    rlOT <- mdtProb$rl * theObject@spareFactor * OT
    rnOT <- mdtProb$rn * theObject@spareFactor * OT

    if (any(is.na(monthSched))) {

      theObject@reg <- as.integer(reg * mdtProb$days + 0.5)
      theObject@rd <- as.integer(rd * mdtProb$days + 0.5)
      theObject@sh <- as.integer(sh * mdtProb$days + 0.5)
      theObject@lh <- as.integer(lh * mdtProb$days + 0.5)
      theObject@nh <- as.integer(nh * mdtProb$days + 0.5)
      theObject@rs <- as.integer(rs * mdtProb$days + 0.5)
      theObject@rl <- as.integer(rl * mdtProb$days + 0.5)
      theObject@rn <- as.integer(rn * mdtProb$days + 0.5)

      theObject@regOT <- as.integer(regOT * mdtProb$days + 0.5)
      theObject@rdOT <- as.integer(rdOT * mdtProb$days + 0.5)
      theObject@shOT <- as.integer(shOT * mdtProb$days + 0.5)
      theObject@lhOT <- as.integer(lhOT * mdtProb$days + 0.5)
      theObject@nhOT <- as.integer(nhOT * mdtProb$days + 0.5)
      theObject@rsOT <- as.integer(rsOT * mdtProb$days + 0.5)
      theObject@rlOT <- as.integer(rlOT * mdtProb$days + 0.5)
      theObject@rnOT <- as.integer(rnOT * mdtProb$days + 0.5)

    } else {

      theObject@reg <- as.integer(reg * monthSched + 0.5)
      theObject@rd <- as.integer(rd * monthSched + 0.5)
      theObject@sh <- as.integer(sh * monthSched + 0.5)
      theObject@lh <- as.integer(lh * monthSched + 0.5)
      theObject@nh <- as.integer(nh * monthSched + 0.5)
      theObject@rs <- as.integer(rs * monthSched + 0.5)
      theObject@rl <- as.integer(rl * monthSched + 0.5)
      theObject@rn <- as.integer(rn * monthSched + 0.5)

      theObject@regOT <- as.integer(regOT * monthSched + 0.5)
      theObject@rdOT <- as.integer(rdOT * monthSched + 0.5)
      theObject@shOT <- as.integer(shOT * monthSched + 0.5)
      theObject@lhOT <- as.integer(lhOT * monthSched + 0.5)
      theObject@nhOT <- as.integer(nhOT * monthSched + 0.5)
      theObject@rsOT <- as.integer(rsOT * monthSched + 0.5)
      theObject@rlOT <- as.integer(rlOT * monthSched + 0.5)
      theObject@rnOT <- as.integer(rnOT * monthSched + 0.5)

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
         "division manager" = division_manager(),
         "group manager" = group_manager(),
         "department manager" = department_manager(),
         "section head" = section_head(),
         "clerk" = clerk(),
         "technical" = technical(),
         "supervisor" = supervisor(),
         "laborer" = laborer(),
         "operator" = operator())
}
