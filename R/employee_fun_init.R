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
#' @param equipment character vector representing the equipment types which the
#'   employee is authorized to operate
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
    theObject@totHours <- totDays * 8L

    holDays <- apply(calDays[,c("lh", "sh", "nh")], MARGIN = 1, FUN = sum)
    theObject@holHours <- holDays * 8L

    theObject@maxReg <- calDays[,c("reg")] * 8L
    theObject@reg <- as.integer(
      calDays[,c("reg")] * 8 * theObject@attendance
    )

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

    return(theObject, calDays)
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

    theObject@rd <- as.integer(
      calDays[,c("rd")] * theObject@attendance * 8
    )
    theObject@rdOT <- as.integer(
      calDays[,c("rd")] * theObject@attendance * OT
    )

    theObject@sh <- as.integer(
      calDays[,c("sh")] * theObject@attendance * 8
    )
    theObject@shOT <- as.integer(
      calDays[,c("sh")] * theObject@attendance * OT
    )

    theObject@lh <- as.integer(
      calDays[,c("lh")] * theObject@attendance * 8
    )
    theObject@lhOT <- as.integer(
      calDays[,c("lh")] * theObject@attendance * OT
    )

    theObject@nh <- as.integer(
      calDays[,c("nh")] * theObject@attendance * 8
    )
    theObject@nhOT <- as.integer(
      calDays[,c("nh")] * theObject@attendance * OT
    )

    theObject@rs <- as.integer(
      calDays[,c("rs")] * theObject@attendance * 8
    )
    theObject@rsOT <- as.integer(
      calDays[,c("rs")] * theObject@attendance * OT
    )

    theObject@rl <- as.integer(
      calDays[,c("rl")] * theObject@attendance * 8
    )
    theObject@rlOT <- as.integer(
      calDays[,c("rl")] * theObject@attendance * OT
    )

    theObject@rn <- as.integer(
      calDays[,c("rn")] * theObject@attendance * 8
    )
    theObject@rnOT <- as.integer(
      calDays[,c("rn")] * theObject@attendance * OT
    )

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
