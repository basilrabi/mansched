#' @import methods
NULL

#' Spend all available man hours
#'
#' Available man hours of a specific man hour type from an
#'   \code{\link{Employee-class}} object representing a real employee is
#'   assigned to the man hours of an \code{Employee-class} object representing
#'   an employee requirement.
#'
#' @param hoursT integer vector of length 12
#'
#'   This represents the man hours from a theoretical employee with a certain
#'   man hour type.
#' @param hoursR integer vector of length 12
#'
#'   This represents the man hours from a real employee with a certain man hour
#'   type.
#' @return a \code{\link{data.frame}} with 12 rows and 4 columns
#'
#'   Each row represents a month. The columns are defined as follows:
#'   \describe{
#'     \item{hoursT}{man hours from the theoretical employee}
#'     \item{hoursR}{man hours from the real employee}
#'     \item{hoursA}{man hours to be assigned}
#'     \item{month}{integer representing the month of assignment}
#'   }
#' @export assignMH
assignMH <- function(hoursT, hoursR) {

  hoursData <- data.frame(hoursT = hoursT,
                          hoursR = hoursR,
                          hoursA = 0,
                          month  = c(1:12))

  hoursData$hoursA <- apply(hoursData[,c(1,2)], MARGIN = 1, FUN = min)
  hoursData$hoursT <- hoursData$hoursT - hoursData$hoursA
  hoursData$hoursR <- hoursData$hoursR - hoursData$hoursA

  hoursData <- lapply(hoursData, as.integer)

  return(hoursData)
}

#' Assign an employee
#'
#' Man hours from an \code{\link{Employee-class}} object representing a real
#'   employee is assigned to the man hours from an \code{\link{Employee-class}}
#'   object representing another real employee (manpower requirement). During
#'   successful assignment, the man hours of both \code{Employee-class} objects
#'   are reduced.
#'
#' @param empT an \code{\link{Employee-class}} object
#' @param empR an \code{\link{Employee-class}} object
#' @return a list containing the following:
#'   \enumerate{
#'     \item a \code{\link{data.frame}} containing the man hours database
#'       resulting from the employee assignment. This is consisted of the
#'       following columns:
#'       \describe{
#'         \item{ID}{character string representing the unique identifier of the
#'           real employee}
#'         \item{mh}{integer value representing the man hours assigned}
#'         \item{mhType}{character string representing the man hour type
#'
#'            \code{mhType} can be one of the following:
#'            \describe{
#'              \item{reg}{working hours not more than 8 hours during a regular
#'                day}
#'              \item{rd}{working hours not more than 8 hours during a rest day}
#'              \item{sh}{working hours not more than 8 hours during a special
#'                holiday}
#'              \item{lh}{working hours not more than 8 hours during a legal
#'                holiday}
#'              \item{nh}{working hours not more than 8 hours during a
#'                negotiated holiday}
#'              \item{rs}{working hours not more than 8 hours during a rest day
#'                on a special holiday}
#'              \item{rl}{working hours not more than 8 hours during a rest day
#'                on a legal holiday}
#'              \item{rn}{working hours not more than 8 hours during a rest day
#'                on a negotiated holiday}
#'              \item{regOT}{working hours more than 8 hours during a regular
#'                day}
#'              \item{rdOT}{working hours more than 8 hours during a rest day}
#'              \item{shOT}{working hours more than 8 hours during a special
#'                holiday}
#'              \item{lhOT}{working hours more than 8 hours during a legal
#'                holiday}
#'              \item{nhOT}{working hours more than 8 hours during a negotiated
#'                holiday}
#'              \item{rsOT}{working hours more than 8 hours during a rest day on
#'                a special holiday}
#'              \item{rlOT}{working hours more than 8 hours during a rest day on
#'                a legal holiday}
#'              \item{rnOT}{working hours more than 8 hours during a rest day on
#'                a negotiated holiday}
#'            }
#'         }
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
#'         \item{status}{character string defining the employment status of
#'           the employee}
#'         \item{maxReg}{integer value \cr
#'           Number of hours the employee is required to report to enjoy full
#'           salary.}
#'       }
#'     \item an \code{\link{Employee-class}} object representing the theoretical
#'       employee with reduced man hours
#'     \item an \code{\link{Employee-class}} object representing the real
#'       employee with reduced man hours
#'   }
#' @importFrom data.table rbindlist
#' @importFrom dplyr left_join
#' @export assignEmp
setGeneric(
  name = "assignEmp",
  def  = function(empT, empR) {
    standardGeneric("assignEmp")
  }
)

#' @describeIn assignEmp Assign \code{reg} hours
setMethod(
  f          = "assignEmp",
  signature  = "Employee",
  definition = function(empT, empR) {

    tempData <- assignMH(hoursT = empT@reg,
                         hoursR = empR@reg)

    empT@reg <- tempData$hoursT
    empR@reg <- tempData$hoursR

    mhDB <- data.frame(ID               = empR@ID,
                       mh               = tempData$hoursA,
                       mhType           = "reg",
                       month            = tempData$month,
                       np               = 0,
                       costCode         = empT@costCode,
                       stringsAsFactors = FALSE)

    mhDB <- mhDB[which(mhDB$mh > 0),]

    return(list(mhDB, empT, empR))
  }
)

#' @describeIn assignEmp Check for incompatible \code{\link{Staff-class}}
#'   objects
setMethod(
  f          = "assignEmp",
  signature  = "Staff",
  definition = function(empT, empR) {

    if (class(empT) != class(empR))
      stop("Incompatible class!")

    # Assign hourly salary
    # Staff has no probationary and seasonal

    results      <- callNextMethod(empT = empT, empR = empR)

    results[[1]] <- dplyr::left_join(x  = results[[1]],
                                     y  = payA,
                                     by = "month")

    if (length(results[[1]]$ID) > 0) {

      results[[1]]$scheme <- "m"
      results[[1]]$status <- empR@status

      maxReg   <- data.frame(month  = 1:12,
                             maxReg = empR@maxReg)

      tempData <- dplyr::left_join(x  = results[[1]],
                                   y  = maxReg,
                                   by = "month")

      results[[1]] <- as.data.frame(tempData)

    } else {
      results[[1]] <- NA
    }

    return(results)
  }
)

#' @describeIn assignEmp Assign \code{regOT} hours
setMethod(
  f          = "assignEmp",
  signature  = "NonStaff",
  definition = function(empT, empR) {

    results <- callNextMethod(empT = empT, empR = empR)

    empT <- results[[2]]
    empR <- results[[3]]

    tempData <- assignMH(hoursT =  empT@regOT, hoursR = empR@regOT)

    empT@regOT <- tempData$hoursT
    empR@regOT <- tempData$hoursR

    mhDB <- data.frame(ID               = empR@ID,
                       mh               = tempData$hoursA,
                       mhType           = "regOT",
                       month            = tempData$month,
                       np               = 0,
                       costCode         = empT@costCode,
                       stringsAsFactors = FALSE)

    mhDB <- mhDB[which(mhDB$mh > 0),]
    mhDB <- rbind(results[[1]], mhDB)

    return(list(mhDB, empT, empR))
  }
)

#' @describeIn assignEmp Check for incompatible \code{\link{Clerk-class}}
#'   objects
setMethod(
  f          = "assignEmp",
  signature  = "Clerk",
  definition = function(empT, empR) {

    if (class(empT) != class(empR))
      stop("Incompatible class!")

    results <- callNextMethod(empT = empT, empR = empR)

    if (length(results[[1]]$ID) > 0) {

      if (empR@status != "reg") {
        results[[1]]$sal <- "a"
      } else {

        if (empR@isRF) {
          results[[1]] <- dplyr::left_join(x  = results[[1]],
                                           y  = payB,
                                           by = "month")
        } else {
          results[[1]] <- dplyr::left_join(x  = results[[1]],
                                           y  = payA,
                                           by = "month")
        }
      }

      if (empR@isRF) {
        results[[1]]$scheme <- "d"
      } else {
        results[[1]]$scheme <- "m"
      }

      results[[1]]$status <- empR@status

      maxReg   <- data.frame(month  = 1:12,
                             maxReg = empR@maxReg)

      tempData <- dplyr::left_join(x  = results[[1]],
                                   y  = maxReg,
                                   by = "month")

      results[[1]] <- as.data.frame(tempData)
    } else {
      results[[1]] <- NA
    }

    return(results)
  }
)

#' @describeIn assignEmp Assign \code{rd}, \code{sh}, \code{lh}, \code{nh},
#'   \code{rs}, \code{rl}, \code{rn}, \code{rdOT}, \code{shOT}, \code{lhOT},
#'   \code{nhOT}, \code{rsOT}, \code{rlPT} and \code{rnOT}
setMethod(
  f= "assignEmp",
  signature = "OperationPersonnel",
  definition = function(empT, empR) {

    results <- callNextMethod(empT = empT, empR = empR)
    empT <- results[[2]]
    empR <- results[[3]]

    tempData.rd <- assignMH(hoursT = empT@rd, hoursR = empR@rd)
    tempData.sh <- assignMH(hoursT = empT@sh, hoursR = empR@sh)
    tempData.lh <- assignMH(hoursT = empT@lh, hoursR = empR@lh)
    tempData.nh <- assignMH(hoursT = empT@nh, hoursR = empR@nh)
    tempData.rs <- assignMH(hoursT = empT@rs, hoursR = empR@rs)
    tempData.rl <- assignMH(hoursT = empT@rl, hoursR = empR@rl)
    tempData.rn <- assignMH(hoursT = empT@rn, hoursR = empR@rn)

    # If a non-regular RF is assigned in a special holiday, add 8 hours per
    # special holiday assigned in holHours
    if (!isReg(empR) & isRF(empR)) {

      if (sum(tempData.sh$hoursA) + sum(tempData.rs$hoursA) > 0) {

        shToBeAddedA <- tempData.sh$hoursA
        shToBeAddedB <- tempData.rs$hoursA

        indexZeroA <- which(shToBeAddedA == 0L)
        indexZeroB <- which(shToBeAddedB == 0L)

        shToBeAddedA <- shToBeAddedA %/% 8
        shToBeAddedB <- shToBeAddedB %/% 8
        shToBeAddedA <- shToBeAddedA + 1L
        shToBeAddedB <- shToBeAddedB + 1L
        shToBeAddedA <- shToBeAddedA * 8L
        shToBeAddedB <- shToBeAddedB * 8L

        shToBeAddedA[indexZeroA] <- 0L
        shToBeAddedB[indexZeroB] <- 0L

        empR@holHours <- as.integer(empR@holHours + shToBeAddedA + shToBeAddedB)
      }

    }

    tempData.rdOT <- assignMH(hoursT = empT@rdOT, hoursR = empR@rdOT)
    tempData.shOT <- assignMH(hoursT = empT@shOT, hoursR = empR@shOT)
    tempData.lhOT <- assignMH(hoursT = empT@lhOT, hoursR = empR@lhOT)
    tempData.nhOT <- assignMH(hoursT = empT@nhOT, hoursR = empR@nhOT)
    tempData.rsOT <- assignMH(hoursT = empT@rsOT, hoursR = empR@rsOT)
    tempData.rlOT <- assignMH(hoursT = empT@rlOT, hoursR = empR@rlOT)
    tempData.rnOT <- assignMH(hoursT = empT@rnOT, hoursR = empR@rnOT)

    empT@rd <- tempData.rd$hoursT
    empT@sh <- tempData.sh$hoursT
    empT@lh <- tempData.lh$hoursT
    empT@nh <- tempData.nh$hoursT
    empT@rs <- tempData.rs$hoursT
    empT@rl <- tempData.rl$hoursT
    empT@rn <- tempData.rn$hoursT

    empT@rdOT <- tempData.rdOT$hoursT
    empT@shOT <- tempData.shOT$hoursT
    empT@lhOT <- tempData.lhOT$hoursT
    empT@nhOT <- tempData.nhOT$hoursT
    empT@rsOT <- tempData.rsOT$hoursT
    empT@rlOT <- tempData.rlOT$hoursT
    empT@rnOT <- tempData.rnOT$hoursT

    empR@rd <- tempData.rd$hoursR
    empR@sh <- tempData.sh$hoursR
    empR@lh <- tempData.lh$hoursR
    empR@nh <- tempData.nh$hoursR
    empR@rs <- tempData.rs$hoursR
    empR@rl <- tempData.rl$hoursR
    empR@rn <- tempData.rn$hoursR

    empR@rdOT <- tempData.rdOT$hoursR
    empR@shOT <- tempData.shOT$hoursR
    empR@lhOT <- tempData.lhOT$hoursR
    empR@nhOT <- tempData.nhOT$hoursR
    empR@rsOT <- tempData.rsOT$hoursR
    empR@rlOT <- tempData.rlOT$hoursR
    empR@rnOT <- tempData.rnOT$hoursR

    mhDB.rd <- data.frame(ID               = empR@ID,
                          mh               = tempData.rd$hoursA,
                          mhType           = "rd",
                          month            = tempData.rd$month,
                          np               = 0,
                          costCode         = empT@costCode,
                          stringsAsFactors = FALSE)

    mhDB.sh <- data.frame(ID               = empR@ID,
                          mh               = tempData.sh$hoursA,
                          mhType           = "sh",
                          month            = tempData.sh$month,
                          np               = 0,
                          costCode         = empT@costCode,
                          stringsAsFactors = FALSE)

    mhDB.lh <- data.frame(ID               = empR@ID,
                          mh               = tempData.lh$hoursA,
                          mhType           = "lh",
                          month            = tempData.lh$month,
                          np               = 0,
                          costCode         = empT@costCode,
                          stringsAsFactors = FALSE)

    mhDB.nh <- data.frame(ID               = empR@ID,
                          mh               = tempData.nh$hoursA,
                          mhType           = "nh",
                          month            = tempData.nh$month,
                          np               = 0,
                          costCode         = empT@costCode,
                          stringsAsFactors = FALSE)

    mhDB.rs <- data.frame(ID               = empR@ID,
                          mh               = tempData.rs$hoursA,
                          mhType           = "rs",
                          month            = tempData.rs$month,
                          np               = 0,
                          costCode         = empT@costCode,
                          stringsAsFactors = FALSE)

    mhDB.rl <- data.frame(ID               = empR@ID,
                          mh               = tempData.rl$hoursA,
                          mhType           = "rl",
                          month            = tempData.rl$month,
                          np               = 0,
                          costCode         = empT@costCode,
                          stringsAsFactors = FALSE)

    mhDB.rn <- data.frame(ID               = empR@ID,
                          mh               = tempData.rn$hoursA,
                          mhType           = "rn",
                          month            = tempData.rn$month,
                          np               = 0,
                          costCode         = empT@costCode,
                          stringsAsFactors = FALSE)

    mhDB.rdOT <- data.frame(ID               = empR@ID,
                            mh               = tempData.rdOT$hoursA,
                            mhType           = "rdOT",
                            month            = tempData.rdOT$month,
                            np               = 0,
                            costCode         = empT@costCode,
                            stringsAsFactors = FALSE)

    mhDB.shOT <- data.frame(ID               = empR@ID,
                            mh               = tempData.shOT$hoursA,
                            mhType           = "shOT",
                            month            = tempData.shOT$month,
                            np               = 0,
                            costCode         = empT@costCode,
                            stringsAsFactors = FALSE)

    mhDB.lhOT <- data.frame(ID               = empR@ID,
                            mh               = tempData.lhOT$hoursA,
                            mhType           = "lhOT",
                            month            = tempData.lhOT$month,
                            np               = 0,
                            costCode         = empT@costCode,
                            stringsAsFactors = FALSE)

    mhDB.nhOT <- data.frame(ID               = empR@ID,
                            mh               = tempData.nhOT$hoursA,
                            mhType           = "nhOT",
                            month            = tempData.nhOT$month,
                            np               = 0,
                            costCode         = empT@costCode,
                            stringsAsFactors = FALSE)

    mhDB.rsOT <- data.frame(ID               = empR@ID,
                            mh               = tempData.rsOT$hoursA,
                            mhType           = "rsOT",
                            month            = tempData.rsOT$month,
                            np               = 0,
                            costCode         = empT@costCode,
                            stringsAsFactors = FALSE)

    mhDB.rlOT <- data.frame(ID               = empR@ID,
                            mh               = tempData.rlOT$hoursA,
                            mhType           = "rlOT",
                            month            = tempData.rlOT$month,
                            np               = 0,
                            costCode         = empT@costCode,
                            stringsAsFactors = FALSE)

    mhDB.rnOT <- data.frame(ID               = empR@ID,
                            mh               = tempData.rnOT$hoursA,
                            mhType           = "rnOT",
                            month            = tempData.rnOT$month,
                            np               = 0,
                            costCode         = empT@costCode,
                            stringsAsFactors = FALSE)

    mhDB <- data.table::rbindlist(list(results[[1]],
                                       mhDB.rd,
                                       mhDB.sh,
                                       mhDB.lh,
                                       mhDB.nh,
                                       mhDB.rs,
                                       mhDB.rl,
                                       mhDB.rn,
                                       mhDB.rdOT,
                                       mhDB.shOT,
                                       mhDB.lhOT,
                                       mhDB.nhOT,
                                       mhDB.rsOT,
                                       mhDB.rlOT,
                                       mhDB.rnOT))

    mhDB <- as.data.frame(mhDB[which(mhDB$mh > 0),])

    return(list(mhDB, empT, empR))
  }
)

#' @describeIn assignEmp Check for incompatible \code{\link{Technical-class}}
#'   objects
setMethod(
  f          = "assignEmp",
  signature = "Technical",
  definition = function(empT, empR) {

    if (class(empT) != class(empR))
      stop("Incompatible class!")

    results <- callNextMethod(empT = empT, empR = empR)

    if (length(results[[1]]$ID) > 0) {
      if (empR@status != "reg") {
        results[[1]]$sal <- "a"
      } else {
        results[[1]] <- dplyr::left_join(x  = results[[1]],
                                         y  = payA,
                                         by = "month")
      }

      results[[1]]$scheme <- "m"
      results[[1]]$status <- empR@status

      maxReg   <- data.frame(month  = 1:12,
                             maxReg = empR@maxReg)

      tempData <- dplyr::left_join(x  = results[[1]],
                                   y  = maxReg,
                                   by = "month")

      results[[1]] <- as.data.frame(tempData)

    } else {
      results[[1]] <- NA
    }

    return(results)
  }
)

#' @describeIn assignEmp Compute for working hours with night premium pay
setMethod(
  f          = "assignEmp",
  signature  = "ProductionPersonnel",
  definition = function(empT, empR) {

    tempData <- callNextMethod(empT = empT, empR = empR)

    mhDB <- tempData[[1]]

    if (length(mhDB$ID) > 0) {
      if (tempData[[3]]@status == "reg") {
        mhDB$np <- as.integer((mhDB$mh * 0.5) + 0.5)
      } else {
        mhDB$np <- as.integer((mhDB$mh * (1/3)) + 0.5)
      }
    } else {
      mhDB <- NA
    }

    return(list(mhDB, tempData[[2]], tempData[[3]]))
  }
)

#' @describeIn assignEmp Check for incompatible \code{\link{Supervisor-class}}
#'   objects
setMethod(
  f          = "assignEmp",
  signature  = "Supervisor",
  definition = function(empT, empR) {

    if (class(empT) != class(empR))
      stop("Incompatible class!")

    results <- callNextMethod(empT = empT, empR = empR)

    if (class(results[[1]]) != "logical") {

      if (empR@status != "reg") {
        results[[1]]$sal <- "a"
      } else {
        results[[1]] <- dplyr::left_join(x  = results[[1]],
                                         y  = payA,
                                         by = "month")
      }

      results[[1]]$scheme <- "m"
      results[[1]]$status <- empR@status

      maxReg   <- data.frame(month  = 1:12,
                             maxReg = empR@maxReg)

      tempData <- dplyr::left_join(x  = results[[1]],
                                   y  = maxReg,
                                   by = "month")

      results[[1]] <- as.data.frame(tempData)
    }

    return(results)
  }
)

#' @describeIn assignEmp Check for incompatible \code{\link{Laborer-class}}
#'   objects
setMethod(
  f          = "assignEmp",
  signature  = "Laborer",
  definition = function(empT, empR) {

    if (class(empT) != class(empR))
      stop("Incompatible class!")

    results <- callNextMethod(empT = empT, empR = empR)

    if (class(results[[1]]) != "logical") {

      if (empR@status != "reg") {
        results[[1]]$sal <- "a"
      } else {
        results[[1]] <- dplyr::left_join(x  = results[[1]],
                                         y  = payB,
                                         by = "month")
      }

      results[[1]]$scheme <- "d"
      results[[1]]$status <- empR@status

      maxReg   <- data.frame(month   = 1:12,
                             maxReg  = empR@maxReg)

      tempData <- dplyr::left_join(x  = results[[1]],
                                   y  = maxReg,
                                   by = "month")

      results[[1]] <- as.data.frame(tempData)
    }

    return(results)
  }
)

#' @describeIn assignEmp Check for incompatible \code{\link{Operator-class}}
#'   objects
setMethod(
  f          = "assignEmp",
  signature  = "Operator",
  definition = function(empT, empR) {

    if (class(empT) != class(empR))
      stop("Incompatible class!")

    if (!empT@equipment %in% empR@equipment)
      stop("Unauthorized equipment!")

    results <- callNextMethod(empT = empT, empR = empR)

    if (class(results[[1]]) != "logical") {
      if (empR@status != "reg") {
        results[[1]]$sal <- "a"
      } else {
        results[[1]] <- dplyr::left_join(x  = results[[1]],
                                         y  = payB,
                                         by = "month")
      }

      results[[1]]$scheme <- "d"
      results[[1]]$status <- empR@status

      maxReg   <- data.frame(month  = 1:12,
                             maxReg = empR@maxReg)

      tempData <- dplyr::left_join(x  = results[[1]],
                                   y  = maxReg,
                                   by = "month")

      results[[1]] <- as.data.frame(tempData)
    }

    return(results)
  }
)
