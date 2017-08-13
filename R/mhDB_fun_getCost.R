#' @import methods
NULL

#' Compute Personnel Cost
#'
#' Calculates monthly personnel cost for each cost code. Personnel costs are
#'   comprised by:
#'   \describe{
#'     \item{521001}{Salaries-Regular}
#'     \item{521021}{OT Pay - Regular}
#'     \item{521002}{Salaries-Seasonal}
#'     \item{521022}{OT Pay - Seasonal}
#'     \item{521004}{Employees Allowance}
#'     \item{521005}{Employee Benefits}
#'     \item{521006}{Premium SSS, EC}
#'     \item{521007}{Prem-HDMF (Pag-ibig)}
#'     \item{521008}{Philhealth}
#'     \item{521012}{Leave Commutation}
#'     \item{521017}{Hospital and Medical Expenses}
#'     \item{521009}{13th Month Pay}
#'   }
#'   After computation, an ODS file is written. Each sheet denotes a cost
#'   code. Personnel costs are tabulated by month.
#'
#' @param mhDB a \code{\link{data.frame}} similar to \code{mhDB} return by
#'   \code{\link{getmhDB}}
#' @param listR a list of \code{\link{Employee-class}} objects representing the
#'   employee pool
#' @param wage a \code{\link{data.frame}} composed of two columns:
#'   \describe{
#'     \item{ID}{character string representing a unique identifier of an
#'       employee}
#'     \item{s}{integer value representing the salary of an employee
#'
#'        For rank and file, this value represents the daily salary at the
#'        beginning of the year. For non-rank and file, this value represents
#'        the monthly salary at the beginning of the year.}
#'   }
#' @param dir character string defining the directory wherein the ODS file will
#'   be written
#' @return 0 if success
#' @importFrom dplyr left_join
#' @importFrom magrittr "%>%"
#' @importFrom tidyr gather
getCost <- function(mhDB, listR, wage, dir = "~") {

  # Fix for "no visible binding for global varible" note in R CMD check
  sal <- salM <- ID <- salH <- NULL

  # library(mansched)
  # library(readODS)
  # library(readr)
  # library(magrittr)
  # myFile <- system.file("exdata", "sampleData.ods", package = "mansched")
  # empReq <- read_ods(path = myFile,
  #                    sheet = 1,
  #                    col_types = cols(.default = col_character(),
  #                                     quantity = col_integer(),
  #                                     spareFactor = col_number(),
  #                                     OT = col_integer()))
  # sched <- read_ods(path = myFile,
  #                   sheet = 2,
  #                   col_types = cols(.default = col_integer(),
  #                                    activity = col_character()))
  # empPool <- read_ods(path = myFile,
  #                     sheet = 3,
  #                     col_types = cols(.default = col_character(),
  #                                      attendance = col_integer(),
  #                                      inHouse = col_logical(),
  #                                      isRF = col_logical()))
  # hol <- read_ods(path = myFile,
  #                 sheet = 4)
  # year <- 2018
  # tempData <- getmhDB(empReq = empReq,
  #                     empPool = empPool,
  #                     sched = sched,
  #                     year = year,
  #                     hol = mansched::holidays)
  # mhDB <- tempData[[1]]
  # listR <- tempData[[3]]
  # wage <- read_ods(path = myFile,
  #                  sheet = 5,
  #                  col_types = cols(col_character(), col_number()))


  # Start of Code
  # Error if any ID in wage is duplicated
  if (anyDuplicated(wage$ID) > 0) {
    tempData <- wage$ID[which(duplicated(wage$ID))]
    cat(paste("Duplicated :", tempData, "\n", sep = ""))
    stop("There must be no duplicated ID's in wage data!")
  }

  # Error if any ID in listR is not in wage$ID
  empID <- sapply(listR, FUN = function(x) x@ID)
  if (any(!empID %in% wage$ID)) {
    empID <- empID[which(!empID %in% wage$ID)]
    cat(paste("No wage data for :", empID, "\n", sep = ""))
    stop("All ID's must have wage data")
  }

  # Assign if employee is RF or not
  wage$isRF <- sapply(wage$ID, FUN = function(x) {
    index <- which(empID == x)
    isRF(listR[[index]])
  })

  # Get salary increase
  wage$sB <- apply(wage[,c(2:3)], MARGIN = 1, FUN = function(x) {
    sal <- NA
    if (x[2])
      sal <- x[1] + 100
    else
      sal <- x[1] + 3000
    return(sal)
  })

  # Assign totHours
  wage$totHours <- sapply(wage$ID, FUN = function(x) {
    index <- which(empID == x)
    sum(listR[[index]]@totHours)
  })

  # Compute hourly rates
  tempData <- apply(wage[,2:5], MARGIN = 1, FUN = function(x) {
    sal <- c(NA, NA)
    if (x[2]) {
      sal[1] <- x[1] / 8
      sal[2] <- x[3] / 8
    } else {
      sal[1] <- x[1] / x[4]
      sal[2] <- x[3] / x[4]
      sal <- sal * 12
    }
    sal <- round(sal, digits = 2)
    return(sal)
  })
  wage$hRateA <- tempData[1,]
  wage$hRateB <- tempData[2,]

  wageM <- wage[,c(1,2,4)]
  wageH <- wage[,c(1,6,7)]
  colnames(wageM)[c(2,3)] <- c("a", "b")
  colnames(wageH)[c(2,3)] <- c("a", "b")

  wageM <- wageM %>% tidyr::gather(sal, salM, -ID)
  wageH <- wageH %>% tidyr::gather(sal, salH, -ID)

  wageTemp <- dplyr::left_join(x = wageM,
                               y = wageH,
                               by = c("ID", "sal"))
  wageTemp <- dplyr::left_join(x = wageTemp,
                               y = wage[,c(1,3)],
                               by = c("ID"))



  mhDB.m <- mhDB[which(mhDB$scheme == "m"),]

  mhDB.d <- mhDB[which(mhDB$scheme == "d"),]

  tempDir <- getwd()
  setwd(dir)
  message(paste("Saving ODS in: '", getwd(), "'.", sep = ""))
  # write ODS
  setwd(tempDir)
  return(0L)
}
