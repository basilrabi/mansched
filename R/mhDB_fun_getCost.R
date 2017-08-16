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
#' @importFrom dplyr left_join group_by summarise
#' @importFrom magrittr "%>%"
#' @importFrom tidyr gather
getCost <- function(mhDB, listR, wage, dir = "~") {

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

  ### Code Contents ###

  # Fix for "no visible binding for global variable" note in R CMD check
  # Error if any ID in wage is duplicated
  # Error if any ID in listR is not in wage$ID
  # Assign if employee is RF or not
  # Assign if employee is staff or not
  # Get salary increase
  # Assign totHours
  # Compute hourly rates

  # Compute Salaries for monthly wagers
  ## Separate Regular Employees
  ### Separate non-OT
  #### Get monthly wage minus absences
  ##### Get absences hours
  ##### Get salary scheme
  ##### Get monthly and hourly salary
  ##### Get absences cost
  ##### monthly wage minus absences = salMB
  #### Combine salMB
  #### Get man hour fraction of each cost code per month
  #### Compute Costs
  ### Separate OT
  #### Get salary scheme
  #### Get hourly wage
  #### Compute Costs
  ## Separate Non-regular Employees

  # Fix for "no visible binding for global variable" note in R CMD check
  sal <-
    salM <-
    ID <-
    salH <-
    mh <- NULL

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

  # Assign if employee is staff or not
  wage$isStaff <- sapply(wage$ID, FUN = function(x) {
    index <- which(empID == x)
    is(listR[[index]], "Staff")
  })

  # Get salary increase
  wage$sB <- apply(wage[,c(2:4)], MARGIN = 1, FUN = function(x) {
    sal <- NA
    if (x[2])
      sal <- x[1] + 105
    else if (x[3])
      sal <- x[1] + 3500
    else
      sal <- x[1] + 3000
    return(sal)
  })

  # Assign totHours
  wage$totHours <- sapply(wage$ID, FUN = function(x) {
    index <- which(empID == x)

    # sum(listR[[index]]@totHours)
    # total days per year is 313 as advised by Accounting

    return(313 * 8)
  })

  # Compute hourly rates
  tempData <- apply(wage[,2:6], MARGIN = 1, FUN = function(x) {
    sal <- c(NA, NA)
    if (x[2]) {
      sal[1] <- x[1] / 8
      sal[2] <- x[4] / 8
    } else {
      sal[1] <- x[1] / x[5]
      sal[2] <- x[4] / x[5]
      sal <- sal * 12
    }
    sal <- round(sal, digits = 2)
    return(sal)
  })
  wage$hRateA <- tempData[1,]
  wage$hRateB <- tempData[2,]

  wageM <- wage[, colnames(wage) %in% c("ID", "s", "sB")]
  wageH <- wage[, colnames(wage) %in% c("ID", "hRateA", "hRateB")]
  colnames(wageM)[c(2,3)] <- c("a", "b")
  colnames(wageH)[c(2,3)] <- c("a", "b")

  wageM <- wageM %>% tidyr::gather(sal, salM, -ID)
  wageH <- wageH %>% tidyr::gather(sal, salH, -ID)

  wageEmp <- dplyr::left_join(x = wageM,
                              y = wageH,
                              by = c("ID", "sal"))
  wageEmp <- dplyr::left_join(x = wageEmp,
                              y = wage[,c(1,3)],
                              by = c("ID"))

  # Compute Salaries for monthly wagers

  mhDB.m <- mhDB[which(mhDB$scheme == "m"),
                 !colnames(mhDB) %in% c("scheme")]

  ## Separate Regular Employees
  mhDB.m.R <- mhDB.m[which(mhDB.m$isReg),
                     !colnames(mhDB.m) %in% c("sal", "isReg", "maxReg")]

  mhDB.m.R <- dplyr::left_join(x = mhDB.m.R,
                               y = premium[,c("isOT.R",
                                              "premiumR",
                                              "npR",
                                              "mhType")])

  ### Separate non-OT
  mhDB.m.R.Reg <- mhDB.m.R[which(!mhDB.m.R$isOT.R),
                           !colnames(mhDB.m.R) %in% c("mhType",
                                                      "salH",
                                                      "isOT.R",
                                                      "premiumR")]

  #### Get monthly wage minus absences

  mhDB.m.R.Reg.M <- mhDB.m.R.Reg %>%
    dplyr::group_by(ID, month) %>%
    dplyr::summarise(mhTot = sum(mh))

  mhDB.m.R.Reg.M <- dplyr::left_join(
    x = mhDB.m.R.Reg.M,
    y = unique(mhDB[, c("ID", "month", "maxReg")])
  )

  ##### Get absences hours
  mhDB.m.R.Reg.M$abHours <- mhDB.m.R.Reg.M$maxReg - mhDB.m.R.Reg.M$mhTot

  ##### Get salary scheme
  mhDB.m.R.Reg.M <- dplyr::left_join(
    x = mhDB.m.R.Reg.M,
    y = unique(mhDB[,c("ID", "month", "sal")])
  )

  ##### Get monthly and hourly salary
  mhDB.m.R.Reg.M <- dplyr::left_join(
    x = mhDB.m.R.Reg.M,
    y = wageEmp[, !colnames(wageEmp) %in% c("isRF")]
  )

  ##### Get absences cost
  mhDB.m.R.Reg.M$abCost <- round(
    mhDB.m.R.Reg.M$salH * mhDB.m.R.Reg.M$abHours, digits = 2
  )

  ##### monthly wage minus absences = salMB
  mhDB.m.R.Reg.M$salMB <- mhDB.m.R.Reg.M$salM - mhDB.m.R.Reg.M$abCost

  #### Combine salMB
  mhDB.m.R.Reg <- dplyr::left_join(
    x = mhDB.m.R.Reg,
    y = mhDB.m.R.Reg.M[,c("ID",
                          "month",
                          "mhTot",
                          "salH",
                          "salMB")]
  )

  #### Get man hour fraction of each cost code per month
  mhDB.m.R.Reg$Xmh <- mhDB.m.R.Reg$mh / mhDB.m.R.Reg$mhTot

  #### Compute Costs
  #### These costs are purely worked
  mhDB.m.R.Reg$costWage <- round(mhDB.m.R.Reg$Xmh * mhDB.m.R.Reg$salMB,
                                 digits = 2)
  mhDB.m.R.Reg$costNP <- round(
    mhDB.m.R.Reg$salH * mhDB.m.R.Reg$np * mhDB.m.R.Reg$npR, digits = 2)


  ### Separate OT

  mhDB.m.R.OT <- mhDB.m.R[which(mhDB.m.R$isOT.R),
                          !colnames(mhDB.m.R) %in% c("mhType")]

  #### Get salary scheme
  mhDB.m.R.OT <- dplyr::left_join(
    x = mhDB.m.R.OT,
    y = unique(mhDB[,c("ID", "month", "sal")])
  )

  #### Get hourly wage
  mhDB.m.R.OT <- dplyr::left_join(
    x = mhDB.m.R.OT,
    y = wageEmp[, !colnames(wageEmp) %in% c("salM", "isRF")]
  )

  #### Compute Costs
  mhDB.m.R.OT$costWage <- round(
    mhDB.m.R.OT$salH * mhDB.m.R.OT$mh * mhDB.m.R.OT$premiumR, digits = 2
  )
  mhDB.m.R.OT$costNP <- round(
    mhDB.m.R.OT$salH * mhDB.m.R.OT$np * mhDB.m.R.OT$npR, digits = 2
  )

  ## Separate Non-regular Employees

  mhDB.m.S <- mhDB.m[which(!mhDB.m$isReg),
                     !colnames(mhDB.m) %in% c("sal", "isReg", "maxReg")]

  mhDB.m.S <- dplyr::left_join(x = mhDB.m.S,
                               y = premium[, c("isOT.S",
                                               "premiumS",
                                               "npS",
                                               "mhType")])

  ### Separate non-OT
  mhDB.m.S.Reg <- mhDB.m.S[which(!mhDB.m.S$isOT.S),
                           !colnames(mhDB.m.S) %in% c("mhType",
                                                      "salH",
                                                      "isOT.S",
                                                      "premiumS")]

  #### Get monthly wage minus absences

  mhDB.m.S.Reg.M <- mhDB.m.S.Reg %>%
    dplyr::group_by(ID, month) %>%
    dplyr::summarise(mhTot = sum(mh))

  mhDB.m.S.Reg.M <- dplyr::left_join(
    x = mhDB.m.S.Reg.M,
    y = unique(mhDB[, c("ID", "month", "maxReg")])
  )


  ### Separate OT
  mhDB.m.S.OT <- mhDB.m.S[which(mhDB.m.S$isOT.S),
                          !colnames(mhDB.m.S) %in% c("mhType")]

  # Compute Salaries-Regular for daily wagers

  mhDB.d <- mhDB[which(mhDB$scheme == "d"),
                 !colnames(mhDB) %in% c("scheme")]

  tempDir <- getwd()
  setwd(dir)
  message(paste("Saving ODS in: '", getwd(), "'.", sep = ""))
  # write ODS
  setwd(tempDir)
  return(0L)
}
