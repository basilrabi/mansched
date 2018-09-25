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
#'     \item{521011}{Food Allowance / Rice Subsidy}
#'     \item{521017}{Hospital and Medical Expenses}
#'     \item{521009}{13th Month Pay}
#'     \item{521018}{HMO}
#'     \item{524037}{Grouplife}
#'     \item{523011}{Safety Gadgets}
#'     \item{522099}{CF Others}
#'     \item{522010}{CF Manpower Services}
#'   }
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
#' @param forecast logical value \cr
#'   Compute cost for forecast?
#' @return a list containing the following:
#'
#'   \enumerate{
#'     \item a data.frame with 16 columns
#'
#'       The last 12 columns represents the cost per month. The first column
#'       is a concatenated string from the 2nd column (Cost Center) and 4th
#'       column (accounting code). The 3rd column is the description of the
#'       accounting code.
#'
#'     \item tabulated total man hours per month per cost code
#'     \item tabulated 13th month pay per month per cost code
#'     \item tabulated bonus cost per month per cost code
#'   }
#' @export getCost
#' @importFrom dplyr left_join group_by summarise mutate "%>%"
#' @importFrom data.table rbindlist
#' @importFrom tidyr gather spread
getCost <- function(mhDB, listR, wage, forecast = FALSE) {

  # Fix for "no visible binding for global variable" note in R CMD check
  costCode  <- NULL
  cost      <- NULL
  ID        <- NULL
  mh        <- NULL
  sal       <- NULL
  salH      <- NULL
  salM      <- NULL
  status    <- NULL
  XholHours <- NULL
  distType  <- c("reg", "rd", "sh", "lh", "nh", "rs", "rl", "rn")

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
  IDs     <- sapply(listR, FUN = function(x) {x@ID})
  wage$sB <- apply(wage[, 1:5], MARGIN = 1, FUN = function(x) {

    x     <- sapply(x, trimws)
    sal   <- as.numeric(x[2])
    inc   <- as.numeric(x[3])

    if (forecast) {
      return(sal)
    } else {
      return(inc)
    }
  })
  wage$i <- NULL
  cat("\nEstimated salary increase.\n")

  # Assign totHours
  wage$totHours <- sapply(wage$ID, FUN = function(x) {

    # index <- which(empID == x)
    # sum(listR[[index]]@totHours)

    # total days per year is 313 as advised by Accounting
    return(313 * 8)
  })

  # Compute hourly rates
  tempData <- apply(wage[, 2:6], MARGIN = 1, FUN = function(x) {

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

  wageEmp <- dplyr::left_join(x  = wageM,
                              y  = wageH,
                              by = c("ID", "sal"))

  wageEmp <- dplyr::left_join(x  = wageEmp,
                              y  = wage[,c(1,3)],
                              by = c("ID"))

  # Compute Salaries for monthly wagers

  cat("\nComputing salaries for non-RF.\n")

  mhDB.m <- mhDB[which(mhDB$scheme == "m"),
                 !colnames(mhDB) %in% c("scheme")]

  ## Separate Regular Employees
  mhDB.m.R <- mhDB.m[mhDB.m$status == "reg",
                     !colnames(mhDB.m) %in% c("sal", "status", "maxReg")]

  ### Get isOT.R
  mhDB.m.R <- dplyr::left_join(x = mhDB.m.R,
                               y = premium.nonRF[,c("isOT.R",
                                                    "premiumR",
                                                    "npR",
                                                    "mhType")],
                               by = c("mhType"))

  ### Separate non-OT
  mhDB.m.R.Reg <- mhDB.m.R[which(!mhDB.m.R$isOT.R),
                           !colnames(mhDB.m.R) %in% c("mhType",
                                                      "isOT.R",
                                                      "premiumR")]

  #### Get monthly wage minus absences

  mhDB.m.R.Reg.M <- mhDB.m.R.Reg %>%
    dplyr::group_by(ID, month) %>%
    dplyr::summarise(mhTot = sum(mh))

  mhDB.m.R.Reg.M <- dplyr::left_join(
    x  = mhDB.m.R.Reg.M,
    y  = unique(mhDB[, c("ID", "month", "maxReg")]),
    by = c("ID", "month")
  )

  ##### Get absences hours
  mhDB.m.R.Reg.M$abHours <- mhDB.m.R.Reg.M$maxReg - mhDB.m.R.Reg.M$mhTot

  ##### Get salary scheme
  mhDB.m.R.Reg.M <- dplyr::left_join(
    x  = mhDB.m.R.Reg.M,
    y  = unique(mhDB[,c("ID", "month", "sal")]),
    by = c("ID", "month")
  )

  ##### Get monthly and hourly salary
  mhDB.m.R.Reg.M <- dplyr::left_join(
    x  = mhDB.m.R.Reg.M,
    y  = wageEmp[, !colnames(wageEmp) %in% c("isRF")],
    by = c("ID", "sal")
  )

  ##### Get absences cost
  mhDB.m.R.Reg.M$abCost <- round(
    mhDB.m.R.Reg.M$salH * mhDB.m.R.Reg.M$abHours, digits = 2
  )

  ##### monthly wage minus absences = salMB
  mhDB.m.R.Reg.M$salMB <- mhDB.m.R.Reg.M$salM - mhDB.m.R.Reg.M$abCost

  #### Combine salMB
  mhDB.m.R.Reg <- dplyr::left_join(
    x  = mhDB.m.R.Reg,
    y  = mhDB.m.R.Reg.M[,c("ID", "month", "mhTot", "salH", "salMB")],
    by = c("ID", "month")
  )

  #### Get man hour fraction of each cost code per month
  mhDB.m.R.Reg$Xmh <- mhDB.m.R.Reg$mh / mhDB.m.R.Reg$mhTot

  #### Compute Costs
  #### These costs are purely worked
  mhDB.m.R.Reg$costWage <- round(mhDB.m.R.Reg$Xmh * mhDB.m.R.Reg$salMB,
                                 digits = 2)
  mhDB.m.R.Reg$costNP   <- round(
    mhDB.m.R.Reg$salH * mhDB.m.R.Reg$np * mhDB.m.R.Reg$npR, digits = 2)

  ### Separate OT

  mhDB.m.R.OT <- mhDB.m.R[which(mhDB.m.R$isOT.R),
                          !colnames(mhDB.m.R) %in% c("mhType")]

  #### Get salary scheme
  mhDB.m.R.OT <- dplyr::left_join(
    x  = mhDB.m.R.OT,
    y  = unique(mhDB[,c("ID", "month", "sal")]),
    by = c("ID", "month")
  )

  #### Get hourly wage
  mhDB.m.R.OT <- dplyr::left_join(
    x  = mhDB.m.R.OT,
    y  = wageEmp[, !colnames(wageEmp) %in% c("salM", "isRF")],
    by = c("ID", "sal")
  )

  #### Compute Costs
  mhDB.m.R.OT$costWage <- round(
    mhDB.m.R.OT$salH * mhDB.m.R.OT$mh * mhDB.m.R.OT$premiumR, digits = 2
  )

  mhDB.m.R.OT$costNP   <- round(
    mhDB.m.R.OT$salH * mhDB.m.R.OT$np * mhDB.m.R.OT$npR, digits = 2
  )

  ## Separate probationary employees

  mhDB.m.P <- mhDB.m[mhDB.m$status == "pro",
                     !colnames(mhDB.m) %in% c("sal", "status", "maxReg")]

  ### Get isOT.S
  mhDB.m.P <- dplyr::left_join(x  = mhDB.m.P,
                               y  = premium.nonRF[, c("isOT.S",
                                                      "premiumS",
                                                      "npS",
                                                      "mhType")],
                               by = "mhType")

  ### Separate non-OT
  mhDB.m.P.Reg <- mhDB.m.P[which(!mhDB.m.P$isOT.S),
                           !colnames(mhDB.m.P) %in% c("mhType",
                                                      "isOT.S",
                                                      "premiumS")]

  #### Get monthly wage minus absences

  mhDB.m.P.Reg.M <- mhDB.m.P.Reg %>%
    dplyr::group_by(ID, month) %>%
    dplyr::summarise(mhTot = sum(mh))

  mhDB.m.P.Reg.M <- dplyr::left_join(
    x  = mhDB.m.P.Reg.M,
    y  = unique(mhDB[, c("ID", "month", "maxReg")]),
    by = c("ID", "month")
  )

  ##### Get absences hours
  mhDB.m.P.Reg.M$abHours <- mhDB.m.P.Reg.M$maxReg - mhDB.m.P.Reg.M$mhTot

  ##### Get salary scheme
  mhDB.m.P.Reg.M <- dplyr::left_join(
    x  = mhDB.m.P.Reg.M,
    y  = unique(mhDB[, c("ID", "month", "sal")]),
    by = c("ID", "month")
  )

  ##### Get monthly and hourly salary
  mhDB.m.P.Reg.M <- dplyr::left_join(
    x = mhDB.m.P.Reg.M,
    y = wageEmp[, !colnames(wageEmp) %in% c("isRF")],
    by = c("ID", "sal")
  )

  ##### Get absences cost
  mhDB.m.P.Reg.M$abCost <- round(
    mhDB.m.P.Reg.M$salH * mhDB.m.P.Reg.M$abHours, digits = 2
  )

  ##### monthly wage minus absences = salMB
  mhDB.m.P.Reg.M$salMB <- mhDB.m.P.Reg.M$salM - mhDB.m.P.Reg.M$abCost

  #### Combine salMB
  mhDB.m.P.Reg <- dplyr::left_join(
    x  = mhDB.m.P.Reg,
    y  = mhDB.m.P.Reg.M[, c("ID", "month", "mhTot", "salH", "salMB")],
    by = c("ID", "month")
  )

  #### Get man hour fraction of each cost code per month
  mhDB.m.P.Reg$Xmh <- mhDB.m.P.Reg$mh / mhDB.m.P.Reg$mhTot

  #### Compute Costs
  #### These costs are purely worked
  mhDB.m.P.Reg$costWage <- round(mhDB.m.P.Reg$Xmh * mhDB.m.P.Reg$salMB,
                                 digits = 2)

  mhDB.m.P.Reg$costNP   <- round(
    mhDB.m.P.Reg$salH * mhDB.m.P.Reg$np * mhDB.m.P.Reg$npS, digits = 2
  )

  ### Separate OT
  mhDB.m.P.OT <- mhDB.m.P[which(mhDB.m.P$isOT.S),
                          !colnames(mhDB.m.P) %in% c("mhType")]

  #### Get salary scheme
  mhDB.m.P.OT <- dplyr::left_join(
    x  = mhDB.m.P.OT,
    y  = unique(mhDB[, c("ID", "month", "sal")]),
    by = c("ID", "month")
  )

  #### Get hourly wage
  mhDB.m.P.OT <- dplyr::left_join(
    x = mhDB.m.P.OT,
    y = wageEmp[, !colnames(wageEmp) %in% c("salM", "isRF")],
    by = c("ID", "sal")
  )

  #### Compute costs
  mhDB.m.P.OT$costWage <- round(
    mhDB.m.P.OT$salH * mhDB.m.P.OT$mh * mhDB.m.P.OT$premiumS, digits = 2
  )

  mhDB.m.P.OT$costNP   <- round(
    mhDB.m.P.OT$salH * mhDB.m.P.OT$np * mhDB.m.P.OT$npS, digits = 2
  )

  ## Separate seasonal employees

  mhDB.m.S <- mhDB.m[mhDB.m$status == "sea",
                     !colnames(mhDB.m) %in% c("sal", "status", "maxReg")]

  ### Get isOT.S
  mhDB.m.S <- dplyr::left_join(x  = mhDB.m.S,
                               y  = premium.nonRF[, c("isOT.S",
                                                      "premiumS",
                                                      "npS",
                                                      "mhType")],
                               by = "mhType")

  ### Separate non-OT
  mhDB.m.S.Reg <- mhDB.m.S[which(!mhDB.m.S$isOT.S),
                           !colnames(mhDB.m.S) %in% c("mhType",
                                                      "isOT.S",
                                                      "premiumS")]

  #### Get monthly wage minus absences

  mhDB.m.S.Reg.M <- mhDB.m.S.Reg %>%
    dplyr::group_by(ID, month) %>%
    dplyr::summarise(mhTot = sum(mh))

  mhDB.m.S.Reg.M <- dplyr::left_join(
    x  = mhDB.m.S.Reg.M,
    y  = unique(mhDB[, c("ID", "month", "maxReg")]),
    by = c("ID", "month")
  )

  ##### Get absences hours
  mhDB.m.S.Reg.M$abHours <- mhDB.m.S.Reg.M$maxReg - mhDB.m.S.Reg.M$mhTot

  ##### Get salary scheme
  mhDB.m.S.Reg.M <- dplyr::left_join(
    x  = mhDB.m.S.Reg.M,
    y  = unique(mhDB[, c("ID", "month", "sal")]),
    by = c("ID", "month")
  )

  ##### Get monthly and hourly salary
  mhDB.m.S.Reg.M <- dplyr::left_join(
    x  = mhDB.m.S.Reg.M,
    y  = wageEmp[, !colnames(wageEmp) %in% c("isRF")],
    by = c("ID", "sal")
  )

  ##### Get absences cost
  mhDB.m.S.Reg.M$abCost <- round(
    mhDB.m.S.Reg.M$salH * mhDB.m.S.Reg.M$abHours, digits = 2
  )

  ##### monthly wage minus absences = salMB
  mhDB.m.S.Reg.M$salMB <- mhDB.m.S.Reg.M$salM - mhDB.m.S.Reg.M$abCost

  #### Combine salMB
  mhDB.m.S.Reg <- dplyr::left_join(
    x  = mhDB.m.S.Reg,
    y  = mhDB.m.S.Reg.M[, c("ID", "month", "mhTot", "salH", "salMB")],
    by = c("ID", "month")
  )

  #### Get man hour fraction of each cost code per month
  mhDB.m.S.Reg$Xmh <- mhDB.m.S.Reg$mh / mhDB.m.S.Reg$mhTot

  #### Compute Costs
  #### These costs are purely worked
  mhDB.m.S.Reg$costWage <- round(mhDB.m.S.Reg$Xmh * mhDB.m.S.Reg$salMB,
                                 digits = 2)

  mhDB.m.S.Reg$costNP <- round(
    mhDB.m.S.Reg$salH * mhDB.m.S.Reg$np * mhDB.m.S.Reg$npS, digits = 2
  )

  ### Separate OT
  mhDB.m.S.OT <- mhDB.m.S[which(mhDB.m.S$isOT.S),
                          !colnames(mhDB.m.S) %in% c("mhType")]

  #### Get salary scheme
  mhDB.m.S.OT <- dplyr::left_join(
    x  = mhDB.m.S.OT,
    y  = unique(mhDB[, c("ID", "month", "sal")]),
    by = c("ID", "month")
  )

  #### Get hourly wage
  mhDB.m.S.OT <- dplyr::left_join(
    x  = mhDB.m.S.OT,
    y  = wageEmp[, !colnames(wageEmp) %in% c("salM", "isRF")],
    by = c("ID", "sal")
  )

  #### Compute costs
  mhDB.m.S.OT$costWage <- round(
    mhDB.m.S.OT$salH * mhDB.m.S.OT$mh * mhDB.m.S.OT$premiumS, digits = 2
  )

  mhDB.m.S.OT$costNP   <- round(
    mhDB.m.S.OT$salH * mhDB.m.S.OT$np * mhDB.m.S.OT$npS, digits = 2
  )

  # Compute Salaries for daily wagers or RF

  cat("\nComputing salaries for RF.\n")

  mhDB.d <- mhDB[which(mhDB$scheme == "d"),
                 !colnames(mhDB) %in% c("scheme")]

  ## Get hourly salary
  mhDB.d <- dplyr::left_join(
    x  = mhDB.d,
    y  = wageEmp[, !colnames(wageEmp) %in% c("salM", "isRF")],
    by = c("ID", "sal")
  )

  ## Separate Regular Employees
  mhDB.d.R <- mhDB.d[mhDB.d$status == "reg",
                     !colnames(mhDB.d) %in% c("status", "maxReg")]

  ### Get premium
  mhDB.d.R <- dplyr::left_join(
    x  = mhDB.d.R,
    y  = premium.RF[, c("isOT.R", "premiumR", "npR", "mhType")],
    by = "mhType"
  )

  ### Get cost
  mhDB.d.R$costWage <- round(
    mhDB.d.R$salH * mhDB.d.R$mh * mhDB.d.R$premiumR, digits = 2
  )

  mhDB.d.R$costNP   <- round(
    mhDB.d.R$salH * mhDB.d.R$np * mhDB.d.R$npR, digits = 2
  )

  ### Separate non-OT
  mhDB.d.R.Reg <- mhDB.d.R[which(!mhDB.d.R$isOT.R),]

  ### Separate OT
  mhDB.d.R.OT <- mhDB.d.R[which(mhDB.d.R$isOT.R),]

  ## Separate probationary employees
  mhDB.d.P <- mhDB.d[mhDB.d$status == "pro",
                     !colnames(mhDB.d) %in% c("status", "maxReg")]

  ### Get premium
  mhDB.d.P <- dplyr::left_join(
    x  = mhDB.d.P,
    y  = premium.RF[, c("isOT.S", "premiumS", "npS", "mhType")],
    by = "mhType"
  )

  ### Get cost
  mhDB.d.P$costWage <- round(
    mhDB.d.P$salH * mhDB.d.P$mh * mhDB.d.P$premiumS, digits = 2
  )

  mhDB.d.P$costNP   <- round(
    mhDB.d.P$salH * mhDB.d.P$np * mhDB.d.P$npS , digits = 2
  )

  ### Separate non-OT
  mhDB.d.P.Reg <- mhDB.d.P[which(!mhDB.d.P$isOT.S),]

  ### Separate OT
  mhDB.d.P.OT <- mhDB.d.P[which(mhDB.d.P$isOT.S),]

  ## Separate seasonal employees
  mhDB.d.S <- mhDB.d[mhDB.d$status == "sea",
                     !colnames(mhDB.d) %in% c("status", "maxReg")]

  ### Get premium
  mhDB.d.S <- dplyr::left_join(
    x  = mhDB.d.S,
    y  = premium.RF[, c("isOT.S", "premiumS", "npS", "mhType")],
    by = "mhType"
  )

  ### Get cost
  mhDB.d.S$costWage <- round(
    mhDB.d.S$salH * mhDB.d.S$mh * mhDB.d.S$premiumS, digits = 2
  )

  mhDB.d.S$costNP   <- round(
    mhDB.d.S$salH * mhDB.d.S$np * mhDB.d.S$npS, digits = 2
  )

  ### Separate non-OT
  mhDB.d.S.Reg <- mhDB.d.S[which(!mhDB.d.S$isOT.S),]

  ### Separate OT
  mhDB.d.S.OT <- mhDB.d.S[which(mhDB.d.S$isOT.S),]

  ## Separate agency employees
  mhDB.d.A <- mhDB.d[mhDB.d$status == "age",
                     !colnames(mhDB.d) %in% c("status", "maxReg")]

  ### Get premium
  mhDB.d.A <- dplyr::left_join(
    x  = mhDB.d.A,
    y  = premium.RF[, c("isOT.S", "premiumS", "npS", "mhType")],
    by = "mhType"
  )

  ### Get cost
  mhDB.d.A$costWage <- round(
    mhDB.d.A$salH * mhDB.d.A$mh * mhDB.d.A$premiumS, digits = 2
  )

  mhDB.d.A$costNP   <- round(
    mhDB.d.A$salH * mhDB.d.A$np * mhDB.d.A$npS, digits = 2
  )

  mhDB.d.A$cost <- mhDB.d.A$costWage + mhDB.d.A$costNP

  # Distribute holHours

  hol.mhDB <- mhDB[mhDB$mhType == "reg",
                   !colnames(mhDB) %in% c("mhType", "np", "scheme", "maxReg")]

  hol.mhDB <- dplyr::left_join(
    x  = hol.mhDB,
    y  = unique(wageEmp[, colnames(wageEmp) %in% c("ID", "isRF")]),
    by = "ID"
  )

  hol.mhDB.m <- hol.mhDB[!hol.mhDB$isRF, ]
  hol.mhDB.d <- hol.mhDB[hol.mhDB$isRF,  ]

  ## Create data.frame of holHours
  holHours <- lapply(listR, FUN = function(x) {
    data.frame(ID               = x@ID,
               month            = 1:12,
               holHours         = x@holHours,
               stringsAsFactors = FALSE)
  })

  holHours <- data.table::rbindlist(holHours)

  ## Get sal
  holHours <- dplyr::left_join(
    x  = holHours,
    y  = unique(mhDB[, c("ID", "month", "sal")]),
    by = c("ID", "month")
  )

  ## Get salH
  holHours <- dplyr::left_join(
    x  = holHours,
    y  = wageEmp[, !colnames(wageEmp) %in% c("salM")],
    by = c("ID", "sal")
  )

  holHours.m <- holHours[!holHours$isRF, ]
  holHours.d <- holHours[holHours$isRF,  ]

  holHours.d$costWage <- holHours.d$salH * holHours.d$holHours

  ## Merge costWage and holHours to hol.mhDB.d
  hol.mhDB.d <- dplyr::left_join(
    x  = hol.mhDB.d,
    y  = unique(holHours.d[, colnames(holHours.d) %in% c("ID",
                                                         "month",
                                                         "holHours",
                                                         "costWage")]),
    by = c("ID", "month")
  )

  hol.mhDB.m <- dplyr::left_join(
    x  = hol.mhDB.m,
    y  = holHours.m[, colnames(holHours.m) %in% c("ID", "month", "holHours")],
    by = c("ID", "month")
  )

  ## Compute for total mh per month per employee
  hol.mhDB.d <- hol.mhDB.d %>%
    dplyr::group_by(ID, month) %>%
    dplyr::mutate(totHours = sum(mh))

  hol.mhDB.m <- hol.mhDB.m %>%
    dplyr::group_by(ID, month) %>%
    dplyr::mutate(totHours = sum(mh))

  ## Compute mh fraction
  hol.mhDB.d$X <- hol.mhDB.d$mh / hol.mhDB.d$totHours
  hol.mhDB.m$X <- hol.mhDB.m$mh / hol.mhDB.m$totHours

  ## Compute for distributed holHours and costWage
  hol.mhDB.d$XholHours <- hol.mhDB.d$X * hol.mhDB.d$holHours
  hol.mhDB.m$XholHours <- hol.mhDB.m$X * hol.mhDB.m$holHours
  hol.mhDB.d$XcostWage <- round(hol.mhDB.d$X * hol.mhDB.d$costWage, digits = 2)

  ## Separate employees by status
  hol.mhDB.d.R <- hol.mhDB.d[hol.mhDB.d$status %in% c("reg", "pro"),]
  hol.mhDB.d.S <- hol.mhDB.d[hol.mhDB.d$status == "sea",]
  hol.mhDB.d.A <- hol.mhDB.d[hol.mhDB.d$status == "age",]

  # Compute for Employee Allowances
  cat("\nComputing employee allowances.\n")

  allowance  <- data.table::rbindlist(lapply(listR, getAllowance))
  mhDB.allow <- mhDB[mhDB$mhType %in% distType, ] %>%
    dplyr::group_by(ID, month, costCode) %>%
    dplyr::summarise(mh = sum(mh))

  mhDB.allow <- mhDB.allow %>%
    dplyr::group_by(ID, month) %>%
    dplyr::mutate(totMH = sum(mh))

  mhDB.allow <- dplyr::left_join(mhDB.allow, allowance, by = c("ID", "month"))

  mhDB.allow$X    <- mhDB.allow$mh / mhDB.allow$totMH
  mhDB.allow$cost <- round(mhDB.allow$X * mhDB.allow$allowance, digits = 2)

  # Compute for Safety Gadgets
  cat("\nComputing safety gadgets.\n")

  safetyGadgets <- data.table::rbindlist(lapply(listR, getSafetyGadgets))
  mhDB.safetyGadgets <- mhDB[mhDB$mhType %in% distType, ] %>%
    dplyr::group_by(ID, month, costCode) %>%
    dplyr::summarise(mh = sum(mh))

  mhDB.safetyGadgets <- mhDB.safetyGadgets %>%
    dplyr::group_by(ID, month) %>%
    dplyr::mutate(totMH = sum(mh))

  mhDB.safetyGadgets <- dplyr::left_join(mhDB.safetyGadgets,
                                         safetyGadgets,
                                         by = c("ID", "month"))
  mhDB.safetyGadgets$X <- mhDB.safetyGadgets$mh / mhDB.safetyGadgets$totMH
  mhDB.safetyGadgets$cost <- round(mhDB.safetyGadgets$X * mhDB.safetyGadgets$sg,
                                   digits = 2)

  # Compute for Group Life Insurance
  cat("\nComputing for group life insurance.\n")

  groupLife <- data.table::rbindlist(lapply(listR, getGroupLife))
  mhDB.groupLife <- mhDB[mhDB$mhType %in% distType, ] %>%
    dplyr::group_by(ID, month, costCode) %>%
    dplyr::summarise(mh = sum(mh))

  mhDB.groupLife <- mhDB.groupLife %>%
    dplyr::group_by(ID, month) %>%
    dplyr::mutate(totMH = sum(mh))

  mhDB.groupLife <- dplyr::left_join(mhDB.groupLife,
                                     groupLife,
                                     by = c("ID", "month"))
  mhDB.groupLife$X <- mhDB.groupLife$mh / mhDB.groupLife$totMH
  mhDB.groupLife$cost <- round(mhDB.groupLife$X * mhDB.groupLife$gl, digits = 2)

  # Compute for HMO
  cat("\nComputing for HMO.\n")

  hmo <- data.table::rbindlist(lapply(listR, getHMO))
  mhDB.hmo <- mhDB[mhDB$mhType %in% distType, ] %>%
    dplyr::group_by(ID, month, costCode) %>%
    dplyr::summarise(mh = sum(mh))

  mhDB.hmo <- mhDB.hmo %>%
    dplyr::group_by(ID, month) %>%
    dplyr::mutate(totMH = sum(mh))

  mhDB.hmo <- dplyr::left_join(mhDB.hmo, hmo, by = c("ID", "month"))
  mhDB.hmo$X <- mhDB.hmo$mh / mhDB.hmo$totMH
  mhDB.hmo$cost <- round(mhDB.hmo$X * mhDB.hmo$hmo, digits = 2)

  # Employee benefits
  # cat("\nComputing for employee benefits.\n")

  benefits <- data.table::rbindlist(list(
    data.table::rbindlist(lapply(listR, getCBA)),
    data.table::rbindlist(lapply(listR, getLongShirt)),
    data.table::rbindlist(lapply(listR, getLaborDayShirt)),
    data.table::rbindlist(lapply(listR, getGC))
  )) %>%
    dplyr::group_by(month, ID) %>%
    dplyr::summarise(benefits = sum(benefits))

  mhDB.benefits <- mhDB[mhDB$mhType %in% distType, ] %>%
    dplyr::group_by(ID, month, costCode) %>%
    dplyr::summarise(mh = sum(mh)) %>%
    dplyr::group_by(ID, month) %>%
    dplyr::mutate(totMH = sum(mh))

  mhDB.benefits <- dplyr::left_join(mhDB.benefits,
                                    benefits,
                                    by = c("ID", "month"))
  mhDB.benefits$X <- mhDB.benefits$mh / mhDB.benefits$totMH
  mhDB.benefits$cost <- round(mhDB.benefits$X * mhDB.benefits$benefits,
                              digits = 2)

  # Compute for Safety Bonus
  cat("\nComputing safety bonus.\n")

  mhDB.SB <- mhDB %>%
    dplyr::group_by(costCode, month) %>%
    dplyr::summarise(mh = sum(mh))

  mhDB.SB$cost <- round(mhDB.SB$mh * 0.2, digits = 2)

  mhDB.SB$costCodeNew <- sapply(
    mhDB.SB$costCode,
    FUN = function(x) {

      if (grepl("13100", x = x))
        return("13100")

      if (x == "0-0")
        return("0-0")

      if (grepl("14\\d00", x = x))
        return("14000")

      return("1100")
    }
  )

  mhDB.SB$costCode <- mhDB.SB$costCodeNew

  # Compute for SSS contribution of employer
  cat("\nComputing SSS contribution.\n")

  #- SSS Contribution is fixed
  #- For monthly wagers, SSS is based on basic monthly salary
  #- For daily wagers, SSS is based on daily wage * 313 / 12

  SSSdb <- lapply(listR, FUN = function(x) {

    tempData <- getCM(x)

    if (!isReg(x)) {
      tempData$sal <- "a"
    } else {
      if (isRF(x)) {
        tempData <- dplyr::left_join(x = tempData, y = payB, by = "month")
      } else {
        tempData <- dplyr::left_join(x = tempData, y = payA, by = "month")
      }
    }

    if (isRF(x)) {

      tempData <- dplyr::left_join(
        x  = tempData,
        y  = wageEmp[wageEmp$isRF, !colnames(wageEmp) %in% c("salH", "isRF")],
        by = c("ID", "sal"))

      tempData$salM2 <- round(tempData$salM * 313 / 12, digits = 2)
      tempData$salM  <- tempData$salM2
      tempData       <- tempData[, !colnames(tempData) %in% c("salM2")]

    } else {

      tempData <- dplyr::left_join(
        x  = tempData,
        y  = wageEmp[!wageEmp$isRF, !colnames(wageEmp) %in% c("salH", "isRF")],
        by = c("ID", "sal")
      )
    }

    tempData$salG <- round(tempData$salM * tempData$allow, digits = 2)

    tempData$SSS <- sapply(tempData$salG, FUN = function(x) {
      SSS$c[which(SSS$r1 <= x & SSS$r2 >= x)]
    })

    tempData <- tempData[, colnames(tempData) %in% c("month", "ID", "SSS")]
    tempData <- as.data.frame(tempData)

    tempData
  })

  SSSdb <- data.table::rbindlist(SSSdb)

  mhDB.SSS <- mhDB[mhDB$mhType %in% distType, ] %>%
    dplyr::group_by(ID, month, costCode, status) %>%
    dplyr::summarise(mh = sum(mh))

  mhDB.SSS <- mhDB.SSS %>%
    dplyr::group_by(ID, month, status) %>%
    dplyr::mutate(totMH = sum(mh))

  mhDB.SSS$X    <- mhDB.SSS$mh / mhDB.SSS$totMH
  mhDB.SSS      <- dplyr::left_join(x = mhDB.SSS,
                                    y = SSSdb,
                                    by = c("ID", "month"))

  mhDB.SSS$cost <- round(mhDB.SSS$X * mhDB.SSS$SSS, digits = 2)
  mhDB.SSS.A    <- mhDB.SSS[mhDB.SSS$status == "age", ]
  mhDB.SSS      <- mhDB.SSS[mhDB.SSS$status != "age", ]

  # Compute for Pag-ibig contribution of employer
  cat("\nComputing Pag-ibig contributions.\n")

  mhDB.PI <- mhDB[mhDB$mhType %in% distType, ] %>%
    dplyr::group_by(ID, month, costCode, status) %>%
    dplyr::summarise(mh = sum(mh))

  mhDB.PI$PI <- 100
  mhDB.PI    <- mhDB.PI %>%
    dplyr::group_by(ID, month, status) %>%
    dplyr::mutate(totMH = sum(mh))

  mhDB.PI$X    <- mhDB.PI$mh / mhDB.PI$totMH
  mhDB.PI$cost <- round(mhDB.PI$X * mhDB.PI$PI, digits = 2)
  mhDB.PI.A    <- mhDB.PI[mhDB.PI$status == "age", ]
  mhDB.PI      <- mhDB.PI[mhDB.PI$status != "age", ]

  # Compute for Phil-Health contribution of employer
  cat("\nComputing Philhealth contribution.\n")

  PHdb <- lapply(listR, FUN = function(x) {

    tempData <- getCM(x)

    if (!isReg(x)) {
      tempData$sal <- "a"
    } else {
      if (isRF(x)) {
        tempData <- dplyr::left_join(x = tempData, y = payB, by = "month")
      } else {
        tempData <- dplyr::left_join(x = tempData, y = payA, by = "month")
      }
    }

    if (isRF(x)) {

      tempData <- dplyr::left_join(
        tempData,
        wageEmp[wageEmp$isRF, !colnames(wageEmp) %in% c("salH", "isRF")],
        by = c("ID", "sal"))

      tempData$salM2 <- round(tempData$salM * 313 / 12, digits = 2)
      tempData$salM  <- tempData$salM2
      tempData       <- tempData[, !colnames(tempData) %in% c("salM2")]
    } else {

      tempData <- dplyr::left_join(
        x  = tempData,
        y  = wageEmp[!wageEmp$isRF, !colnames(wageEmp) %in% c("salH", "isRF")],
        by = c("ID", "sal")
      )
    }

    tempData$salG <- round(tempData$salM * tempData$allow, digits = 2)

    tempData$PH <- sapply(tempData$salG, FUN = function(x) {
      PHIC$c[which(PHIC$r1 <= x & PHIC$r2 >= x)]
    })

    tempData <- tempData[, colnames(tempData) %in% c("month", "ID", "PH")]
    tempData <- as.data.frame(tempData)

    tempData
  })

  PHdb <- data.table::rbindlist(PHdb)

  mhDB.PH <- mhDB[mhDB$mhType %in% distType, ] %>%
    dplyr::group_by(ID, month, costCode, status) %>%
    dplyr::summarise(mh = sum(mh))

  mhDB.PH <- mhDB.PH %>%
    dplyr::group_by(ID, month, status) %>%
    dplyr::mutate(totMH = sum(mh))

  mhDB.PH$X    <- mhDB.PH$mh / mhDB.PH$totMH
  mhDB.PH      <- dplyr::left_join(x = mhDB.PH, y = PHdb, by = c("ID", "month"))
  mhDB.PH$cost <- round(mhDB.PH$X * mhDB.PH$PH, digits = 2)

  mhDB.PH.A    <- mhDB.PH[mhDB.PH$status == "age", ]
  mhDB.PH      <- mhDB.PH[mhDB.PH$status != "age", ]

  # Compute for Leave Commutation
  cat("\nComputing leave commutation.\n")

  ## Separate regular employees

  ### Extract maximum regular hours for each employee
  maxRegDB <- lapply(listR[sapply(listR, isReg)], FUN = function(x) {
    data.frame(ID               = x@ID,
               month            = 1:12,
               maxReg           = x@maxReg,
               stringsAsFactors = FALSE)
  })

  ### Get total reg hours attendance (RH)
  mhDB.RH <- mhDB[mhDB$status == "reg" & mhDB$mhType == "reg", ] %>%
    dplyr::group_by(ID, month, sal) %>%
    dplyr::summarise(mh = sum(mh))

  ### Join RH to maxRegDB then compute leave hours (LH) per month
  tempID   <- sapply(listR, FUN = function(x) {x@ID})
  maxRegDB <- lapply(maxRegDB, FUN = function(z) {

    z <- dplyr::left_join(x = z, y = mhDB.RH, by = c("ID", "month"))

    z[is.na(z)] <- 0L
    z$absence <- z$maxReg - z$mh

    if (any(z$absence) < 0)
      stop("Absence must not be less than 0!")

    tempIndex <- which(tempID == z$ID[1])
    LC        <- listR[[tempIndex]]@leaveHours
    z$LH      <- 0

    for (i in 1:12) {
      if (LC > 0) {
        minus   <- min(z$absence[i], LC)
        z$LH[i] <- minus
        LC      <- LC - minus
      } else {
        break
      }
    }

    z$LH[4] <- z$LH[4] + LC

    if (sum(z$LH) != listR[[tempIndex]]@leaveHours)
      stop("Leave hours do not match!")

    return(z)
  })

  maxRegDB <- data.table::rbindlist(maxRegDB)

  ### Compute LC cost
  maxRegDB <- dplyr::left_join(
    x  = maxRegDB,
    y  = wageEmp[, !colnames(wageEmp) %in% c("salM", "isRF")],
    by = c("ID", "sal")
  )
  maxRegDB$LC <- maxRegDB$salH * maxRegDB$LH

  ### Distribute LC cost for regular employees
  mhDB.LC.R <-mhDB[mhDB$mhType %in% distType & mhDB$status == "reg", ] %>%
    dplyr::group_by(ID, month, costCode) %>%
    dplyr::summarise(mh = sum(mh))

  mhDB.LC.R <- mhDB.LC.R %>%
    dplyr::group_by(ID, month) %>%
    dplyr::mutate(totMH = sum(mh))

  mhDB.LC.R$X <- mhDB.LC.R$mh / mhDB.LC.R$totMH
  mhDB.LC.R   <- dplyr::left_join(
    x  = mhDB.LC.R,
    y  = maxRegDB[, colnames(maxRegDB) %in% c("ID", "month", "LC")],
    by = c("ID", "month")
  )

  mhDB.LC.R$cost <- round(mhDB.LC.R$X * mhDB.LC.R$LC, digits = 2)

  ## Separate seasonal and agency employees
  ##- These must be separated since they are not entitled to negotiated
  ##- holiday premium.
  ##- Probationary employees have no leave commutation.

  ### Get leave hours
  LH <- lapply(listR[sapply(listR, FUN = function(x) {
    x@status %in% c("sea", "age")
  })], FUN = function(x) {
    data.frame(ID  = x@ID,
               LH  = x@leaveHours,
               sal = "a",
               stringsAsFactors = FALSE)
  })
  LH <- data.table::rbindlist(LH)

  ### Get LC cost per employee for the whole year
  LH <- dplyr::left_join(x  = LH,
                         y  = wageEmp[ ,
                                       !colnames(wageEmp) %in% c("salM",
                                                                 "isRF")],
                         by = c("ID", "sal"))

  LH$LC <- LH$LH * LH$salH

  ### Distribute LC throughout the year
  mhDB.LC.S <- mhDB[mhDB$status %in% c("sea", "age") &
                      mhDB$mhType %in% distType, ] %>%
    dplyr::group_by(ID, month, costCode, status) %>%
    dplyr::summarise(mh = sum(mh))

  mhDB.LC.S <- mhDB.LC.S %>%
    dplyr::group_by(ID) %>%
    dplyr::mutate(totMH = sum(mh))

  mhDB.LC.S$X <- mhDB.LC.S$mh / mhDB.LC.S$totMH
  mhDB.LC.S   <- dplyr::left_join(
    x  = mhDB.LC.S,
    y  = LH[, colnames(LH) %in% c("ID", "LC")],
    by = "ID"
  )

  mhDB.LC.S$cost <- round(mhDB.LC.S$X * mhDB.LC.S$LC, digits = 2)

  ## Get cost
  mhDB.LC <- data.table::rbindlist(l = list(
    mhDB.LC.R[, colnames(mhDB.LC.R) %in% c("costCode", "month", "cost")],
    mhDB.LC.S[mhDB.LC.S$status == "sea",
              colnames(mhDB.LC.S) %in% c("costCode", "month", "cost")]
  ))

  mhDB.LC.A <- mhDB.LC.S[mhDB.LC.S$status == "age",
                         colnames(mhDB.LC.S) %in% c("costCode",
                                                    "month",
                                                    "cost")]

  # Compute for Hospital and Medical Expenses
  cat("\nComputing hospital and medical expenses.\n")

  hm <- data.table::rbindlist(lapply(listR, getHM))

  mhDB.HM <- mhDB[mhDB$mhType %in% distType, ] %>%
    dplyr::group_by(ID, month, costCode, status) %>%
    dplyr::summarise(mh = sum(mh))

  mhDB.HM <- mhDB.HM %>%
    dplyr::group_by(ID, month, status) %>%
    dplyr::mutate(totMH = sum(mh))

  mhDB.HM      <- dplyr::left_join(x = mhDB.HM, y = hm, by = c("ID", "month"))
  mhDB.HM$X    <- mhDB.HM$mh / mhDB.HM$totMH
  mhDB.HM$cost <- round(mhDB.HM$X * mhDB.HM$HM, digits = 2)

  mhDB.HM.A <- mhDB.HM[mhDB.HM$status == "age", ]
  mhDB.HM   <- mhDB.HM[mhDB.HM$status != "age", ]

  # Compute for 13th month pay
  cat("\nComputing 13th month pay.\n")

  mp13 <- data.table::rbindlist(lapply(listR, FUN = function(x) {

    tempIndex <- which(wageEmp$ID == x@ID)
    tempSal   <- wageEmp$salM[tempIndex]
    tempData  <- get13mp(theObject = x, sal = tempSal)

    return(tempData)
  }))

  mhDB.13mp <- mhDB[mhDB$mhType %in% distType, ] %>%
    dplyr::group_by(ID, month, costCode, status) %>%
    dplyr::summarise(mh = sum(mh))

  mhDB.13mp <- mhDB.13mp %>%
    dplyr::group_by(ID, month, status) %>%
    dplyr::mutate(totMH = sum(mh))

  mhDB.13mp$X <- mhDB.13mp$mh / mhDB.13mp$totMH

  mhDB.13mp <- dplyr::left_join(x = mhDB.13mp, y = mp13, by = c("ID", "month"))

  mhDB.13mp$cost <- round(mhDB.13mp$X * mhDB.13mp$mp, digits = 2)
  mhDB.13mp      <- mhDB.13mp %>%
    dplyr::group_by(costCode, status, month) %>%
    dplyr::summarise(cost = sum(cost))

  mhDB.13mp.A <- mhDB.13mp[mhDB.13mp$status == "age", ]
  mhDB.13mp   <- mhDB.13mp[mhDB.13mp$status != "age", ]

  if (forecast) {
    mhDB.13mp$costCodeNew <- sapply(
      mhDB.13mp$costCode,
      FUN = function(x) {

        if (grepl("13100", x = x))
          return("13100")

        if (x == "0-0")
          return("0-0")

        if (grepl("14\\d00", x = x))
          return("14000")

        return("1100")
      }
    )

    mhDB.13mp$costCode <- mhDB.13mp$costCodeNew
    mhDB.13mp          <- mhDB.13mp %>%
      dplyr::group_by(costCode, status, month) %>%
      dplyr::summarise(cost = sum(cost))
  }

  ## Separate 13th month pay for regular and non-regular in-house as requested
  ## by accounting

  accr.13mp <- mhDB.13mp %>%
    dplyr::group_by(status, costCode, month) %>%
    dplyr::summarise(cost = sum(cost)) %>%
    tidyr::spread(month, cost, fill = 0)

  # Sum all man-hours

  mhDB.mh1 <- mhDB %>%
    dplyr::group_by(costCode, month) %>%
    dplyr::summarise(cost = sum(mh))

  mhDB.mh2 <- hol.mhDB.m %>%
    dplyr::group_by(costCode, month) %>%
    dplyr::summarise(cost = sum(XholHours))

  mhDB.mh3 <- hol.mhDB.d %>%
    dplyr::group_by(costCode, month) %>%
    dplyr::summarise(cost = sum(XholHours))

  mhDB.mh <- data.table::rbindlist(list(mhDB.mh1,
                                        mhDB.mh2,
                                        mhDB.mh3))

  # Compute for Mid-year and Year-end bonus
  cat("\nComputing bonus.\n")

  if (forecast) {
    bonusFactor <- 1.5
  } else {
    bonusFactor <- 2
  }

  # FIXME: Bonus must pro-rated
  # The date hired (probationary or regular, which ever came first) of the
  #   employee is used in pro-rating.
  # The to fully enjoy the bonus for the year, the date of hiring must be not
  #   not later than January 01.

  bonus <- lapply(listR, FUN = function(x) {

    if (!isReg(x))
      return(NULL)

    tempData <- getCM(x)

    if (isRF(x)) {

      tempData <- dplyr::left_join(x = tempData, y = payB, by = "month")
      tempData <- dplyr::left_join(
        x  = tempData,
        y  = wageEmp[wageEmp$isRF, !colnames(wageEmp) %in% c("salH", "isRF")],
        by = c("ID", "sal")
      )

      tempData$salM2 <- round(tempData$salM * 26, digits = 2)
      tempData$salM  <- tempData$salM2
      tempData       <- tempData[, !colnames(tempData) %in% c("salM2")]

    } else {

      tempData <- dplyr::left_join(x = tempData, y = payA, by = "month")
      tempData <- dplyr::left_join(
        x  = tempData,
        y  = wageEmp[!wageEmp$isRF, !colnames(wageEmp) %in% c("salH", "isRF")],
        by = c("ID", "sal")
      )

    }

    tempData$salG  <- round(tempData$salM * tempData$allow, digits = 2)
    tempData$bonus <-
      tempData$salG * c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, bonusFactor)

    tempData <- tempData[, colnames(tempData) %in% c("month", "ID", "bonus")]
    tempData <- as.data.frame(tempData)

    return(tempData)
  })

  bonus <- data.table::rbindlist(bonus)

  mhDB.bonus <- mhDB[mhDB$mhType %in% distType, ] %>%
    dplyr::group_by(ID, month, costCode) %>%
    dplyr::summarise(mh = sum(mh))

  mhDB.bonus <- mhDB.bonus %>%
    dplyr::group_by(ID, month) %>%
    dplyr::mutate(totMH = sum(mh))

  mhDB.bonus$X <- mhDB.bonus$mh / mhDB.bonus$totMH
  mhDB.bonus   <- dplyr::left_join(x  = mhDB.bonus,
                                   y  = bonus,
                                   by = c("ID", "month"))

  mhDB.bonus$cost <- round(mhDB.bonus$X * mhDB.bonus$bonus, digits = 2)
  mhDB.bonus      <- mhDB.bonus[!is.na(mhDB.bonus$cost),]
  mhDB.bonus      <- mhDB.bonus %>%
    dplyr::group_by(costCode, month) %>%
    dplyr::summarise(cost = sum(cost))

  if (forecast) {
    mhDB.bonus$costCodeNew <- sapply(
      mhDB.bonus$costCode,
      FUN = function(x) {

        if (grepl("13100", x = x))
          return("13100")

        if (x == "0-0")
          return("0-0")

        if (grepl("14\\d00", x = x))
          return("14000")

        return("1100")
      }
    )

    mhDB.bonus$costCode <- mhDB.bonus$costCodeNew
    mhDB.bonus          <- mhDB.bonus %>%
      dplyr::group_by(costCode, month) %>%
      dplyr::summarise(cost = sum(cost)) %>%
      tidyr::spread(month, cost, fill = 0)
  }

  # Compute for Rice Subsidy for Agency
  cat("\nComputing rice subsidy (agency).\n")

  listR.A <- listR[sapply(listR, function(x) {return(x@status == "age")})]

  if (length(listR.A) > 0) {

    ## Get rice subsidy per month per employee
    riceSub.A <- lapply(listR.A, FUN = getRiceSub)
    riceSub.A <- data.table::rbindlist(riceSub.A)

    ## Distribute rice subsidy
    mhDB.riceSub.A <- mhDB[mhDB$mhType %in% distType &
                             mhDB$status == "age",] %>%
      dplyr::group_by(ID, month, costCode) %>%
      dplyr::summarise(mh = sum(mh))

    mhDB.riceSub.A <- mhDB.riceSub.A %>%
      dplyr::group_by(ID, month) %>%
      dplyr::mutate(totMH = sum(mh))

    mhDB.riceSub.A$X <- mhDB.riceSub.A$mh / mhDB.riceSub.A$totMH
    mhDB.riceSub.A   <- dplyr::left_join(x  = mhDB.riceSub.A,
                                         y  = riceSub.A,
                                         by = c("ID", "month"))

    mhDB.riceSub.A$cost <- mhDB.riceSub.A$X * mhDB.riceSub.A$riceSub
  } else {
    mhDB.riceSub.A <- NULL
  }

  # Compute for Rice Subsidy for in-house
  cat("\nComputing rice subsidy (in-house).\n")

  listR.I <- listR[sapply(listR, function(x) {return(x@status != "age")})]

  if (length(listR.I) > 0) {

    ## Get rice subsidy per month per employee
    riceSub.I <- lapply(listR.I, FUN = getRiceSub)
    riceSub.I <- data.table::rbindlist(riceSub.I)

    ## Distribute rice subsidy
    mhDB.riceSub.I <- mhDB[mhDB$mhType %in% distType &
                             mhDB$status != "age",] %>%
      dplyr::group_by(ID, month, costCode) %>%
      dplyr::summarise(mh = sum(mh)) %>%
      dplyr::group_by(ID, month) %>%
      dplyr::mutate(totMH = sum(mh))

    mhDB.riceSub.I$X <- mhDB.riceSub.I$mh / mhDB.riceSub.I$totMH
    mhDB.riceSub.I <-
      dplyr::left_join(x = mhDB.riceSub.I, y = riceSub.I, by = c("ID", "month"))
    mhDB.riceSub.I$cost <- mhDB.riceSub.I$X * mhDB.riceSub.I$riceSub
  } else {
    mhDB.riceSub.I <- NULL
  }

  cat("\nMerging costs.\n")

  # Salaries-Regular
  r01.01 <- data.frame(costCode         = mhDB.m.R.Reg$costCode,
                       month            = mhDB.m.R.Reg$month,
                       cost             = mhDB.m.R.Reg$costWage,
                       stringsAsFactors = FALSE)

  r01.02 <- data.frame(costCode         = mhDB.d.R.Reg$costCode,
                       month            = mhDB.d.R.Reg$month,
                       cost             = mhDB.d.R.Reg$costWage,
                       stringsAsFactors = FALSE)

  r01.03 <- data.frame(costCode         = hol.mhDB.d.R$costCode,
                       month            = hol.mhDB.d.R$month,
                       cost             = hol.mhDB.d.R$XcostWage,
                       stringsAsFactors = FALSE)

  r01.04 <- data.frame(costCode         = mhDB.m.P.Reg$costCode,
                       month            = mhDB.m.P.Reg$month,
                       cost             = mhDB.m.P.Reg$costWage,
                       stringsAsFactors = FALSE)

  r01.05 <- data.frame(costCode         = mhDB.d.P.Reg$costCode,
                       month            = mhDB.d.P.Reg$month,
                       cost             = mhDB.d.P.Reg$costWage,
                       stringsAsFactors = FALSE)

  r01 <- as.data.frame(data.table::rbindlist(l = list(r01.01,
                                                      r01.02,
                                                      r01.03,
                                                      r01.04,
                                                      r01.05)))

  if (nrow(r01) > 0) {
    r01$row <- "Salaries-Regular"
  } else {
    r01 <- NULL
  }

  # OT Pay - Regular
  r02.01 <- data.frame(costCode         = mhDB.m.R.Reg$costCode,
                       month            = mhDB.m.R.Reg$month,
                       cost             = mhDB.m.R.Reg$costNP,
                       stringsAsFactors = FALSE)

  r02.02 <- data.frame(costCode         = mhDB.m.R.OT$costCode,
                       month            = mhDB.m.R.OT$month,
                       cost             = mhDB.m.R.OT$costWage,
                       stringsAsFactors = FALSE)

  r02.03 <- data.frame(costCode         = mhDB.m.R.OT$costCode,
                       month            = mhDB.m.R.OT$month,
                       cost             = mhDB.m.R.OT$costNP,
                       stringsAsFactors = FALSE)

  r02.04 <- data.frame(costCode         = mhDB.d.R.Reg$costCode,
                       month            = mhDB.d.R.Reg$month,
                       cost             = mhDB.d.R.Reg$costNP,
                       stringsAsFactors = FALSE)

  r02.05 <- data.frame(costCode         = mhDB.d.R.OT$costCode,
                       month            = mhDB.d.R.OT$month,
                       cost             = mhDB.d.R.OT$costWage,
                       stringsAsFactors = FALSE)

  r02.06 <- data.frame(costCode         = mhDB.d.R.OT$costCode,
                       month            = mhDB.d.R.OT$month,
                       cost             = mhDB.d.R.OT$costNP,
                       stringsAsFactors = FALSE)

  r02.07 <- data.frame(costCode         = mhDB.m.P.Reg$costCode,
                       month            = mhDB.m.P.Reg$month,
                       cost             = mhDB.m.P.Reg$costNP,
                       stringsAsFactors = FALSE)

  r02.08 <- data.frame(costCode         = mhDB.m.P.OT$costCode,
                       month            = mhDB.m.P.OT$month,
                       cost             = mhDB.m.P.OT$costWage,
                       stringsAsFactors = FALSE)

  r02.09 <- data.frame(costCode         = mhDB.m.P.OT$costCode,
                       month            = mhDB.m.P.OT$month,
                       cost             = mhDB.m.P.OT$costNP,
                       stringsAsFactors = FALSE)

  r02.10 <- data.frame(costCode         = mhDB.d.P.Reg$costCode,
                       month            = mhDB.d.P.Reg$month,
                       cost             = mhDB.d.P.Reg$costNP,
                       stringsAsFactors = FALSE)

  r02.11 <- data.frame(costCode         = mhDB.d.P.OT$costCode,
                       month            = mhDB.d.P.OT$month,
                       cost             = mhDB.d.P.OT$costWage,
                       stringsAsFactors = FALSE)

  r02.12 <- data.frame(costCode         = mhDB.d.P.OT$costCode,
                       month            = mhDB.d.P.OT$month,
                       cost             = mhDB.d.P.OT$costNP,
                       stringsAsFactors = FALSE)

  r02 <- as.data.frame(data.table::rbindlist(l = list(r02.01,
                                                      r02.02,
                                                      r02.03,
                                                      r02.04,
                                                      r02.05,
                                                      r02.06,
                                                      r02.07,
                                                      r02.08,
                                                      r02.09,
                                                      r02.10,
                                                      r02.11,
                                                      r02.12)))

  if (nrow(r02) > 0) {
    r02$row <- "OT Pay - Regular"
  } else {
    r02 <- NULL
  }

  # Salaries-Seasonal
  r03.01 <- data.frame(costCode         = mhDB.m.S.Reg$costCode,
                       month            = mhDB.m.S.Reg$month,
                       cost             = mhDB.m.S.Reg$costWage,
                       stringsAsFactors = FALSE)

  r03.02 <- data.frame(costCode         = mhDB.d.S.Reg$costCode,
                       month            = mhDB.d.S.Reg$month,
                       cost             = mhDB.d.S.Reg$costWage,
                       stringsAsFactors = FALSE)

  r03.03 <- data.frame(costCode         = hol.mhDB.d.S$costCode,
                       month            = hol.mhDB.d.S$month,
                       cost             = hol.mhDB.d.S$XcostWage,
                       stringsAsFactors = FALSE)

  r03 <- as.data.frame(data.table::rbindlist(l = list(r03.01,
                                                      r03.02,
                                                      r03.03)))

  if (nrow(r03) > 0) {
    r03$row <- "Salaries-Seasonal"
  } else {
    r03 <- NULL
  }

  # OT Pay - Seasonal
  r04.01 <- data.frame(costCode         = mhDB.m.S.Reg$costCode,
                       month            = mhDB.m.S.Reg$month,
                       cost             = mhDB.m.S.Reg$costNP,
                       stringsAsFactors = FALSE)

  r04.02 <- data.frame(costCode         = mhDB.m.S.OT$costCode,
                       month            = mhDB.m.S.OT$month,
                       cost             = mhDB.m.S.OT$costWage,
                       stringsAsFactors = FALSE)

  r04.03 <- data.frame(costCode         = mhDB.m.S.OT$costCode,
                       month            = mhDB.m.S.OT$month,
                       cost             = mhDB.m.S.OT$costNP,
                       stringsAsFactors = FALSE)

  r04.04 <- data.frame(costCode         = mhDB.d.S.Reg$costCode,
                       month            = mhDB.d.S.Reg$month,
                       cost             = mhDB.d.S.Reg$costNP,
                       stringsAsFactors = FALSE)

  r04.05 <- data.frame(costCode         = mhDB.d.S.OT$costCode,
                       month            = mhDB.d.S.OT$month,
                       cost             = mhDB.d.S.OT$costWage,
                       stringsAsFactors = FALSE)

  r04.06 <- data.frame(costCode         = mhDB.d.S.OT$costCode,
                       month            = mhDB.d.S.OT$month,
                       cost             = mhDB.d.S.OT$costNP,
                       stringsAsFactors = FALSE)

  r04 <- as.data.frame(data.table::rbindlist(l = list(r04.01,
                                                      r04.02,
                                                      r04.03,
                                                      r04.04,
                                                      r04.05,
                                                      r04.06)))

  if (nrow(r04) > 0) {
    r04$row <- "OT Pay - Seasonal"
  } else {
    r04 <- NULL
  }

  # Employee Allowance
  r05 <- mhDB.allow %>%
    dplyr::group_by(costCode, month) %>%
    dplyr::summarise(cost = sum(cost))

  r05 <- as.data.frame(r05)

  if (nrow(r05) > 0) {
    r05$row <- "Employees Allowance"
  } else {
    r05 <- NULL
  }

  # Employee Benefits
  r06 <- mhDB.SB[, !colnames(mhDB.SB) %in% c("mh", "costCodeNew")]

  if (nrow(r06) > 0) {
    r06$row <- "Employee Benefits"
  } else {
    r06 <- NULL
  }

  # Premium SSS, EC
  r07 <- mhDB.SSS[, c("costCode", "month", "cost")]

  if (nrow(r07) > 0) {
    r07$row <- "Premium SSS, EC"
  } else {
    r07 <- NULL
  }

  # Prem-HDMF (Pag-ibig)
  r08 <- mhDB.PI[, c("costCode", "month", "cost")]

  if (nrow(r08) > 0) {
    r08$row <- "Prem-HDMF (Pag-ibig)"
  } else {
    r08 <- NULL
  }

  # Philhealth
  r09 <- mhDB.PH[, c("costCode", "month", "cost")]

  if (nrow(r09) > 0) {
    r09$row <- "Philhealth"
  } else {
    r09 <- NULL
  }

  # Leave Commutation
  r10 <- mhDB.LC[, c("costCode", "month", "cost")]

  if (nrow(r10) > 0) {
    r10$row <- "Leave Commutation"
  } else {
    r10 <- NULL
  }

  # Hospital and Medical Expenses
  r11 <- mhDB.HM[, c("costCode", "month", "cost")]

  if (nrow(r11) > 0) {
    r11$row <- "Hospital and Medical Expenses"
  } else {
    r11 <- NULL
  }

  # 13th Month Pay
  r12 <- mhDB.13mp[, c("costCode", "month", "cost")]

  ## In 2019 budget, the bonus is part of 13th Month Pay
  if (!forecast) {
    bonus <-mhDB.bonus[, c("costCode", "month", "cost")]
    r12 <- data.table::rbindlist(list(r12, bonus))
  }

  if (nrow(r12) > 0) {
    r12$row <- "13th Month Pay"
  } else {
    r12 <- NULL
  }

  # man hours
  r13 <- as.data.frame(mhDB.mh)

  if (nrow(r13) > 0) {
    r13$row <- "man-hours"
  } else {
    r13 <- NULL
  }

  # CF Manpower Services
  if (forecast){
    manpowerServicesName <- "CF Others"
  } else {
    manpowerServicesName <- "CF Manpower Services"
  }

  r14 <- data.table::rbindlist(l = list(
    data.frame(costCode         = mhDB.d.A$costCode,
               month            = mhDB.d.A$month,
               cost             = mhDB.d.A$cost,
               stringsAsFactors = FALSE),
    data.frame(costCode         = hol.mhDB.d.A$costCode,
               month            = hol.mhDB.d.A$month,
               cost             = hol.mhDB.d.A$XcostWage,
               stringsAsFactors = FALSE),
    data.frame(costCode         = mhDB.SSS.A$costCode,
               month            = mhDB.SSS.A$month,
               cost             = mhDB.SSS.A$cost,
               stringsAsFactors = FALSE),
    data.frame(costCode         = mhDB.PI.A$costCode,
               month            = mhDB.PI.A$month,
               cost             = mhDB.PI.A$cost,
               stringsAsFactors = FALSE),
    data.frame(costCode         = mhDB.PH.A$costCode,
               month            = mhDB.PH.A$month,
               cost             = mhDB.PH.A$cost,
               stringsAsFactors = FALSE),
    data.frame(costCode         = mhDB.LC.A$costCode,
               month            = mhDB.LC.A$month,
               cost             = mhDB.LC.A$cost,
               stringsAsFactors = FALSE),
    data.frame(costCode         = mhDB.HM.A$costCode,
               month            = mhDB.HM.A$month,
               cost             = mhDB.HM.A$cost,
               stringsAsFactors = FALSE),
    data.frame(costCode         = mhDB.13mp.A$costCode,
               month            = mhDB.13mp.A$month,
               cost             = mhDB.13mp.A$cost,
               stringsAsFactors = FALSE),
    data.frame(costCode         = mhDB.riceSub.A$costCode,
               month            = mhDB.riceSub.A$month,
               cost             = mhDB.riceSub.A$cost,
               stringsAsFactors = FALSE)
  ))

  r14      <- as.data.frame(r14)
  r14$cost <- round(r14$cost * 1.15, digits = 2)

  if (nrow(r14) > 0) {
    r14$row  <- manpowerServicesName
  } else {

    r14 <- data.frame(costCode         = unique(mhDB$costCode),
                      month            = 1L,
                      cost             = 0L,
                      row              = manpowerServicesName,
                      stringsAsFactors = FALSE)

  }

  # Safety Gadgets
  r15 <- mhDB.safetyGadgets %>%
    dplyr::group_by(costCode, month) %>%
    dplyr::summarise(cost = sum(cost))
  r15 <- as.data.frame(r15)

  if (nrow(r15) > 0) {
    r15$row <- "Safety Gadgets"
  } else {
    r15 <- NULL
  }

  # Group Life Insurance
  r16 <- mhDB.groupLife %>%
    dplyr::group_by(costCode, month) %>%
    dplyr::summarise(cost = sum(cost))
  r16 <- as.data.frame(r16)

  if (nrow(r16) > 0) {
    r16$row <- "Grouplife"
  } else {
    r16 <- NULL
  }

  # HMO
  r17 <- mhDB.hmo %>%
    dplyr::group_by(costCode, month) %>%
    dplyr::summarise(cost = sum(cost))
  r17 <- as.data.frame(r17)

  if (nrow(r17) > 0) {
    r17$row <- "HMO"
  } else {
    r17 <- NULL
  }

  # Employee Benefits
  r18 <- mhDB.benefits %>%
    dplyr::group_by(costCode, month) %>%
    dplyr::summarise(cost = sum(cost))
  r18 <- as.data.frame(r18)

  if (nrow(r18) > 0) {
    r18$row <- "Employee Benefits"
  } else {
    r18 <- NULL
  }

  # Food Allowance / Rice Subsidy
  r19 <- mhDB.riceSub.I %>%
    dplyr::group_by(costCode, month) %>%
    dplyr::summarise(cost = sum(cost))
  r19 <- as.data.frame(r19)

  if (nrow(r19) > 0) {
    r19$row <- "Food Allowance / Rice Subsidy"
  } else {
    r19 <- NULL
  }

  costDB <- data.table::rbindlist(list(
    r01, r02, r03, r04, r05, r06, r07, r08, r09, r10,
    r11, r12, r13, r14, r15, r16, r17, r18, r19))

  costDB <- costDB %>%
    dplyr::group_by(costCode, row, month) %>%
    dplyr::summarise(cost = sum(cost))

  costDB <- dplyr::left_join(x = costDB, y = ac, by = "row")

  costDB <- costDB %>%
    tidyr::spread(month, cost, fill = 0)

  costCode <- unique(costDB$costCode)

  cat("\nExporting data.\n")

  export <- cbind(paste0(costDB$costCode, costDB$code), as.data.frame(costDB))
  colnames(export)[1] <- "concat"
  export <- export[, c("costCode", "row", "code", "concat", as.character(1:12))]

  export.mh     <- costDB[costDB$code == 999999,
                          !colnames(costDB) %in% c("row", "code")]
  export.mh     <- as.data.frame(export.mh)
  export.mh     <- export.mh[order(export.mh$costCode),]
  export.mh$SUM <- apply(export.mh[, 2:13], MARGIN = 1, FUN = sum)

  return(list(export, export.mh, accr.13mp, mhDB.bonus))
}
