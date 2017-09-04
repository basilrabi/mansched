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
#' @return list
#'
#'   Each element of the list contains a list of 2:
#'   \enumerate{
#'     \item cost code
#'     \item personnel costs table for the whole year
#'   }
#' @importFrom dplyr left_join group_by summarise mutate
#' @importFrom magrittr "%>%"
#' @importFrom data.table rbindlist
#' @importFrom tidyr gather spread
getCost <- function(mhDB, listR, wage) {

  # Fix for "no visible binding for global variable" note in R CMD check
  sal <-
    salM <-
    ID <-
    salH <-
    mh <-
    costCode <-
    cost <-
    XholHours <- NULL

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

  ### Get isOT.R
  mhDB.m.R <- dplyr::left_join(x = mhDB.m.R,
                               y = premium.nonRF[,c("isOT.R",
                                                    "premiumR",
                                                    "npR",
                                                    "mhType")])

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

  ### Get isOT.S
  mhDB.m.S <- dplyr::left_join(x = mhDB.m.S,
                               y = premium.nonRF[, c("isOT.S",
                                                     "premiumS",
                                                     "npS",
                                                     "mhType")])

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
    x = mhDB.m.S.Reg.M,
    y = unique(mhDB[, c("ID", "month", "maxReg")])
  )

  ##### Get absences hours
  mhDB.m.S.Reg.M$abHours <- mhDB.m.S.Reg.M$maxReg - mhDB.m.S.Reg.M$mhTot

  ##### Get salary scheme
  mhDB.m.S.Reg.M <- dplyr::left_join(
    x = mhDB.m.S.Reg.M,
    y = unique(mhDB[, c("ID", "month", "sal")])
  )

  ##### Get monthly and hourly salary
  mhDB.m.S.Reg.M <- dplyr::left_join(
    x = mhDB.m.S.Reg.M,
    y = wageEmp[, !colnames(wageEmp) %in% c("isRF")]
  )

  ##### Get absences cost
  mhDB.m.S.Reg.M$abCost <- round(
    mhDB.m.S.Reg.M$salH * mhDB.m.S.Reg.M$abHours, digits = 2
  )

  ##### monthly wage minus absences = salMB
  mhDB.m.S.Reg.M$salMB <- mhDB.m.S.Reg.M$salM - mhDB.m.S.Reg.M$abCost

  #### Combine salMB
  mhDB.m.S.Reg <- dplyr::left_join(
    x = mhDB.m.S.Reg,
    y = mhDB.m.S.Reg.M[, c("ID",
                           "month",
                           "mhTot",
                           "salH",
                           "salMB")]
  )

  #### Get man hour fraction of each cost code per month
  mhDB.m.S.Reg$Xmh <- mhDB.m.S.Reg$mh / mhDB.m.S.Reg$mhTot

  #### Compute Costs
  #### These costs are purely worked
  mhDB.m.S.Reg$costWage <- round(mhDB.m.S.Reg$Xmh * mhDB.m.S.Reg$salMB,
                                 digits = 2)
  mhDB.m.S.Reg$costNP <- round(
    mhDB.m.S.Reg$salH * mhDB.m.S.Reg$np * mhDB.m.S.Reg$npS, digits = 2)

  ### Separate OT
  mhDB.m.S.OT <- mhDB.m.S[which(mhDB.m.S$isOT.S),
                          !colnames(mhDB.m.S) %in% c("mhType")]

  #### Get salary scheme
  mhDB.m.S.OT <- dplyr::left_join(
    x = mhDB.m.S.OT,
    y = unique(mhDB[, c("ID", "month", "sal")])
  )

  #### Get hourly wage
  mhDB.m.S.OT <- dplyr::left_join(
    x = mhDB.m.S.OT,
    y = wageEmp[, !colnames(wageEmp) %in% c("salM", "isRF")]
  )

  #### Compute costs
  mhDB.m.S.OT$costWage <- round(
    mhDB.m.S.OT$salH * mhDB.m.S.OT$mh * mhDB.m.S.OT$premiumS, digits = 2
  )
  mhDB.m.S.OT$costNP <- round(
    mhDB.m.S.OT$salH * mhDB.m.S.OT$np * mhDB.m.S.OT$npS, digits = 2
  )

  # Compute Salaries for daily wagers or RF

  mhDB.d <- mhDB[which(mhDB$scheme == "d"),
                 !colnames(mhDB) %in% c("scheme")]

  ## Get hourly salary
  mhDB.d <- dplyr::left_join(
    x = mhDB.d,
    y = wageEmp[, !colnames(wageEmp) %in% c("salM", "isRF")]
  )

  ## Separate Regular Employees
  mhDB.d.R <- mhDB.d[which(mhDB.d$isReg),
                     !colnames(mhDB.d) %in% c("isReg", "maxReg")]

  ### Get premium
  mhDB.d.R <- dplyr::left_join(x = mhDB.d.R,
                               y = premium.RF[, c("isOT.R",
                                                  "premiumR",
                                                  "npR",
                                                  "mhType")])

  ### Get cost
  mhDB.d.R$costWage <- round(
    mhDB.d.R$salH * mhDB.d.R$mh * mhDB.d.R$premiumR, digits = 2
  )
  mhDB.d.R$costNP <- round(
    mhDB.d.R$salH * mhDB.d.R$np * mhDB.d.R$npR, digits = 2
  )

  ### Separate non-OT
  mhDB.d.R.Reg <- mhDB.d.R[which(!mhDB.d.R$isOT.R),]

  ### Separate OT
  mhDB.d.R.OT <- mhDB.d.R[which(mhDB.d.R$isOT.R),]

  ## Separate Non-regular Employees
  mhDB.d.S <- mhDB.d[which(!mhDB.d$isReg),
                     !colnames(mhDB.d) %in% c("isReg", "maxReg")]

  ### Get premium
  mhDB.d.S <- dplyr::left_join(x = mhDB.d.S,
                               y = premium.RF[, c("isOT.S",
                                                  "premiumS",
                                                  "npS",
                                                  "mhType")])

  ### Get cost
  mhDB.d.S$costWage <- round(
    mhDB.d.S$salH * mhDB.d.S$mh * mhDB.d.S$premiumS, digits = 2
  )
  mhDB.d.S$costNP <- round(
    mhDB.d.S$salH * mhDB.d.S$np * mhDB.d.S$npS, digits = 2
  )

  ### Separate non-OT
  mhDB.d.S.Reg <- mhDB.d.S[which(!mhDB.d.S$isOT.S),]

  ### Separate OT
  mhDB.d.S.OT <- mhDB.d.S[which(mhDB.d.S$isOT.S),]

  # Distribute holHours

  hol.mhDB <- mhDB[, !colnames(mhDB) %in% c("mhType",
                                            "np",
                                            "scheme",
                                            "maxReg")]

  hol.mhDB <- dplyr::left_join(
    x = hol.mhDB,
    y = unique(wageEmp[, colnames(wageEmp) %in% c("ID", "isRF")])
  )

  hol.mhDB.m <- hol.mhDB[!hol.mhDB$isRF,]
  hol.mhDB.d <- hol.mhDB[hol.mhDB$isRF,]

  ## Create data.frame of holHours
  holHours <- lapply(listR, FUN = function(x) {
    data.frame(ID = x@ID,
               month = 1:12,
               holHours = x@holHours,
               stringsAsFactors = FALSE)
  })

  holHours <- data.table::rbindlist(holHours)

  ## Get sal
  holHours <- dplyr::left_join(
    x = holHours,
    y = unique(mhDB[, c("ID", "month", "sal")])
  )

  ## Get salH
  holHours <- dplyr::left_join(
    x = holHours,
    y = wageEmp[, !colnames(wageEmp) %in% c("salM")]
  )

  holHours.m <- holHours[!holHours$isRF,]
  holHours.d <- holHours[holHours$isRF,]

  holHours.d$costWage <- holHours.d$salH * holHours.d$holHours

  ## Merge costWage and holHours to hol.mhDB.d
  hol.mhDB.d <- dplyr::left_join(
    x = hol.mhDB.d,
    y = unique(holHours.d[, colnames(holHours.d) %in% c("ID",
                                                        "month",
                                                        "holHours",
                                                        "costWage")])
  )
  hol.mhDB.m <- dplyr::left_join(
    x = hol.mhDB.m,
    y = holHours.m[, colnames(holHours.m) %in% c("ID",
                                                 "month",
                                                 "holHours")]
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

  ## Separate regular and non-regular employees
  hol.mhDB.d.R <- hol.mhDB.d[hol.mhDB.d$isReg,]
  hol.mhDB.d.S <- hol.mhDB.d[!hol.mhDB.d$isReg,]

  # Compute for Employee Allowances
  allowance <- data.table::rbindlist(lapply(listR, getAllowance))

  mhDB.allow <- mhDB %>%
    dplyr::group_by(ID, month, costCode) %>%
    dplyr::summarise(mh = sum(mh))

  mhDB.allow <- mhDB.allow %>%
    dplyr::group_by(ID, month) %>%
    dplyr::mutate(totMH = sum(mh))

  mhDB.allow <- dplyr::left_join(mhDB.allow, allowance)

  mhDB.allow$X <- mhDB.allow$mh / mhDB.allow$totMH

  mhDB.allow$cost <- round(mhDB.allow$X * mhDB.allow$allowance, digits = 2)

  # Compute for Safety Bonus
  mhDB.SB <- mhDB %>%
    dplyr::group_by(costCode, month) %>%
    dplyr::summarise(mh = sum(mh))

  mhDB.SB$cost <- round(mhDB.SB$mh * 0.2, digits = 2)
  mhDB.SB <- as.data.frame(mhDB.SB)

  # Compute for SSS contribution of employer
  # FIXME - monthly salaries of all employees are all estimated at the same time

  mhDB.SSS <- mhDB %>%
    dplyr::group_by(ID, month, costCode, sal) %>%
    dplyr::summarise(mh = sum(mh))

  mhDB.SSS <- dplyr::left_join(
    x = mhDB.SSS,
    y = wageEmp[, !colnames(wageEmp) %in% c("salM", "isRF")]
  )

  mhDB.SSS$salM <- mhDB.SSS$salH * 313 * 8 / 12
  mhDB.SSS$SSS <- sapply(mhDB.SSS$salM, FUN = function(x) {
    SSS$c[which(SSS$r1 <= x & SSS$r2 >= x)]
  })

  mhDB.SSS <- mhDB.SSS %>%
    dplyr::group_by(ID, month) %>%
    dplyr::mutate(totMH = sum(mh))

  mhDB.SSS$X <- mhDB.SSS$mh / mhDB.SSS$totMH

  mhDB.SSS$cost <- round(mhDB.SSS$X * mhDB.SSS$SSS, digits = 2)
  mhDB.SSS <- as.data.frame(mhDB.SSS)

  # Compute for Pag-ibig contribution of employer

  mhDB.PI <- mhDB %>%
    dplyr::group_by(ID, month, costCode) %>%
    dplyr::summarise(mh = sum(mh))

  mhDB.PI$PI <- 100

  mhDB.PI <- mhDB.PI %>%
    dplyr::group_by(ID, month) %>%
    dplyr::mutate(totMH = sum(mh))

  mhDB.PI$X <- mhDB.PI$mh / mhDB.PI$totMH

  mhDB.PI$cost <- round(mhDB.PI$X * mhDB.PI$PI, digits = 2)
  mhDB.PI <- as.data.frame(mhDB.PI)

  # Compute for Phil-Health contribution of employer

  mhDB.PH <- mhDB %>%
    dplyr::group_by(ID, month, costCode, sal) %>%
    dplyr::summarise(mh = sum(mh))

  mhDB.PH <- dplyr::left_join(
    x = mhDB.PH,
    y = wageEmp[, !colnames(wageEmp) %in% c("salM", "isRF")]
  )

  mhDB.PH$salM <- mhDB.PH$salH * 313 * 8 / 12
  mhDB.PH$PH <- sapply(mhDB.PH$salM, FUN = function(x) {
    PHIC$c[which(PHIC$r1 <= x & PHIC$r2 >= x)]
  })

  mhDB.PH <- mhDB.PH %>%
    dplyr::group_by(ID, month) %>%
    dplyr::mutate(totMH = sum(mh))

  mhDB.PH$X <- mhDB.PH$mh / mhDB.PH$totMH

  mhDB.PH$cost <- round(mhDB.PH$X * mhDB.PH$PH, digits = 2)
  mhDB.PH <- as.data.frame(mhDB.PH)

  # Compute for Leave Commutation

  ## Get maxReg

  maxRegDB <- lapply(listR, FUN = function(x) {
    data.frame(ID = x@ID,
               month = 1:12,
               maxReg = x@maxReg,
               stringsAsFactors = FALSE)
  })

  ## Get total reg hours attendance (RH)

  ### Separate regular employees

  mhDB.RH.R <- mhDB[mhDB$isReg &
                      mhDB$mhType == "reg",] %>%
    dplyr::group_by(ID, month, sal) %>%
    dplyr::summarise(mh = sum(mh))

  ### Separate non-regular employee

  mhDB.RH.S <- mhDB[!mhDB$isReg &
                      mhDB$mhType %in% c("reg", "nh"),] %>%
    dplyr::group_by(ID, month, sal) %>%
    dplyr::summarise(mh = sum(mh))

  ### Merge regular and non-regular

  mhDB.RH <- data.table::rbindlist(l = list(mhDB.RH.R, mhDB.RH.S))

  ## Combine RH to maxRegDB
  maxRegDB <- lapply(maxRegDB, FUN = function(z) {

    z <- dplyr::left_join(x = z, y = mhDB.RH)
    z[is.na(z)] <- 0L
    z$absence <- z$maxReg - z$mh

    if (any(z$absence) < 0)
      stop("Uh, something's wrong.")

    tempID <- sapply(listR, FUN = function(x) {x@ID})

    tempIndex <- which(tempID == z$ID[1])

    LC <- listR[[tempIndex]]@leaveHours

    z$LH <- 0

    for (i in 1:12) {
      if (LC > 0) {
        minus <- min(z$absence[i], LC)
        z$LH[i] <- minus
        LC <- LC - minus
      } else
        break
    }
    z$LH[4] <- z$LH[4] + LC

    if (sum(z$LH) != listR[[tempIndex]]@leaveHours)
      stop("Uh, something's wrong.")

    z
  })

  maxRegDB <- data.table::rbindlist(maxRegDB)
  maxRegDB <- dplyr::left_join(
    x = maxRegDB,
    y = wageEmp[, !colnames(wageEmp) %in% c("salM", "isRF")]
  )
  maxRegDB$LC <- maxRegDB$salH * maxRegDB$LH

  ## Distribute leave commutation cost
  mhDB.LC <-mhDB %>%
    dplyr::group_by(ID, month, costCode) %>%
    dplyr::summarise(mh = sum(mh))

  mhDB.LC <- mhDB.LC %>%
    dplyr::group_by(ID, month) %>%
    dplyr::mutate(totMH = sum(mh))

  mhDB.LC$X <- mhDB.LC$mh / mhDB.LC$totMH

  if (any(mhDB.LC$X > 1))
    stop("Uh, something's wrong.")

  mhDB.LC <- dplyr::left_join(
    x = mhDB.LC,
    y = maxRegDB[, colnames(maxRegDB) %in% c("ID",
                                             "month",
                                             "LC")]
  )

  ## Get cost
  mhDB.LC$cost <- round(mhDB.LC$X * mhDB.LC$LC, digits = 2)
  mhDB.LC <- as.data.frame(mhDB.LC)

  # Compute for Hospital and Medical Expenses
  hm <- data.table::rbindlist(lapply(listR, getHM))

  mhDB.HM <- mhDB %>%
    dplyr::group_by(ID, month, costCode) %>%
    dplyr::summarise(mh = sum(mh))

  mhDB.HM <- mhDB.HM %>%
    dplyr::group_by(ID, month) %>%
    dplyr::mutate(totMH = sum(mh))

  mhDB.HM <- dplyr::left_join(mhDB.HM, hm)

  mhDB.HM$X <- mhDB.HM$mh / mhDB.HM$totMH

  mhDB.HM$cost <- round(mhDB.HM$X * mhDB.HM$HM, digits = 2)

  # Compute for 13th month pay

  mp13 <- data.table::rbindlist(lapply(listR, FUN = function(x) {
    tempIndex <- which(wageEmp$ID == x@ID)
    tempSal <- wageEmp$salH[tempIndex]
    get13mp(theObject = x, sal = tempSal)
  }))

  mhDB.13mp <- mhDB %>%
    dplyr::group_by(ID, month, costCode) %>%
    dplyr::summarise(mh = sum(mh))

  mhDB.13mp <- mhDB.13mp %>%
    dplyr::group_by(ID, month) %>%
    dplyr::mutate(totMH = sum(mh))

  mhDB.13mp$X <- mhDB.13mp$mh / mhDB.13mp$totMH

  mhDB.13mp <- dplyr::left_join(mhDB.13mp, mp13)

  mhDB.13mp$cost <- round(mhDB.13mp$X * mhDB.13mp$mp, digits = 2)

  # Sum all manhours
  mhDB.mh1 <- mhDB %>%
    dplyr::group_by(costCode, month) %>%
    dplyr::summarise(cost = sum(mh))

  mhDB.mh2 <- hol.mhDB.m %>%
    dplyr::group_by(costCode, month) %>%
    dplyr::summarise(cost = sum(XholHours))

  mhDB.mh3 <- hol.mhDB.d %>%
    dplyr::group_by(costCode, month) %>%
    dplyr::summarise(cost = sum(XholHours))

  mhDB.mh <- data.table::rbindlist(list(mhDB.mh1, mhDB.mh2, mhDB.mh3))

  # Salaries-Regular
  r01.01 <- data.frame(costCode = mhDB.m.R.Reg$costCode,
                       month = mhDB.m.R.Reg$month,
                       cost = mhDB.m.R.Reg$costWage,
                       stringsAsFactors = FALSE)

  r01.02 <- data.frame(costCode = mhDB.d.R.Reg$costCode,
                       month = mhDB.d.R.Reg$month,
                       cost = mhDB.d.R.Reg$costWage,
                       stringsAsFactors = FALSE)

  r01.03 <- data.frame(costCode = hol.mhDB.d.R$costCode,
                       month = hol.mhDB.d.R$month,
                       cost = hol.mhDB.d.R$XcostWage,
                       stringsAsFactors = FALSE)

  r01 <- as.data.frame(data.table::rbindlist(l = list(r01.01,
                                                      r01.02,
                                                      r01.03)))
  r01$row <- "Salaries-Regular"

  # OT Pay - Regular
  r02.01 <- data.frame(costCode = mhDB.m.R.Reg$costCode,
                       month = mhDB.m.R.Reg$month,
                       cost = mhDB.m.R.Reg$costNP,
                       stringsAsFactors = FALSE)

  r02.02 <- data.frame(costCode = mhDB.m.R.OT$costCode,
                       month = mhDB.m.R.OT$month,
                       cost = mhDB.m.R.OT$costWage,
                       stringsAsFactors = FALSE)

  r02.03 <- data.frame(costCode = mhDB.m.R.OT$costCode,
                       month = mhDB.m.R.OT$month,
                       cost = mhDB.m.R.OT$costNP,
                       stringsAsFactors = FALSE)

  r02.04 <- data.frame(costCode = mhDB.d.R.Reg$costCode,
                       month = mhDB.d.R.Reg$month,
                       cost = mhDB.d.R.Reg$costNP,
                       stringsAsFactors = FALSE)

  r02.05 <- data.frame(costCode = mhDB.d.R.OT$costCode,
                       month = mhDB.d.R.OT$month,
                       cost = mhDB.d.R.OT$costWage,
                       stringsAsFactors = FALSE)

  r02.06 <- data.frame(costCode = mhDB.d.R.OT$costCode,
                       month = mhDB.d.R.OT$month,
                       cost = mhDB.d.R.OT$costNP,
                       stringsAsFactors = FALSE)

  r02 <- as.data.frame(data.table::rbindlist(l = list(r02.01,
                                                      r02.02,
                                                      r02.03,
                                                      r02.04,
                                                      r02.05,
                                                      r02.06)))
  r02$row <- "OT Pay - Regular"

  # Salaries-Seasonal
  r03.01 <- data.frame(costCode = mhDB.m.S.Reg$costCode,
                       month = mhDB.m.S.Reg$month,
                       cost = mhDB.m.S.Reg$costWage,
                       stringsAsFactors = FALSE)

  r03.02 <- data.frame(costCode = mhDB.d.S.Reg$costCode,
                       month = mhDB.d.S.Reg$month,
                       cost = mhDB.d.S.Reg$costWage,
                       stringsAsFactors = FALSE)

  r03.03 <- data.frame(costCode = hol.mhDB.d.S$costCode,
                       month = hol.mhDB.d.S$month,
                       cost = hol.mhDB.d.S$XcostWage,
                       stringsAsFactors = FALSE)

  r03 <- as.data.frame(data.table::rbindlist(l = list(r03.01,
                                                      r03.02,
                                                      r03.03)))
  r03$row <- "Salaries-Seasonal"

  # OT Pay - Seasonal
  r04.01 <- data.frame(costCode = mhDB.m.S.Reg$costCode,
                       month = mhDB.m.S.Reg$month,
                       cost = mhDB.m.S.Reg$costNP,
                       stringsAsFactors = FALSE)

  r04.02 <- data.frame(costCode = mhDB.m.S.OT$costCode,
                       month = mhDB.m.S.OT$month,
                       cost = mhDB.m.S.OT$costWage,
                       stringsAsFactors = FALSE)

  r04.03 <- data.frame(costCode = mhDB.m.S.OT$costCode,
                       month = mhDB.m.S.OT$month,
                       cost = mhDB.m.S.OT$costNP,
                       stringsAsFactors = FALSE)

  r04.04 <- data.frame(costCode = mhDB.d.S.Reg$costCode,
                       month = mhDB.d.S.Reg$month,
                       cost = mhDB.d.S.Reg$costNP,
                       stringsAsFactors = FALSE)

  r04.05 <- data.frame(costCode = mhDB.d.S.OT$costCode,
                       month = mhDB.d.S.OT$month,
                       cost = mhDB.d.S.OT$costWage,
                       stringsAsFactors = FALSE)

  r04.06 <- data.frame(costCode = mhDB.d.S.OT$costCode,
                       month = mhDB.d.S.OT$month,
                       cost = mhDB.d.S.OT$costNP,
                       stringsAsFactors = FALSE)

  r04 <- as.data.frame(data.table::rbindlist(l = list(r04.01,
                                                      r04.02,
                                                      r04.03,
                                                      r04.04,
                                                      r04.05,
                                                      r04.06)))
  r04$row <- "OT Pay - Seasonal"

  # Employee Allowance
  r05 <- mhDB.allow %>%
    dplyr::group_by(costCode, month) %>%
    dplyr::summarise(cost = sum(cost))
  r05 <- as.data.frame(r05)
  r05$row <- "Employees Allowance"

  # Employee Benefits
  r06 <- mhDB.SB[, !colnames(mhDB.SB) %in% c("mh")]
  r06$row <- "Employee Benefits"

  # Premium SSS, EC
  r07 <- mhDB.SSS[, c("costCode", "month", "cost")]
  r07$row <- "Premium SSS, EC"

  # Prem-HDMF (Pag-ibig)
  r08 <- mhDB.PI[, c("costCode", "month", "cost")]
  r08$row <- "Prem-HDMF (Pag-ibig)"

  # Philhealth
  r09 <- mhDB.PH[, c("costCode", "month", "cost")]
  r09$row <- "Philhealth"

  # Leave Commutation
  r10 <- mhDB.LC[, c("costCode", "month", "cost")]
  r10$row <- "Leave Commutation"

  # Hospital and Medical Expenses
  r11 <- mhDB.HM[, c("costCode", "month", "cost")]
  r11$row <- "Hospital and Medical Expenses"

  # 13th Month Pay
  r12 <- mhDB.13mp[, c("costCode", "month", "cost")]
  r12$row <- "13th Month Pay"

  # man hours
  r13 <- as.data.frame(mhDB.mh)
  r13$row <- "man-hours"

  costDB <- data.table::rbindlist(list(r01,
                                       r02,
                                       r03,
                                       r04,
                                       r05,
                                       r06,
                                       r07,
                                       r08,
                                       r09,
                                       r10,
                                       r11,
                                       r12,
                                       r13))

  costDB <- costDB %>%
    dplyr::group_by(costCode, row, month) %>%
    dplyr::summarise(cost = sum(cost))

  costDB <- dplyr::left_join(costDB, ac)

  costDB <- costDB %>%
    tidyr::spread(month, cost, fill = 0)

  costCode <- unique(costDB$costCode)

  export <- lapply(costCode, FUN = function(x) {
    tempData <- costDB[costDB$costCode == x,
                       !colnames(costDB) %in% c("costCode")]
    tempData <- tempData[, c(2, 1, 3:14)]
    return(list(x, tempData))
  })

  return(export)
}
