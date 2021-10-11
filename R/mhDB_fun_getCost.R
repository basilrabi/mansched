#' @import methods
NULL

#' Compute Personnel Cost
#'
#' Calculates monthly personnel cost for each cost center Personnel costs are
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
#'     \item{521009}{Bonus}
#'     \item{521018}{HMO}
#'     \item{524037}{Grouplife}
#'     \item{523011}{Safety Gadgets}
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
#' @param bonusFactorYearEnd a numeric value for the salary multiplier for the
#'   year-end bonus
#' @param absentee a \code{\link{data.frame}} composed of at least two columns:
#'   \describe{
#'     \item{ID}{character string representing a unique identifier of an
#'       employee}
#'     \item{costCenter}{Cost centers wherein the leave commutation or the
#'       holiday pays are distributed whenever the employee does not report for
#'       work for a month.
#'     }
#'   }
#' @param monthStart an integer representing the month of start of forecasting
#' @param leaveConversionFactor numeric value for the monthly salary fraction
#'   assumed as leave conversion
#' @return a list containing the following:
#'
#'   \enumerate{
#'     \item a data.frame with 16 columns
#'
#'       The last 12 columns represents the cost per month.
#'       The first column is the activity cost center.
#'       The 2nd column is the description of the general ledger code.
#'       The 3rd column is the equipment type used.
#'       The 4rth column is the general ledger code.
#'
#'     \item tabulated total man hours per month per cost center
#'     \item tabulated 13th month pay per month per cost center
#'     \item tabulated bonus cost per month per cost center
#'   }
#' @export getCost
#' @importFrom dplyr "%>%" bind_rows case_when filter group_by left_join mutate select summarise ungroup
#' @importFrom data.table data.table rbindlist
#' @importFrom tidyr gather pivot_longer pivot_wider spread
getCost <- function(mhDB,
                    listR,
                    wage,
                    forecast = FALSE,
                    bonusFactorYearEnd = 1.5,
                    absentee = NA,
                    monthStart = 1L,
                    leaveConversionFactor = 0.5) {

  HM <-
    ID <-
    LC <-
    PHIC <-
    PI <-
    X <-
    XholHours <-
    Xmh <-
    abCost <-
    abHours <-
    absenteeList <-
    allow <-
    code <-
    conversion <-
    cost <-
    costCenters <-
    costCenter <-
    costCenterNew <-
    costNP <-
    costWage <-
    description <-
    equipment <-
    hRateA <-
    hRateB <-
    i <-
    isOT.R <-
    isOT.S <-
    isStaff <-
    maxReg <-
    mh <-
    mhDB.m.R.Reg.M <-
    mhTot <-
    mhType <-
    mp <-
    np <-
    npR <-
    npS <-
    premiumR <-
    premiumS <-
    retentionBonus <-
    riceSub <-
    s <-
    sB <-
    sal <-
    salH <-
    salM <-
    salMB <-
    scheme <-
    sg <-
    status <-
    totalCost <-
    totMH <-
    totMHCostCenter <- NULL
  distType  <- c("reg", "rd", "sh", "lh", "nh", "rs", "rl", "rn")

  # Arrange absentee to a data.frame
  if (!any(is.na(absentee))) {
    absenteeList <- lapply(1:nrow(absentee), function(x) {
      costCenters <- rmWS(absentee$costCenter[x])
      costCenters <- unique(strsplit(x = costCenters,
                                     split = " ",
                                     fixed = TRUE)[[1]])
      dfOut <- data.frame(ID = absentee$ID[x], costCenters = costCenters)
    }) %>%
      data.table::rbindlist()
  }

  #### Sanity Check ####

  # Error if any ID in wage is duplicated
  if (anyDuplicated(wage$ID) > 0) {
    tempData <- wage$ID[which(duplicated(wage$ID))]
    cat(paste("Duplicated :", tempData, "\n", sep = ""))
    stop("There must be no duplicated ID's in wage data!")
  }

  # Employee salary link and maximum regular hours per month
  empSM <- lapply(listR, function(x) {
    if (forecast) {
      sm <- data.frame(month = 1:12, sal = "a")
    } else if (isRF(x)) {
      sm <- payB
    } else {
      sm <- payA
    }
    sm$maxReg <- x@maxReg
    sm$ID <- x@ID
    return(sm)
  }) %>% data.table::rbindlist()

  # Employee payment scheme and status
  empSS <- lapply(listR, function(x) {
    data.frame(ID = x@ID,
               scheme = ifelse(isRF(x), "d", "m"),
               status = x@status,
               isRF = isRF(x),
               isStaff = is(x, "Staff"))
  }) %>% data.table::rbindlist()
  duplicatedID <- empSS$ID[which(duplicated(empSS$ID))]
  if (length(duplicatedID) > 0)
    stop(paste0("ID ", duplicatedID, " duplicated"))

  # Error if any ID in listR is not in wage$ID
  if (any(!empSS$ID %in% wage$ID)) {
    empID <- empSS$ID[which(!empSS$ID %in% wage$ID)]
    cat(paste("No wage data for :", empID, "\n", sep = ""))
    stop("All ID's must have wage data")
  }

  # Assign if employee
  # > isRF
  # > isStaff
  # > assign salary increase
  wage <- dplyr::left_join(wage,
                           dplyr::select(empSS, ID, isRF, isStaff),
                           by = "ID") %>%
    dplyr::mutate(sB = dplyr::case_when(forecast ~ s, TRUE ~ i)) %>%
    dplyr::select(-i)

  #### Assign totHours ####
  wage$totHours <- sapply(wage$ID, FUN = function(x) {
    # index <- which(empID == x)
    # sum(listR[[index]]@totHours)

    # total days per year is 313 as advised by Accounting
    return(313 * 8)
  })

  #### Compute hourly rates ####
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

  wageM <- dplyr::select(wage, ID, s, sB)
  wageH <- dplyr::select(wage, ID, hRateA, hRateB)

  colnames(wageM)[c(2,3)] <- c("a", "b")
  colnames(wageH)[c(2,3)] <- c("a", "b")

  wageM <- wageM %>%
    tidyr::pivot_longer(-ID, names_to = "sal", values_to = "salM")
  wageH <- wageH %>%
    tidyr::pivot_longer(-ID, names_to = "sal", values_to = "salH")

  wageEmp <- dplyr::left_join(x  = wageM, y  = wageH, by = c("ID", "sal")) %>%
    dplyr::left_join(dplyr::select(wage, ID, isRF), by = "ID")

  # Join sal, scheme, status, and maxReg
  mhDB <- dplyr::left_join(mhDB,
                           dplyr::select(empSS,  ID, scheme, status),
                           by = "ID") %>%
    dplyr::left_join(empSM, by = c("ID", "month")) %>%
    dplyr::mutate(costCenter = cleanCC(costCenter))

  #### Compute Salaries for monthly wagers ####

  cat("\nComputing salaries for non-RF.\n")

  mhDB.m <- dplyr::filter(mhDB, scheme == "m") %>% dplyr::select(-scheme)

  ## Separate Regular Employees
  mhDB.m.R <- dplyr::filter(mhDB.m, status == "reg") %>%
    dplyr::select(-c(sal, status, maxReg)) %>%
    dplyr::left_join(
      dplyr::select(premium.nonRF, isOT.R, premiumR, npR, mhType),
      by = c("mhType")
    )

  ### Separate non-OT
  mhDB.m.R.Reg <- dplyr::filter(mhDB.m.R, !isOT.R) %>%
    dplyr::select(-c(mhType, isOT.R, premiumR))

  #### Get monthly wage minus absences
  mhDB.m.R.Reg.M <- mhDB.m.R.Reg %>%
    dplyr::group_by(ID, month) %>%
    dplyr::summarise(mhTot = sum(mh)) %>%
    dplyr::left_join(
      unique(dplyr::select(mhDB, ID, month, maxReg)),
      by = c("ID", "month")
    ) %>%
    dplyr::mutate(abHours = maxReg - mhTot) %>%
    dplyr::left_join(unique(dplyr::select(mhDB, ID, month, sal)),
                     by = c("ID", "month")) %>%
    dplyr::left_join(dplyr::select(wageEmp, -isRF),
                     by = c("ID", "sal")) %>%
    dplyr::mutate(abCost = round(salH * abHours, digits = 2)) %>%
    dplyr::mutate(salMB = salM - abCost)

  #### Combine salMB
  mhDB.m.R.Reg <- dplyr::left_join(
    x  = mhDB.m.R.Reg,
    y  = dplyr::select(mhDB.m.R.Reg.M, ID, month, mhTot, salH, salMB),
    by = c("ID", "month")
  ) %>%
    dplyr::mutate(Xmh = mh / mhTot) %>%
    dplyr::mutate(costWage = round(Xmh * salMB, digits = 2),
                  costNP = round(salH * np * npR, digits = 2))

  ### Separate OT
  mhDB.m.R.OT <- dplyr::filter(mhDB.m.R, isOT.R) %>%
    dplyr::select(-mhType) %>%
    dplyr::left_join(unique(dplyr::select(mhDB, ID, month, sal)),
                     by = c("ID", "month")) %>%
    dplyr::left_join(dplyr::select(wageEmp, -c(salM, isRF)),
                     by = c("ID", "sal")) %>%
    dplyr::mutate(costWage = round(salH * mh * premiumR, digits = 2),
                  costNP = round(salH * np * npR, digits = 2))

  ## Separate probationary employees
  mhDB.m.P <- dplyr::filter(mhDB.m, status == "pro") %>%
    dplyr::select(-c(sal, status, maxReg)) %>%
    dplyr::left_join(
      dplyr::select(premium.nonRF, isOT.S, premiumS, npS, mhType),
      by = "mhType"
    )

  ### Separate non-OT
  mhDB.m.P.Reg <- dplyr::filter(mhDB.m.P, !isOT.S) %>%
    dplyr::select(-c(mhType, isOT.S, premiumS))

  #### Get monthly wage minus absences
  mhDB.m.P.Reg.M <- mhDB.m.P.Reg %>%
    dplyr::group_by(ID, month) %>%
    dplyr::summarise(mhTot = sum(mh)) %>%
    dplyr::left_join(unique(dplyr::select(mhDB, ID, month, maxReg)),
                     by = c("ID", "month")) %>%
    dplyr::mutate(abHours = maxReg - mhTot) %>%
    dplyr::left_join(unique(dplyr::select(mhDB, ID, month, sal)),
                     by = c("ID", "month")) %>%
    dplyr::left_join(dplyr::select(wageEmp, -isRF),
                     by = c("ID", "sal")) %>%
    dplyr::mutate(abCost = round(salH * abHours, digits = 2)) %>%
    dplyr::mutate(salMB = salM - abCost)

  #### Combine salMB
  mhDB.m.P.Reg <- dplyr::left_join(
    x  = mhDB.m.P.Reg,
    y  = dplyr::select(mhDB.m.P.Reg.M, ID, month, mhTot, salH, salMB),
    by = c("ID", "month")
  ) %>%
    dplyr::mutate(Xmh = mh / mhTot) %>%
    dplyr::mutate(costWage = round(Xmh * salMB, digits = 2),
                  costNP = round(salH * np * npS, digits = 2))

  ### Separate OT
  mhDB.m.P.OT <- dplyr::filter(mhDB.m.P, isOT.S) %>%
    dplyr::select(-mhType) %>%
    dplyr::left_join(unique(dplyr::select(mhDB, ID, month, sal)),
                     by = c("ID", "month")) %>%
    dplyr::left_join(dplyr::select(wageEmp, -c(salM, isRF)),
                     by = c("ID", "sal")) %>%
    dplyr::mutate(costWage = round(salH * mh * premiumS, digits = 2),
                  costNP = round(salH * np * npS, digits = 2))

  ## Separate seasonal employees
  mhDB.m.S <- dplyr::filter(mhDB.m, status == "sea") %>%
    dplyr::select(-c(sal, status, maxReg)) %>%
    dplyr::left_join(
      dplyr::select(premium.nonRF, isOT.S, premiumS, npS, mhType),
      by = "mhType"
    )

  ### Separate non-OT
  mhDB.m.S.Reg <- dplyr::filter(mhDB.m.S, !isOT.S) %>%
    dplyr::select(-c(mhType, isOT.S, premiumS))

  #### Get monthly wage minus absences
  mhDB.m.S.Reg.M <- mhDB.m.S.Reg %>%
    dplyr::group_by(ID, month) %>%
    dplyr::summarise(mhTot = sum(mh)) %>%
    dplyr::left_join(
      unique(dplyr::select(mhDB, ID, month, maxReg)),
      by = c("ID", "month")
    ) %>%
    dplyr::mutate(abHours = maxReg - mhTot) %>%
    dplyr::left_join(unique(dplyr::select(mhDB, ID, month, sal)),
                     by = c("ID", "month")) %>%
    dplyr::left_join(dplyr::select(wageEmp, -isRF),
                     by = c("ID", "sal")) %>%
    dplyr::mutate(abCost = round(salH * abHours, digits = 2)) %>%
    dplyr::mutate(salMB = salM - abCost)

  #### Combine salMB
  mhDB.m.S.Reg <- dplyr::left_join(
    x = mhDB.m.S.Reg,
    y = dplyr::select(mhDB.m.S.Reg.M, ID, month, mhTot, salH, salMB),
    by = c("ID", "month")
  ) %>%
    dplyr::mutate(Xmh = mh / mhTot) %>%
    dplyr::mutate(costWage = round(Xmh * salMB, digits = 2),
                  costNP = round(salH * np * npS, digits = 2))

  ### Separate OT
  mhDB.m.S.OT <- dplyr::filter(mhDB.m.S, isOT.S) %>%
    dplyr::select(-mhType) %>%
    dplyr::left_join(unique(dplyr::select(mhDB, ID, month, sal)),
                     by = c("ID", "month")) %>%
    dplyr::left_join(dplyr::select(wageEmp, -c(salM, isRF)),
                     by = c("ID", "sal")) %>%
    dplyr::mutate(costWage = round(salH * mh * premiumS, digits = 2),
                  costNP = round(salH * np * npS, digits = 2))

  #### Compute Salaries for daily wagers ####
  cat("\nComputing salaries for RF.\n")

  mhDB.d <- dplyr::filter(mhDB, scheme == "d") %>%
    dplyr::select(-scheme) %>%
    dplyr::left_join(dplyr::select(wageEmp, -c(salM, isRF)),
                     by = c("ID", "sal"))

  ## Separate Regular Employees
  mhDB.d.R <- dplyr::filter(mhDB.d, status == "reg") %>%
    dplyr::select(-c(status, maxReg)) %>%
    dplyr::left_join(
      y = dplyr::select(premium.RF, isOT.R, premiumR, npR, mhType),
      by = "mhType"
    ) %>%
    dplyr::mutate(costWage = round(salH * mh * premiumR, digits = 2),
                  costNP = round(salH * np * npR, digits = 2))

  ### Separate non-OT and OT
  mhDB.d.R.Reg <- dplyr::filter(mhDB.d.R, !isOT.R)
  mhDB.d.R.OT <- dplyr::filter(mhDB.d.R, isOT.R)

  ## Separate probationary employees
  mhDB.d.P <- dplyr::filter(mhDB.d, status == "pro") %>%
    dplyr::select(-c(status, maxReg)) %>%
    dplyr::left_join(dplyr::select(premium.RF, isOT.S, premiumS, npS, mhType),
                     by = "mhType") %>%
    dplyr::mutate(costWage = round(salH * mh * premiumS, digits = 2),
                  costNP = round(salH * np * npS, digits = 2))

  ### Separate non-OT and OT
  mhDB.d.P.Reg <- dplyr::filter(mhDB.d.P, !isOT.S)
  mhDB.d.P.OT <- dplyr::filter(mhDB.d.P, isOT.S)

  ## Separate seasonal employees
  mhDB.d.S <- dplyr::filter(mhDB.d, status == "sea") %>%
    dplyr::select(-c(status, maxReg)) %>%
    dplyr::left_join(dplyr::select(premium.RF, isOT.S, premiumS, npS, mhType),
                     by = "mhType") %>%
    dplyr::mutate(costWage = round(salH * mh * premiumS, digits = 2),
                  costNP = round(salH * np * npS, digits = 2))

  ### Separate non-OT and OT
  mhDB.d.S.Reg <- dplyr::filter(mhDB.d.S, !isOT.S)
  mhDB.d.S.OT <- dplyr::filter(mhDB.d.S, isOT.S)

  ## Separate agency employees
  mhDB.d.A <- dplyr::filter(mhDB.d, status == "age") %>%
    dplyr::select(-c(status, maxReg)) %>%
    dplyr::left_join(dplyr::select(premium.RF, isOT.S, premiumS, npS, mhType),
                     by = "mhType") %>%
    dplyr::mutate(costWage = round(salH * mh * premiumS, digits = 2),
                  costNP = round(salH * np * npS, digits = 2)) %>%
    dplyr::mutate(cost = costWage + costNP)

  #### Distribute holHours ####
  hol.mhDB <- dplyr::filter(mhDB, mhType == "reg") %>%
    dplyr::select(-c(mhType, np, scheme, maxReg)) %>%
    dplyr::left_join(unique(dplyr::select(wageEmp, ID, isRF)),
                     by = "ID")
  hol.mhDB.m <- dplyr::filter(hol.mhDB, !isRF)
  hol.mhDB.d <- dplyr::filter(hol.mhDB, isRF)

  ## Create data.frame of holHours
  holHours <- lapply(listR, FUN = function(x) {
    data.frame(ID = x@ID, month = 1:12, holHours = x@holHours)
  }) %>%
    data.table::rbindlist(use.names = TRUE) %>%
    dplyr::left_join(unique(dplyr::select(mhDB, ID, month, sal)),
                     by = c("ID", "month")) %>%
    dplyr::left_join(dplyr::select(wageEmp, -salM),
                     by = c("ID", "sal"))

  holHours.m <- dplyr::filter(holHours, !isRF)
  holHours.d <- dplyr::filter(holHours, isRF) %>%
    dplyr::mutate(costWage = salH * holHours)

  ## Merge costWage and holHours to hol.mhDB.d
  hol.mhDB.d <- dplyr::left_join(
    x = hol.mhDB.d,
    y = unique(dplyr::select(holHours.d, ID, month, holHours, costWage)),
    by = c("ID", "month")
  )

  hol.mhDB.m <- dplyr::left_join(
    x = hol.mhDB.m,
    y = dplyr::select(holHours.m, ID, month, holHours),
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
  hol.mhDB.d.R <- dplyr::filter(hol.mhDB.d, status %in% c("reg", "pro"))
  hol.mhDB.d.S <- dplyr::filter(hol.mhDB.d, status == "sea")
  hol.mhDB.d.A <- dplyr::filter(hol.mhDB.d, status == "age")

  #### Compute for Employee Allowances ####
  cat("\nComputing employee allowances.\n")

  allowance  <- data.table::rbindlist(lapply(listR, getAllowance),
                                      use.names = TRUE)
  mhDB.allow <- dplyr::filter(mhDB, mhType %in% distType) %>%
    dplyr::group_by(ID, month, costCenter, status, equipment) %>%
    dplyr::summarise(mh = sum(mh)) %>%
    dplyr::group_by(ID, month, status) %>%
    dplyr::mutate(totMH = sum(mh)) %>%
    dplyr::left_join(allowance, by = c("ID", "month")) %>%
    dplyr::mutate(X = mh / totMH) %>%
    dplyr::mutate(cost = round(X * allowance, digits = 2))

  #### Compute for Safety Gadgets ####
  cat("\nComputing safety gadgets.\n")

  safetyGadgets <- data.table::rbindlist(lapply(listR, getSafetyGadgets),
                                         use.names = TRUE)
  mhDB.safetyGadgets <- dplyr::filter(mhDB, mhType %in% distType) %>%
    dplyr::group_by(ID, month, costCenter, status, equipment) %>%
    dplyr::summarise(mh = sum(mh)) %>%
    dplyr::group_by(ID, month, status) %>%
    dplyr::mutate(totMH = sum(mh)) %>%
    dplyr::left_join(safetyGadgets, by = c("ID", "month")) %>%
    dplyr::mutate(X = mh / totMH) %>%
    dplyr::mutate(cost = round(X * sg, digits = 2))

  #### Compute for Group Life Insurance ####
  cat("\nComputing for group life insurance.\n")

  groupLife <- data.table::rbindlist(lapply(listR, getGroupLife),
                                     use.names = TRUE)
  mhDB.groupLife <- dplyr::filter(mhDB, mhType %in% distType) %>%
    dplyr::group_by(ID, month, costCenter, equipment) %>%
    dplyr::summarise(mh = sum(mh)) %>%
    dplyr::group_by(ID, month) %>%
    dplyr::mutate(totMH = sum(mh)) %>%
    dplyr::left_join(groupLife, by = c("ID", "month")) %>%
    dplyr::mutate(X = mh / totMH) %>%
    dplyr::mutate(cost = round(X * gl, digits = 2))

  ####  Compute for Signing Bonus ####
  cat("\nComputing for Signing Bonus.\n")

  signingBonus <- data.table::rbindlist(lapply(listR, getSigningBonus),
                                        use.names = TRUE)
  mhDB.signingBonus <- dplyr::filter(mhDB,
                                     mhType %in% distType,
                                     month < 6) %>%
    dplyr::group_by(ID, costCenter, equipment) %>%
    dplyr::summarise(mh = sum(mh)) %>%
    dplyr::group_by(ID) %>%
    dplyr::mutate(totMH = sum(mh), month = 5L) %>%
    dplyr::mutate(X = mh / totMH) %>%
    dplyr::left_join(signingBonus, by = c("ID", "month")) %>%
    dplyr::mutate(cost = round(X * signingBonus, digits = 2)) %>%
    dplyr::filter(!is.na(cost)) %>%
    dplyr::group_by(costCenter, month, equipment) %>%
    dplyr::summarise(cost = sum(cost))

  #### Compute for HMO ####
  cat("\nComputing for HMO.\n")

  hmo <- data.table::rbindlist(lapply(listR, getHMO), use.names = TRUE)
  mhDB.hmo <- dplyr::filter(mhDB, mhType %in% distType) %>%
    dplyr::group_by(ID, month, costCenter, equipment) %>%
    dplyr::summarise(mh = sum(mh)) %>%
    dplyr::group_by(ID, month) %>%
    dplyr::mutate(totMH = sum(mh)) %>%
    dplyr::left_join(hmo, by = c("ID", "month")) %>%
    dplyr::mutate(X = mh / totMH) %>%
    dplyr::mutate(cost = round(X * hmo, digits = 2))

  #### Employee benefits ####
  # cat("\nComputing for employee benefits.\n")

  benefits <- data.table::rbindlist(list(
    data.table::rbindlist(lapply(listR, getCBA), use.names = TRUE),
    data.table::rbindlist(lapply(listR, getLongShirt), use.names = TRUE),
    data.table::rbindlist(lapply(listR, getLaborDayShirt), use.names = TRUE),
    data.table::rbindlist(lapply(listR, FUN = function(x) {
      if (isReg(x)) {
        return(getGC(x))
      } else {
        return(NULL)
      }
    }), use.names = TRUE)
  ), use.names = TRUE) %>%
    dplyr::group_by(month, ID) %>%
    dplyr::summarise(benefits = sum(benefits))

  mhDB.benefits <- dplyr::filter(mhDB, mhType %in% distType) %>%
    dplyr::group_by(ID, month, costCenter, status, equipment) %>%
    dplyr::summarise(mh = sum(mh)) %>%
    dplyr::group_by(ID, month) %>%
    dplyr::mutate(totMH = sum(mh)) %>%
    dplyr::left_join(benefits, by = c("ID", "month")) %>%
    dplyr::mutate(X = mh / totMH) %>%
    dplyr::mutate(cost = round(X * benefits, digits = 2))

  mhDB.benefits.sea <- dplyr::filter(mhDB.benefits, status == "sea") %>%
    dplyr::select(-status)

  gcSea <- data.table::rbindlist(lapply(listR, FUN = function(x) {
    if (x@status == "sea") {
      return(getGC(x))
    } else {
      return(NULL)
    }
  }) , use.names = TRUE)

  if (ncol(gcSea) > 0 & nrow(gcSea) > 0) {
    mhDB.GC.sea <- dplyr::filter(mhDB,
                                 mhType %in% distType,
                                 status == "sea",
                                 month %in% 4:10) %>%
      dplyr::group_by(ID, costCenter, equipment) %>%
      dplyr::summarise(mh = sum(mh)) %>%
      dplyr::group_by(ID) %>%
      dplyr::mutate(totMH = sum(mh)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(month = 11L, X = mh / totMH) %>%
      dplyr::left_join(gcSea, by = c("ID", "month")) %>%
      dplyr::mutate(cost = round(X * benefits, digits = 2))
    mhDB.benefits.sea <- data.table::rbindlist(list(mhDB.benefits.sea,
                                                    mhDB.GC.sea),
                                               use.names = TRUE)
    mhDB.GC.sea$status <- "sea"
  } else {
    mhDB.GC.sea  <- NULL
  }
  mhDB.benefits <- data.table::rbindlist(list(mhDB.benefits, mhDB.GC.sea),
                                         use.names = TRUE)

  ##### Compute for Safety Bonus ####
  cat("\nComputing safety bonus.\n")

  mhDB.SB <- mhDB %>%
    dplyr::group_by(costCenter, month, equipment) %>%
    dplyr::summarise(mh = sum(mh)) %>%
    dplyr::mutate(
      cost = round(mh * 0.2, digits = 2),
      costCenterNew = dplyr::case_when(
        grepl("1100CM", x = costCenter) ~ "1100CM",
        grepl("1100DM", x = costCenter) ~ "1100DM",
        grepl("1100MP", x = costCenter) ~ "1100MP",
        grepl("13[0-4][0-9]{2,}", x = costCenter) ~ "13102D",
        grepl("14\\d00", x = costCenter) ~ "14000",
        grepl("1100Y", x = costCenter) ~ "1100B",
        grepl("1100Z", x = costCenter) ~ "1100B",
        TRUE ~ "1100B"
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(costCenter = costCenterNew)

  #### Compute for Pag-ibig contribution of employer ####
  cat("\nComputing Pag-ibig contributions.\n")

  mhDB.PI <- dplyr::filter(mhDB, mhType %in% distType) %>%
    dplyr::group_by(ID, month, costCenter, status, equipment) %>%
    dplyr::summarise(mh = sum(mh)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(PI = 150) %>%
    dplyr::group_by(ID, month, status) %>%
    dplyr::mutate(totMH = sum(mh)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(X = mh / totMH) %>%
    dplyr::mutate(cost = round(X * PI, digits = 2))

  mhDB.PI.A <- dplyr::filter(mhDB.PI, status == "age")
  mhDB.PI <- dplyr::filter(mhDB.PI, status != "age")

  #### Compute for Phil-Health contribution of employer ####
  cat("\nComputing Philhealth contribution.\n")

  #- PHIC Contribution is fixed
  #- For monthly wagers, PHIC is based on basic monthly salary
  #- For daily wagers, PHIC is based on daily wage * 313 / 12

  PHICdb <- lapply(listR, FUN = function(x) {

    max_salary <- 80000
    phic_premium <- 0.02
    phic_premium_max <- 1600
    phic_premium_min <- 200
    if (forecast) {
      max_salary <- 70000
      phic_premium <- 0.0175
      phic_premium_max <- 1225
      phic_premium_min <- 175
    }

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
        x = tempData,
        y = dplyr::filter(wageEmp, isRF) %>% dplyr::select(-c(salH, isRF)),
        by = c("ID", "sal")
      ) %>%
        dplyr::mutate(salM = round(salM * 313 / 12, digits = 2))
    } else {
      tempData <- dplyr::left_join(
        x = tempData,
        y = dplyr::filter(wageEmp, !isRF) %>% dplyr::select(-c(salH, isRF)),
        by = c("ID", "sal")
      )
    }
    tempData <- dplyr::mutate(tempData,
                              salG = round(salM * allow, digits = 2)) %>%
      dplyr::mutate(PHIC = dplyr::case_when(
        salG < 0.01 ~ 0,
        salG < 10000.01 ~ phic_premium_min,
        salG < max_salary ~ round(salG * phic_premium, digits = 2),
        TRUE ~ phic_premium_max
      )) %>%
      dplyr::select(month, ID, PHIC)
    tempData
  }) %>%
    data.table::rbindlist(use.names = TRUE)

  mhDB.PHIC <- dplyr::filter(mhDB, mhType %in% distType) %>%
    dplyr::group_by(ID, month, costCenter, status, equipment) %>%
    dplyr::summarise(mh = sum(mh)) %>%
    dplyr::group_by(ID, month, status) %>%
    dplyr::mutate(totMH = sum(mh)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(X = mh / totMH) %>%
    dplyr::left_join(PHICdb, by = c("ID", "month")) %>%
    dplyr::mutate(cost = round(X * PHIC, digits = 2))

  mhDB.PHIC.A <- dplyr::filter(mhDB.PHIC, status == "age")
  mhDB.PHIC <- dplyr::filter(mhDB.PHIC, status != "age")

  #### Compute for Leave Commutation ####
  cat("\nComputing leave commutation.\n")

  ## Separate regular employees

  ### Extract maximum regular hours for each employee
  maxRegDB <- lapply(listR[sapply(listR, isReg)], FUN = function(x) {
    data.frame(ID = x@ID, month = 1:12, maxReg = x@maxReg)
  })

  if (length(maxRegDB) > 0) {
    ### Get total reg hours attendance (RH)
    mhDB.RH <- dplyr::filter(mhDB, status == "reg", mhType == "reg") %>%
      dplyr::group_by(ID, month) %>%
      dplyr::summarise(mh = sum(mh)) %>%
      tidyr::pivot_wider(names_from = month,
                         values_from = mh,
                         values_fill = list(mh = 0)) %>%
      tidyr::pivot_longer(names_to = "month", values_to = "mh", -ID) %>%
      dplyr::mutate(month = as.integer(month)) %>%
      dplyr::left_join(
        lapply(listR, function(x) {
          if (isReg(x)) {
            if (isRF(x)) {
              dfOut <- payB
            } else {
              dfOut <- payA
            }
            dfOut$ID <- x@ID
            return(dfOut)
          }
          return(NULL)
        }) %>%
          data.table::rbindlist()
      )

    ### Join RH to maxRegDB then compute leave hours (LH) per month
    tempID <- sapply(listR, FUN = function(x) {x@ID})
    maxRegDB <- lapply(maxRegDB, FUN = function(z) {
      z <- dplyr::left_join(x = z, y = mhDB.RH, by = c("ID", "month"))
      z[is.na(z)] <- 0L
      z$absence <- z$maxReg - z$mh

      if (any(z$absence) < 0)
        stop("Absence must not be less than 0!")

      tempIndex <- which(tempID == z$ID[1])
      LC <- listR[[tempIndex]]@leaveHours
      z$LH <- 0

      for (i in 1:12) {
        if (LC > 0) {
          minus <- min(z$absence[i], LC)
          z$LH[i] <- minus
          LC <- LC - minus
        } else {
          break
        }
      }

      if ((sum(z$LH) + LC) != listR[[tempIndex]]@leaveHours)
        stop("Leave hours do not match!")

      return(z)
    })

    maxRegDB <- data.table::rbindlist(maxRegDB, use.names = TRUE) %>%
      dplyr::left_join(dplyr::select(wageEmp, -c(salM, isRF)),
                       by = c("ID", "sal")) %>%
      dplyr::mutate(LC = salH * LH)

    ### Filter-out months with full absences but with leave
    vacationMode <- dplyr::filter(maxRegDB, LC > 0, mh < 1)

    if (nrow(vacationMode) > 0) {
      if (is.null(absenteeList))
        stop("Absentees present but without cost centers distribution.")
      absenteeID <- unique(vacationMode$ID)
      absenteeIDAbsent <- absenteeID[!absenteeID %in% absenteeList$ID]
      if (length(absenteeIDAbsent) > 0)
        stop(paste0("No distribution absentee for ", absenteeIDAbsent, "."))
      absenteeCost <- vacationMode %>%
        dplyr::select(ID, month, LC) %>%
        dplyr::inner_join(absenteeList, by = "ID") %>%
        dplyr::group_by(ID, month) %>%
        dplyr::mutate(n = dplyr::n()) %>%
        dplyr::mutate(cost = round(LC / n, digits = 2)) %>%
        dplyr::group_by(month, costCenters, ID) %>%
        dplyr::summarise(cost = sum(cost))
    }

    ### Distribute LC cost for regular employees
    mhDB.LC.R <- dplyr::filter(mhDB, mhType %in% distType, status == "reg") %>%
      dplyr::group_by(ID, month, costCenter, equipment) %>%
      dplyr::summarise(mh = sum(mh)) %>%
      dplyr::group_by(ID, month) %>%
      dplyr::mutate(totMH = sum(mh)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(X = mh / totMH) %>%
      dplyr::left_join(dplyr::select(maxRegDB, ID, month, LC),
                       by = c("ID", "month")) %>%
      dplyr::mutate(cost = round(X * LC, digits = 2))

    if (nrow(vacationMode) > 0 &&
        !is.null(absenteeCost) &&
        nrow(absenteeCost) > 0) {
      mhDB.LC.R <- dplyr::select(
        mhDB.LC.R, ID, month, costCenter, equipment, cost
      ) %>%
        dplyr::bind_rows(
          dplyr::select(
            absenteeCost, ID, month, costCenter = costCenters, equipment, cost)
        )
    }
  } else {
    mhDB.LC.R <- NULL
  }

  ## Separate seasonal and agency employees
  ##- These must be separated since they are not entitled to negotiated
  ##- holiday premium.
  ##- Probationary employees have no leave commutation.

  ### Get leave hours
  LH <- lapply(listR[sapply(listR, FUN = function(x) {
    x@status %in% c("sea", "age")
  })], FUN = function(x) {
    data.frame(ID = x@ID, LH = x@leaveHours, sal = "a")
  }) %>% data.table::rbindlist(use.names = TRUE)

  if (nrow(LH) > 0) {
    ### Get LC cost per employee for the whole year
    LH <- dplyr::left_join(x = LH,
                           y = dplyr::select(wageEmp, -c(salM, isRF)),
                           by = c("ID", "sal")) %>%
      dplyr::mutate(LC = LH * salH)

    ### Distribute LC throughout the year
    mhDB.LC.S <- dplyr::filter(mhDB,
                               status %in% c("sea", "age"),
                               mhType %in% distType) %>%
      dplyr::group_by(ID, month, costCenter, status, equipment) %>%
      dplyr::summarise(mh = sum(mh)) %>%
      dplyr::group_by(ID) %>%
      dplyr::mutate(totMH = sum(mh)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(X = mh / totMH) %>%
      dplyr::left_join(dplyr::select(LH, ID, LC), by = "ID") %>%
      dplyr::mutate(cost = round(X * LC, digits = 2))
  } else {
    mhDB.LC.S <- data.table::data.table(ID = character(),
                                        status = character(),
                                        costCenter = character(),
                                        month = integer(),
                                        equipment = character(),
                                        cost = numeric())
  }

  ## Get cost
  mhDB.LC <- dplyr::filter(mhDB.LC.S, status == "sea") %>%
    dplyr::select(costCenter, month, equipment, cost)
  mhDB.LC.A <- dplyr::filter(mhDB.LC.S, status == "age") %>%
    dplyr::select(costCenter, month, equipment, cost)

  ##### Compute for SSS contribution of employer ####
  cat("\nComputing SSS contribution.\n")

  #- SSS Contribution is based on gross salary

  SSSdb <- data.frame(
    ID = c(hol.mhDB.d.R$ID,
           hol.mhDB.d.S$ID,
           mhDB.d.P.OT$ID,
           mhDB.d.P.OT$ID,
           mhDB.d.P.Reg$ID,
           mhDB.d.P.Reg$ID,
           mhDB.d.R.OT$ID,
           mhDB.d.R.OT$ID,
           mhDB.d.R.Reg$ID,
           mhDB.d.R.Reg$ID,
           mhDB.d.S.OT$ID,
           mhDB.d.S.OT$ID,
           mhDB.d.S.Reg$ID,
           mhDB.d.S.Reg$ID,
           mhDB.m.P.OT$ID,
           mhDB.m.P.OT$ID,
           mhDB.m.P.Reg$ID,
           mhDB.m.P.Reg$ID,
           mhDB.m.R.OT$ID,
           mhDB.m.R.OT$ID,
           mhDB.m.R.Reg$ID,
           mhDB.m.R.Reg$ID,
           mhDB.m.S.OT$ID,
           mhDB.m.S.OT$ID,
           mhDB.m.S.Reg$ID,
           mhDB.m.S.Reg$ID,
           mhDB.LC.R$ID),
    costCenter = c(hol.mhDB.d.R$costCenter,
                   hol.mhDB.d.S$costCenter,
                   mhDB.d.P.OT$costCenter,
                   mhDB.d.P.OT$costCenter,
                   mhDB.d.P.Reg$costCenter,
                   mhDB.d.P.Reg$costCenter,
                   mhDB.d.R.OT$costCenter,
                   mhDB.d.R.OT$costCenter,
                   mhDB.d.R.Reg$costCenter,
                   mhDB.d.R.Reg$costCenter,
                   mhDB.d.S.OT$costCenter,
                   mhDB.d.S.OT$costCenter,
                   mhDB.d.S.Reg$costCenter,
                   mhDB.d.S.Reg$costCenter,
                   mhDB.m.P.OT$costCenter,
                   mhDB.m.P.OT$costCenter,
                   mhDB.m.P.Reg$costCenter,
                   mhDB.m.P.Reg$costCenter,
                   mhDB.m.R.OT$costCenter,
                   mhDB.m.R.OT$costCenter,
                   mhDB.m.R.Reg$costCenter,
                   mhDB.m.R.Reg$costCenter,
                   mhDB.m.S.OT$costCenter,
                   mhDB.m.S.OT$costCenter,
                   mhDB.m.S.Reg$costCenter,
                   mhDB.m.S.Reg$costCenter,
                   mhDB.LC.R$costCenter),
    month = c(hol.mhDB.d.R$month,
              hol.mhDB.d.S$month,
              mhDB.d.P.OT$month,
              mhDB.d.P.OT$month,
              mhDB.d.P.Reg$month,
              mhDB.d.P.Reg$month,
              mhDB.d.R.OT$month,
              mhDB.d.R.OT$month,
              mhDB.d.R.Reg$month,
              mhDB.d.R.Reg$month,
              mhDB.d.S.OT$month,
              mhDB.d.S.OT$month,
              mhDB.d.S.Reg$month,
              mhDB.d.S.Reg$month,
              mhDB.m.P.OT$month,
              mhDB.m.P.OT$month,
              mhDB.m.P.Reg$month,
              mhDB.m.P.Reg$month,
              mhDB.m.R.OT$month,
              mhDB.m.R.OT$month,
              mhDB.m.R.Reg$month,
              mhDB.m.R.Reg$month,
              mhDB.m.S.OT$month,
              mhDB.m.S.OT$month,
              mhDB.m.S.Reg$month,
              mhDB.m.S.Reg$month,
              mhDB.LC.R$month),
    equipment = c(hol.mhDB.d.R$equipment,
                  hol.mhDB.d.S$equipment,
                  mhDB.d.P.OT$equipment,
                  mhDB.d.P.OT$equipment,
                  mhDB.d.P.Reg$equipment,
                  mhDB.d.P.Reg$equipment,
                  mhDB.d.R.OT$equipment,
                  mhDB.d.R.OT$equipment,
                  mhDB.d.R.Reg$equipment,
                  mhDB.d.R.Reg$equipment,
                  mhDB.d.S.OT$equipment,
                  mhDB.d.S.OT$equipment,
                  mhDB.d.S.Reg$equipment,
                  mhDB.d.S.Reg$equipment,
                  mhDB.m.P.OT$equipment,
                  mhDB.m.P.OT$equipment,
                  mhDB.m.P.Reg$equipment,
                  mhDB.m.P.Reg$equipment,
                  mhDB.m.R.OT$equipment,
                  mhDB.m.R.OT$equipment,
                  mhDB.m.R.Reg$equipment,
                  mhDB.m.R.Reg$equipment,
                  mhDB.m.S.OT$equipment,
                  mhDB.m.S.OT$equipment,
                  mhDB.m.S.Reg$equipment,
                  mhDB.m.S.Reg$equipment,
                  mhDB.LC.R$equipment),
    cost = c(hol.mhDB.d.R$XcostWage,
             hol.mhDB.d.S$XcostWage,
             mhDB.d.P.OT$costNP,
             mhDB.d.P.OT$costWage,
             mhDB.d.P.Reg$costNP,
             mhDB.d.P.Reg$costWage,
             mhDB.d.R.OT$costNP,
             mhDB.d.R.OT$costWage,
             mhDB.d.R.Reg$costNP,
             mhDB.d.R.Reg$costWage,
             mhDB.d.S.OT$costNP,
             mhDB.d.S.OT$costWage,
             mhDB.d.S.Reg$costNP,
             mhDB.d.S.Reg$costWage,
             mhDB.m.P.OT$costNP,
             mhDB.m.P.OT$costWage,
             mhDB.m.P.Reg$costNP,
             mhDB.m.P.Reg$costWage,
             mhDB.m.R.OT$costNP,
             mhDB.m.R.OT$costWage,
             mhDB.m.R.Reg$costNP,
             mhDB.m.R.Reg$costWage,
             mhDB.m.S.OT$costNP,
             mhDB.m.S.OT$costWage,
             mhDB.m.S.Reg$costNP,
             mhDB.m.S.Reg$costWage,
             mhDB.LC.R$cost)
  ) %>%
    dplyr::group_by(ID, month, costCenter, equipment) %>%
    dplyr::summarise(cost = sum(cost)) %>%
    dplyr::filter(cost > 0) %>%
    dplyr::group_by(ID, month) %>%
    dplyr::mutate(totalCost = round(sum(cost), digits = 2)) %>%
    dplyr::mutate(X = cost / totalCost)
  SSSdb$SSSRate <- sapply(SSSdb$totalCost, function(x) {
    SSS$c[which(SSS$r1 <= x & SSS$r2 >= x)]
  })
  SSSdb$SSS <- SSSdb$X * SSSdb$SSSRate

  mhDB.SSS <- dplyr::group_by(SSSdb, ID, month, costCenter, equipment) %>%
    dplyr::summarise(cost = round(sum(SSS), digits = 2)) %>%
    dplyr::left_join(
      lapply(listR, function(x) {
        data.frame(ID = x@ID, status = x@status)
      }) %>%
        data.table::rbindlist(use.names = TRUE)
    ) %>%
    dplyr::ungroup()

  mhDB.SSS.A <- dplyr::filter(mhDB.SSS, status == "age")
  mhDB.SSS <- dplyr::filter(mhDB.SSS, status != "age")

  #### Compute for Hospital and Medical Expenses ####
  cat("\nComputing hospital and medical expenses.\n")

  hm <- data.table::rbindlist(lapply(listR, getHM), use.names = TRUE)

  mhDB.HM <- dplyr::filter(mhDB, mhType %in% distType) %>%
    dplyr::group_by(ID, month, costCenter, status, equipment) %>%
    dplyr::summarise(mh = sum(mh)) %>%
    dplyr::group_by(ID, month, status) %>%
    dplyr::mutate(totMH = sum(mh)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(hm, by = c("ID", "month")) %>%
    dplyr::mutate(X = mh / totMH) %>%
    dplyr::mutate(cost = round(X * HM, digits = 2))

  mhDB.HM.A <- dplyr::filter(mhDB.HM, status == "age")
  mhDB.HM <- dplyr::filter(mhDB.HM, status != "age")

  #### Compute for 13th month pay ####
  cat("\nComputing 13th month pay.\n")

  mp13 <- data.table::rbindlist(lapply(listR, FUN = function(x) {

    tempIndex <- which(wageEmp$ID == x@ID)
    tempSal <- wageEmp$salM[tempIndex]
    tempData <- get13mp(theObject = x, sal = tempSal)

    return(tempData)
  }), use.names = TRUE)

  mhDB.13mp <- dplyr::filter(mhDB, mhType %in% distType) %>%
    dplyr::group_by(ID, month, costCenter, status, equipment) %>%
    dplyr::summarise(mh = sum(mh)) %>%
    dplyr::group_by(ID, month, status) %>%
    dplyr::mutate(totMH = sum(mh)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(X = mh / totMH) %>%
    dplyr::left_join(mp13, by = c("ID", "month")) %>%
    dplyr::mutate(cost = round(X * mp, digits = 2)) %>%
    dplyr::group_by(costCenter, status, month, equipment) %>%
    dplyr::summarise(cost = sum(cost)) %>%
    dplyr::ungroup()

  mhDB.13mp.A <- dplyr::filter(mhDB.13mp, status == "age")
  mhDB.13mp <- dplyr::filter(mhDB.13mp, status != "age")

  ## Separate 13th month pay for regular and non-regular in-house as requested
  ## by accounting

  accr.13mp <- dplyr::group_by(
    mhDB.13mp, status, costCenter, month, equipment
  ) %>%
    dplyr::summarise(cost = sum(cost)) %>%
    tidyr::spread(month, cost, fill = 0)

  # Sum all man-hours
  mhDB.mh1 <- dplyr::group_by(mhDB, costCenter, month, equipment) %>%
    dplyr::summarise(cost = sum(mh))

  mhDB.mh2 <- dplyr::group_by(hol.mhDB.m, costCenter, month, equipment) %>%
    dplyr::summarise(cost = sum(XholHours))

  mhDB.mh3 <- dplyr::group_by(hol.mhDB.d, costCenter, month, equipment) %>%
    dplyr::summarise(cost = sum(XholHours))

  mhDB.mh <- data.table::rbindlist(list(mhDB.mh1, mhDB.mh2, mhDB.mh3),
                                   use.names = TRUE)

  #### Compute for Mid-year and Year-end bonus ####
  cat("\nComputing bonus.\n")

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
      tempData <- dplyr::left_join(x = tempData, y = payB, by = "month") %>%
        dplyr::left_join(
          y  = dplyr::filter(wageEmp, isRF) %>% dplyr::select(-c(salH, isRF)),
          by = c("ID", "sal")
        ) %>%
        dplyr::mutate(salM = round(salM * 26, digits = 2))
    } else {
      tempData <- dplyr::left_join(x = tempData, y = payA, by = "month") %>%
        dplyr::left_join(
          dplyr::filter(wageEmp, !isRF) %>% dplyr::select(-c(salH, isRF)),
          by = c("ID", "sal")
        )
    }

    tempData$salG  <- round(tempData$salM * tempData$allow, digits = 2)
    tempData$bonus <-
      tempData$salG * c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, bonusFactorYearEnd)

    tempData <- tempData[, colnames(tempData) %in% c("month", "ID", "bonus")]
    tempData <- as.data.frame(tempData)

    return(tempData)
  }) %>%
    data.table::rbindlist(use.names = TRUE)

  if (length(bonus) > 0) {

    # Distribute mid-year bonus to all manhours from January to May.
    # TODO: in forecast, use actual manhours per employee.
    # If forecasting, temporarily drop man-hours before the start of forecasted
    # month so the the cost center "0-0" is not given bonus
    mhDB.bonusMid <- dplyr::filter(mhDB,
                                   mhType %in% distType,
                                   month < 6,
                                   month >= monthStart,
                                   status == "reg") %>%
      dplyr::group_by(ID, costCenter, equipment) %>%
      dplyr::summarise(mh = sum(mh)) %>%
      dplyr::group_by(ID) %>%
      dplyr::mutate(totMH = sum(mh), month = 5L) %>%
      dplyr::mutate(X = mh / totMH) %>%
      dplyr::left_join(bonus, by = c("ID", "month"))

    # Distribute year-and bonus to all manhours from January to December
    # TODO: in forecast, use actual manhours per employee.
    # If forecasting, temporarily drop man-hours before the start of forecasted
    # month so the the cost center "0-0" is not given bonus
    mhDB.bonusEnd <- dplyr::filter(
      mhDB, mhType %in% distType, month >= monthStart, status == "reg"
    ) %>%
      dplyr::group_by(ID, costCenter, equipment) %>%
      dplyr::summarise(mh = sum(mh)) %>%
      dplyr::group_by(ID) %>%
      dplyr::mutate(totMH = sum(mh), month = 12L) %>%
      dplyr::mutate(X = mh / totMH) %>%
      dplyr::left_join(bonus, by = c("ID", "month"))

    mhDB.bonus <- data.table::rbindlist(list(mhDB.bonusMid, mhDB.bonusEnd),
                                        use.names = TRUE) %>%
      dplyr::mutate(cost = round(X * bonus, digits = 2)) %>%
      dplyr::filter(!is.na(cost)) %>%
      dplyr::group_by(costCenter, month, equipment) %>%
      dplyr::summarise(cost = sum(cost))
  } else {
    mhDB.bonus <- data.table::data.table(costCenter = character(),
                                         month = integer(),
                                         equipment = character(),
                                         cost = numeric())
  }


  #### Compute for Leave Conversion of Regular Employees ####
  cat("\nComputing leave conversion.\n")

  leaveConversion <- lapply(listR, FUN = function(x) {

    if (!isReg(x))
      return(NULL)

    tempData <- getCM(x)

    if (isRF(x)) {
      tempData <- dplyr::left_join(x = tempData, y = payB, by = "month") %>%
        dplyr::left_join(
          y  = dplyr::filter(wageEmp, isRF) %>% dplyr::select(-c(salH, isRF)),
          by = c("ID", "sal")
        ) %>%
        dplyr::mutate(salM = round(salM * 26, digits = 2))
    } else {
      tempData <- dplyr::left_join(x = tempData, y = payA, by = "month") %>%
        dplyr::left_join(
          dplyr::filter(wageEmp, !isRF) %>% dplyr::select(-c(salH, isRF)),
          by = c("ID", "sal")
        )
    }

    tempData$salG  <- round(tempData$salM * tempData$allow, digits = 2)
    tempData$conversion <-
      tempData$salG * c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, leaveConversionFactor)

    tempData <- tempData[, colnames(tempData) %in% c("month", "ID", "conversion")]
    tempData <- as.data.frame(tempData)

    return(tempData)
  }) %>%
    data.table::rbindlist(use.names = TRUE)

  if (length(leaveConversion) > 0) {
    # Distribute leave conversion to all manhours from January to December
    mhDB.leaveConversion <- dplyr::filter(
      mhDB, mhType %in% distType, month >= monthStart, status == "reg"
    ) %>%
      dplyr::group_by(ID, costCenter, equipment) %>%
      dplyr::summarise(mh = sum(mh)) %>%
      dplyr::group_by(ID) %>%
      dplyr::mutate(totMH = sum(mh)) %>%
      dplyr::mutate(month = 12L, X = mh / totMH) %>%
      dplyr::left_join(leaveConversion, by = c("ID", "month")) %>%
      dplyr::mutate(cost = round(X * conversion, digits = 2)) %>%
      dplyr::filter(!is.na(cost)) %>%
      dplyr::group_by(costCenter, month, equipment) %>%
      dplyr::summarise(cost = sum(cost))
  } else {
    mhDB.leaveConversion <- data.table::data.table(costCenter = character(),
                                                   month = integer(),
                                                   equipment = character(),
                                                   cost = numeric())
  }

  #### Compute for Seasonal Employees Bonuses ####

  cat("\nComputing signing bonus for seasonal employees.\n")

  signingBonusSea <- lapply(listR, getSigningBonusSea) %>%
    data.table::rbindlist(use.names = TRUE)

  # Distribute seasonal signing bonus
  mhDB.signingBonusSea <- dplyr::filter(mhDB, status == "sea") %>%
    dplyr::group_by(ID, costCenter, month, equipment) %>%
    dplyr::summarise(mh = sum(mh)) %>%
    dplyr::group_by(ID, month) %>%
    dplyr::mutate(totMH = sum(mh)) %>%
    dplyr::mutate(X = mh / totMH) %>%
    dplyr::left_join(signingBonusSea, by = c("ID", "month")) %>%
    dplyr::mutate(benefits = signingBonus * X) %>%
    dplyr::group_by(costCenter, month, equipment) %>%
    dplyr::summarise(cost = round(sum(benefits), digits = 2))

  cat("\nComputing retention bonus for seasonal employees.\n")

  retentionBonusSea <- lapply(listR, getRetentionBonus) %>%
    data.table::rbindlist(use.names = TRUE)

  # Distribute seasonal retention bonus
  mhDB.retentionBonusSea <- dplyr::filter(mhDB, status == "sea") %>%
    dplyr::group_by(ID, costCenter, month, equipment) %>%
    dplyr::summarise(mh = sum(mh)) %>%
    dplyr::group_by(ID) %>%
    dplyr::mutate(totMH = sum(mh)) %>%
    dplyr::group_by(ID, costCenter) %>%
    dplyr::mutate(totMHCostCenter = sum(mh)) %>%
    dplyr::mutate(X = totMHCostCenter / totMH) %>%
    dplyr::left_join(retentionBonusSea, by = c("ID", "month")) %>%
    dplyr::mutate(benefits = retentionBonus * X) %>%
    dplyr::group_by(costCenter, month, equipment) %>%
    dplyr::summarise(cost = round(sum(benefits), digits = 2))

  # Combine Bonuses
  mhDB.RB <- data.table::rbindlist(list(mhDB.signingBonusSea,
                                        mhDB.retentionBonusSea),
                                   use.names = TRUE) %>%
    dplyr::group_by(costCenter, month, equipment) %>%
    dplyr::summarise(cost = sum(cost))

  ####  Compute for Rice Subsidy for Agency ####
  cat("\nComputing rice subsidy (agency).\n")

  listR.A <- listR[sapply(listR, function(x) {return(x@status == "age")})]

  if (length(listR.A) > 0) {

    ## Get rice subsidy per month per employee
    riceSub.A <- lapply(listR.A, FUN = getRiceSub) %>%
      data.table::rbindlist(use.names = TRUE)

    ## Distribute rice subsidy
    mhDB.riceSub.A <- dplyr::filter(mhDB,
                                    mhType %in% distType,
                                    status == "age") %>%
      dplyr::group_by(ID, month, costCenter, equipment) %>%
      dplyr::summarise(mh = sum(mh)) %>%
      dplyr::group_by(ID, month) %>%
      dplyr::mutate(totMH = sum(mh)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(X = mh / totMH) %>%
      dplyr::left_join(riceSub.A, by = c("ID", "month")) %>%
      dplyr::mutate(cost = round(X * riceSub, digits = 2))
  } else {
    mhDB.riceSub.A <- NULL
  }

  ##### Compute for Rice Subsidy for in-house ####
  cat("\nComputing rice subsidy (in-house).\n")

  listR.I <- listR[sapply(listR, function(x) {return(x@status != "age")})]

  if (length(listR.I) > 0) {

    ## Get rice subsidy per month per employee
    riceSub.I <- lapply(listR.I, FUN = getRiceSub) %>%
      data.table::rbindlist(use.names = TRUE)

    ## Distribute rice subsidy
    mhDB.riceSub.I <- dplyr::filter(mhDB,
                                    mhType %in% distType,
                                    status != "age") %>%
      dplyr::group_by(ID, month, costCenter, status, equipment) %>%
      dplyr::summarise(mh = sum(mh)) %>%
      dplyr::group_by(ID, month) %>%
      dplyr::mutate(totMH = sum(mh)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(X = mh / totMH) %>%
      dplyr::left_join(riceSub.I, by = c("ID", "month")) %>%
      dplyr::mutate(cost = round(X * riceSub, digits = 2))
  } else {
    mhDB.riceSub.I <- NULL
  }

  #### Merged Costs ####
  cat("\nMerging costs.\n")

  c60300000 <- dplyr::bind_rows(
    data.frame(costCenter = mhDB.m.R.Reg$costCenter,
               month = mhDB.m.R.Reg$month,
               equipment = mhDB.m.R.Reg$equipment,
               cost = mhDB.m.R.Reg$costWage),
    data.frame(costCenter = mhDB.d.R.Reg$costCenter,
               month = mhDB.d.R.Reg$month,
               equipment = mhDB.d.R.Reg$equipment,
               cost = mhDB.d.R.Reg$costWage),
    data.frame(costCenter = hol.mhDB.d.R$costCenter,
               month = hol.mhDB.d.R$month,
               equipment = hol.mhDB.d.R$equipment,
               cost = hol.mhDB.d.R$XcostWage),
    data.frame(costCenter = mhDB.m.P.Reg$costCenter,
               month = mhDB.m.P.Reg$month,
               equipment = mhDB.m.P.Reg$equipment,
               cost = mhDB.m.P.Reg$costWage),
    data.frame(costCenter = mhDB.d.P.Reg$costCenter,
               month = mhDB.d.P.Reg$month,
               equipment = mhDB.d.P.Reg$equipment,
               cost = mhDB.d.P.Reg$costWage),
    data.frame(costCenter = mhDB.LC.R$costCenter,
               month = mhDB.LC.R$month,
               equipment = mhDB.LC.R$equipment,
               cost = mhDB.LC.R$cost)
  ) %>%
    dplyr::mutate(description = "Payroll - Salaries (Regular)")

  c60300001 <- dplyr::bind_rows(
    data.frame(costCenter = mhDB.m.S.Reg$costCenter,
               month = mhDB.m.S.Reg$month,
               equipment = mhDB.m.S.Reg$equipment,
               cost = mhDB.m.S.Reg$costWage),
    data.frame(costCenter = mhDB.d.S.Reg$costCenter,
               month = mhDB.d.S.Reg$month,
               equipment = mhDB.d.S.Reg$equipment,
               cost = mhDB.d.S.Reg$costWage),
    data.frame(costCenter = hol.mhDB.d.S$costCenter,
               month = hol.mhDB.d.S$month,
               equipment = hol.mhDB.d.S$equipment,
               cost = hol.mhDB.d.S$XcostWage)
  ) %>%
    dplyr::mutate(description = "Payroll - Salaries (Seasonal)")

  c60300004 <- dplyr::bind_rows(
    data.frame(costCenter = mhDB.m.R.Reg$costCenter,
               month = mhDB.m.R.Reg$month,
               equipment = mhDB.m.R.Reg$equipment,
               cost = mhDB.m.R.Reg$costNP),
    data.frame(costCenter = mhDB.m.R.OT$costCenter,
               month = mhDB.m.R.OT$month,
               equipment = mhDB.m.R.OT$equipment,
               cost = mhDB.m.R.OT$costWage),
    data.frame(costCenter = mhDB.m.R.OT$costCenter,
               month = mhDB.m.R.OT$month,
               equipment = mhDB.m.R.OT$equipment,
               cost = mhDB.m.R.OT$costNP),
    data.frame(costCenter = mhDB.d.R.Reg$costCenter,
               month = mhDB.d.R.Reg$month,
               equipment = mhDB.d.R.Reg$equipment,
               cost = mhDB.d.R.Reg$costNP),
    data.frame(costCenter = mhDB.d.R.OT$costCenter,
               month = mhDB.d.R.OT$month,
               equipment = mhDB.d.R.OT$equipment,
               cost = mhDB.d.R.OT$costWage),
    data.frame(costCenter = mhDB.d.R.OT$costCenter,
               month = mhDB.d.R.OT$month,
               equipment = mhDB.d.R.OT$equipment,
               cost = mhDB.d.R.OT$costNP),
    data.frame(costCenter = mhDB.m.P.Reg$costCenter,
               month = mhDB.m.P.Reg$month,
               equipment = mhDB.m.P.Reg$equipment,
               cost = mhDB.m.P.Reg$costNP),
    data.frame(costCenter = mhDB.m.P.OT$costCenter,
               month = mhDB.m.P.OT$month,
               equipment = mhDB.m.P.OT$equipment,
               cost = mhDB.m.P.OT$costWage),
    data.frame(costCenter = mhDB.m.P.OT$costCenter,
               month = mhDB.m.P.OT$month,
               equipment = mhDB.m.P.OT$equipment,
               cost = mhDB.m.P.OT$costNP),
    data.frame(costCenter = mhDB.d.P.Reg$costCenter,
               month = mhDB.d.P.Reg$month,
               equipment = mhDB.d.P.Reg$equipment,
               cost = mhDB.d.P.Reg$costNP),
    data.frame(costCenter = mhDB.d.P.OT$costCenter,
               month = mhDB.d.P.OT$month,
               equipment = mhDB.d.P.OT$equipment,
               cost = mhDB.d.P.OT$costWage),
    data.frame(costCenter = mhDB.d.P.OT$costCenter,
               month = mhDB.d.P.OT$month,
               equipment = mhDB.d.P.OT$equipment,
               cost = mhDB.d.P.OT$costNP)
  ) %>%
    dplyr::mutate(description = "Payroll - Overtime (Regular)")

  c60300005 <- dplyr::bind_rows(
    data.frame(costCenter = mhDB.m.S.Reg$costCenter,
               month = mhDB.m.S.Reg$month,
               equipment = mhDB.m.S.Reg$equipment,
               cost = mhDB.m.S.Reg$costNP),
    data.frame(costCenter = mhDB.m.S.OT$costCenter,
               month = mhDB.m.S.OT$month,
               equipment = mhDB.m.S.OT$equipment,
               cost = mhDB.m.S.OT$costWage),
    data.frame(costCenter = mhDB.m.S.OT$costCenter,
               month = mhDB.m.S.OT$month,
               equipment = mhDB.m.S.OT$equipment,
               cost = mhDB.m.S.OT$costNP),
    data.frame(costCenter = mhDB.d.S.Reg$costCenter,
               month = mhDB.d.S.Reg$month,
               equipment = mhDB.d.S.Reg$equipment,
               cost = mhDB.d.S.Reg$costNP),
    data.frame(costCenter = mhDB.d.S.OT$costCenter,
               month = mhDB.d.S.OT$month,
               equipment = mhDB.d.S.OT$equipment,
               cost = mhDB.d.S.OT$costWage),
    data.frame(costCenter = mhDB.d.S.OT$costCenter,
               month = mhDB.d.S.OT$month,
               equipment = mhDB.d.S.OT$equipment,
               cost = mhDB.d.S.OT$costNP)
  ) %>%
    dplyr::mutate(description = "Payroll - Overtime (Seasonal)")

  c60300008 <- dplyr::bind_rows(
    dplyr::filter(mhDB.13mp, status != "sea") %>%
      dplyr::select(costCenter, month, equipment, cost),
    dplyr::select(mhDB.bonus, costCenter, month, equipment, cost),
    dplyr::select(mhDB.signingBonus, costCenter, month, equipment, cost)
  ) %>%
    dplyr::mutate(description = "Payroll - 13th Month and Other Bonuses (Regular)")

  c60300009 <- dplyr::bind_rows(
    dplyr::filter(mhDB.13mp, status == "sea") %>%
      dplyr::select(costCenter, month, equipment, cost),
    dplyr::select(mhDB.RB, costCenter, month, equipment, cost)
  ) %>%
    dplyr::mutate(description = "Payroll - 13th Month and Other Bonuses (Seasonal)")


  c60300011 <- dplyr::select(
    mhDB.leaveConversion, costCenter, month, equipment, cost
  ) %>%
    dplyr::mutate(description = "Payroll - Leave Commutation (Regular)")

  c60300012 <- dplyr::select(mhDB.LC, costCenter, month, equipment, cost) %>%
    dplyr::mutate(description = "Payroll - Leave Commutation (Seasonal)")

  c60300014 <- dplyr::bind_rows(
    dplyr::filter(mhDB.benefits, status != "sea") %>%
      dplyr::select(costCenter, month, equipment, cost),
    dplyr::filter(mhDB.riceSub.I, status != "sea") %>%
      dplyr::select(costCenter, month, equipment, cost)
  ) %>%
    dplyr::mutate(description = "Payroll - De Minimis Benefits (Regular)")

  c60300015 <- dplyr::bind_rows(
    dplyr::filter(mhDB.benefits, status == "sea") %>%
      dplyr::select(costCenter, month, equipment, cost),
    dplyr::filter(mhDB.riceSub.I, status == "sea") %>%
      dplyr::select(costCenter, month, equipment, cost)
  ) %>%
    dplyr::mutate(description = "Payroll - De Minimis Benefits (Seasonal)")

  c60300017 <- dplyr::filter(mhDB.SSS, status != "sea") %>%
    dplyr::select(costCenter, month, equipment, cost) %>%
    dplyr::mutate(description = "SSS Contributions (Regular)")

  c60300018 <- dplyr::filter(mhDB.SSS, status == "sea") %>%
    dplyr::select(costCenter, month, equipment, cost) %>%
    dplyr::mutate(description = "SSS Contributions (Seasonal)")

  c60300020 <- dplyr::filter(mhDB.PHIC, status != "sea") %>%
    dplyr::select(costCenter, month, equipment, cost) %>%
    dplyr::mutate(description = "PHIC Contributions (Regular)")

  c60300021 <- dplyr::filter(mhDB.PHIC, status == "sea") %>%
    dplyr::select(costCenter, month, equipment, cost) %>%
    dplyr::mutate(description = "PHIC Contributions (Seasonal)")

  c60300023 <- dplyr::filter(mhDB.PI, status != "sea") %>%
    dplyr::select(costCenter, month, equipment, cost) %>%
    dplyr::mutate(description = "HDMF Contributions (Regular)")

  c60300024 <- dplyr::filter(mhDB.PI, status == "sea") %>%
    dplyr::select(costCenter, month, equipment, cost) %>%
    dplyr::mutate(description = "HDMF Contributions (Seasonal)")

  c60300036 <- dplyr::select(mhDB.SB, -c(mh, costCenterNew)) %>%
    dplyr::mutate(description = "Awards and Prizes")

  c60300029 <- dplyr::group_by(mhDB.allow, costCenter, month, equipment) %>%
    dplyr::summarise(cost = sum(cost)) %>%
    dplyr::mutate(description = "Other Employee Allowances")

  c60300033 <- dplyr::select(mhDB.HM, costCenter, month, equipment, cost) %>%
    dplyr::mutate(description = "Hospital and Medical Expenses - Employees")

  c60900000 <- dplyr::bind_rows(
    dplyr::group_by(mhDB.hmo, costCenter, month, equipment) %>%
      dplyr::summarise(cost = sum(cost)),
    dplyr::group_by(mhDB.groupLife, costCenter, month, equipment) %>%
      dplyr::summarise(cost = sum(cost))
  )  %>%
    dplyr::mutate(description = "Life and Medical Insurance - Employees")

  c61100000 <- dplyr::bind_rows(
    data.frame(costCenter = mhDB.d.A$costCenter,
               month = mhDB.d.A$month,
               equipment = mhDB.d.A$equipment,
               cost = mhDB.d.A$cost),
    data.frame(costCenter = hol.mhDB.d.A$costCenter,
               month = hol.mhDB.d.A$month,
               equipment = hol.mhDB.d.A$equipment,
               cost = hol.mhDB.d.A$XcostWage),
    data.frame(costCenter = mhDB.SSS.A$costCenter,
               month = mhDB.SSS.A$month,
               equipment = mhDB.SSS.A$equipment,
               cost = mhDB.SSS.A$cost),
    data.frame(costCenter = mhDB.PI.A$costCenter,
               month = mhDB.PI.A$month,
               equipment = mhDB.PI.A$equipment,
               cost = mhDB.PI.A$cost),
    data.frame(costCenter = mhDB.PHIC.A$costCenter,
               month = mhDB.PHIC.A$month,
               equipment = mhDB.PHIC.A$equipment,
               cost = mhDB.PHIC.A$cost),
    data.frame(costCenter = mhDB.LC.A$costCenter,
               month = mhDB.LC.A$month,
               equipment = mhDB.LC.A$equipment,
               cost = mhDB.LC.A$cost),
    data.frame(costCenter = mhDB.HM.A$costCenter,
               month = mhDB.HM.A$month,
               equipment = mhDB.HM.A$equipment,
               cost = mhDB.HM.A$cost),
    data.frame(costCenter = mhDB.13mp.A$costCenter,
               month = mhDB.13mp.A$month,
               equipment = mhDB.13mp.A$equipment,
               cost = mhDB.13mp.A$cost)
  ) %>%
    dplyr::mutate(cost = round(cost * 1.3117, digits = 2)) %>%
    dplyr::bind_rows(
      data.frame(costCenter = mhDB.riceSub.A$costCenter,
                 month = mhDB.riceSub.A$month,
                 equipment = mhDB.riceSub.A$equipment,
                 cost = mhDB.riceSub.A$cost)
    ) %>%
    dplyr::mutate(description = "Contract Fee - Agency Services")

  c61300016 <- dplyr::group_by(
    mhDB.safetyGadgets, costCenter, month, equipment
  ) %>%
    dplyr::summarise(cost = sum(cost)) %>%
    dplyr::mutate(description = "Safety Equipment and Supplies")

  c99999999 <- as.data.frame(mhDB.mh) %>%
    dplyr::mutate(description = "TMC Tools - Man-hours")

  costDB <- dplyr::bind_rows(c60300000,
                             c60300001,
                             c60300004,
                             c60300005,
                             c60300008,
                             c60300009,
                             c60300011,
                             c60300012,
                             c60300014,
                             c60300015,
                             c60300017,
                             c60300018,
                             c60300020,
                             c60300021,
                             c60300023,
                             c60300024,
                             c60300036,
                             c60300029,
                             c60300033,
                             c60900000,
                             c61100000,
                             c61300016,
                             c99999999) %>%
    dplyr::group_by(costCenter, description, month, equipment) %>%
    dplyr::summarise(cost = sum(cost)) %>%
    dplyr::left_join(y = mansched::acSAP, by = "description") %>%
    tidyr::spread(month, cost, fill = 0) %>%
    as.data.frame()

  costCenter <- unique(costDB$costCenter)

  cat("\nExporting data.\n")
  missingCols <- (1:12)[which(!1:12 %in% colnames(costDB))]
  for (i in missingCols) {
    cmd <- paste0("costDB$`", i, "` <- 0")
    eval(parse(text = cmd))
  }
  costDB <- costDB[, c("costCenter",
                       "description",
                       "equipment",
                       "code",
                       as.character(1:12))]

  export.mh <- dplyr::filter(costDB, code == 99999999L) %>%
    dplyr::select(-c(description, code)) %>%
    dplyr::arrange(costCenter, equipment) %>%
    as.data.frame()
  missingCols <- (1:12)[which(!1:12 %in% colnames(export.mh))]
  for (i in missingCols) {
    cmd <- paste0("export.mh$`", i, "` <- 0")
    eval(parse(text = cmd))
  }
  export.mh <- export.mh[, c("costCenter", "equipment", as.character(1:12))]
  export.mh$SUM <- apply(export.mh[, 3:14], MARGIN = 1, FUN = sum)

  return(list(costDB, export.mh, accr.13mp, mhDB.bonus))
}
