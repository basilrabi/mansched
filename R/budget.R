#' Compute budget
#'
#' Given a xlsx file containing the employee pool, requirements, wages, and
#'   work schedules, the budget is computed.
#'
#' @param myFile full path of the spreadsheet
#' @param year integer value defining the year to be budgeted
#' @param forecast logical value \cr
#'   Compute cost for forecast?
#' @return NULL
#' @export budget
#' @importFrom readxl read_xlsx
#' @importFrom xlsx write.xlsx
#' @importFrom stringr str_to_upper
budget <- function(myFile, year, forecast = FALSE) {

  # setwd("/home/brabi/sampleBudget/")
  # library(magrittr)

  # myFile <- "MinesBudget2018.xlsx"

  # myFile <- "sampleData.xlsx"
  # year <- 2018
  # forecast <- FALSE

  empReq.colnames <- c("activity",
                       "personnelClass",
                       "quantity",
                       "spareFactor",
                       "equipment",
                       "OT",
                       "costCode")

  empReq <- readxl::read_xlsx(path = myFile,
                              sheet = "Requirement")

  empReq <- empReq[,colnames(empReq) %in% empReq.colnames]

  # Check for data types

  if (length(empReq) != length(empReq.colnames)) {
    stop(paste("Column names of Requirement must be:",
               paste(empReq.colnames, collapse = " ")))
  }

  if (class(empReq$activity) != "character")
    stop("Column activity in Requirement is not character!")

  if (class(empReq$personnelClass) != "character")
    stop("Column personnelClass in Requirement is not character!")

  if (class(empReq$quantity) != "numeric")
    stop("Column quantity in Requirement is not numeric!")

  if (!all(is.na(empReq$spareFactor)) & class(empReq$spareFactor) != "numeric")
    stop("Column spareFactor in Requirement is not numeric!")

  empReq$spareFactor <- as.numeric(empReq$spareFactor)

  if (class(empReq$equipment) != "character")
    stop("Column equipment in Requirement is not character!")

  if (class(empReq$OT) != "numeric")
    stop("Column OT in Requirement is not numeric!")

  if (class(empReq$costCode) != "character")
    stop("Column costCode in Requirement is not character!")

  empReq[, colnames(empReq) %in% c("quantity", "OT")] <- lapply(
    empReq[, colnames(empReq) %in% c("quantity", "OT")], FUN = as.integer
  )
  empReq <- as.data.frame(empReq)

  # Remove empReq rows with zero or NA quantities
  empReq <- empReq[!is.na(empReq$quantity),]
  empReq <- empReq[!empReq$quantity == 0,]

  sched <- readxl::read_xlsx(path = myFile,
                             sheet = "Schedule",
                             col_types = c("text",
                                           "numeric",
                                           "numeric",
                                           "numeric",
                                           "numeric",
                                           "numeric",
                                           "numeric",
                                           "numeric",
                                           "numeric",
                                           "numeric",
                                           "numeric",
                                           "numeric",
                                           "numeric"))

  sched[, !colnames(sched) %in% c("activity")] <- lapply(
    sched[, !colnames(sched) %in% c("activity")], FUN = as.integer
  )
  sched <- as.data.frame(sched)

  empPool.colnames <- c("ID",
                        "name",
                        "designation",
                        "personnelClass",
                        "attendance",
                        "equipment",
                        "costCode",
                        "status",
                        "cBegin",
                        "cEnd",
                        "inHouse",
                        "restday",
                        "isRF",
                        "d.rd",
                        "d.ho",
                        "d.rh")

  empPool <- readxl::read_xlsx(path = myFile,
                               sheet = "Pool")

  # sapply(empPool, class)

  empPool <- empPool[, colnames(empPool) %in% empPool.colnames]

  # Check for data types

  if (length(empPool) != length(empPool.colnames)) {
    stop(paste("Column names of Pool must be:",
               paste(empPool.colnames, collapse = " ")))
  }

  if (class(empPool$ID) != "character")
    stop("Column ID in Pool is not character!")

  if (class(empPool$name) != "character")
    stop("Column name in Pool is not character!")

  if (class(empPool$designation) != "character")
    stop("Column designation in Pool is not character!")

  if (class(empPool$personnelClass) != "character")
    stop("Column personnelClass in Pool is not character!")

  if (class(empPool$attendance) != "numeric")
    stop("Column attendance in Pool is not numeric!")

  if (!all(is.na(empPool$equipment)) & class(empPool$equipment) != "character")
    stop("Column equipment in Pool is not character!")

  if (!all(is.na(empPool$equipment)) & class(empPool$costCode) != "character")
    stop("Column costCode in Pool is not character!")

  if (class(empPool$status) != "character")
    stop("Column status in Pool is not character!")

  if (!"POSIXct" %in% class(empPool$cBegin))
    stop("Column cBegin in Pool is not date!")

  if (!"POSIXct" %in% class(empPool$cEnd))
    stop("Column cEnd in Pool is not date!")

  if (class(empPool$restday) != "character")
    stop("Column restday in Pool is not character!")

  empPool[, colnames(empPool) %in% c("inHouse", "isRF")] <- lapply(
    empPool[, colnames(empPool) %in% c("inHouse", "isRF")], FUN = as.logical
  )

  empPool[, colnames(empPool) %in% c("cBegin", "cEnd")] <- lapply(
    empPool[, colnames(empPool) %in% c("cBegin", "cEnd")], FUN = as.character
  )

  empPool <- as.data.frame(empPool)

  hol.colnames <- c("Month",
                    "Day",
                    "Type",
                    "Description")

  hol <- readxl::read_xlsx(path = myFile,
                           sheet = "hol")

  hol <- hol[, colnames(hol) %in% hol.colnames]

  # Check data types

  if (length(hol) != length(hol.colnames)) {
    stop(paste("Column names of hol must be:",
               paste(hol.colnames, collapse = " ")))
  }

  if (class(hol$Month) != "character")
    stop("Column Month in hol is not character!")

  if (class(hol$Day) != "numeric")
    stop("Column Day in hol is not numeric!")

  hol$Day <- as.integer(hol$Day)

  if (class(hol$Type) != "character")
    stop("Column Type in hol is not character!")

  if (class(hol$Description) != "character")
    stop("Column Description in hol is not character!")

  hol <- as.data.frame(hol)

  empReq$activity <- stringr::str_to_upper(empReq$activity)
  sched$activity <- stringr::str_to_upper(sched$activity)

  tempData <- getmhDB(empReq = empReq,
                      empPool = empPool,
                      sched = sched,
                      year = year,
                      hol = hol)

  mhDB <- tempData[[1]]
  listR <- tempData[[3]]

  wage.colnames <- c("ID", "S", "s")

  wage <- readxl::read_xlsx(path = myFile,
                            sheet = "Wage")

  wage <- wage[, colnames(wage) %in% wage.colnames]
  colnames(wage)[2] <- "s"

  if (class(wage$ID) != "character")
    stop("Column ID in wage is not character!")

  if (class(wage$s) != "numeric")
    wage$s <- as.numeric(wage$s)

  wage <- wage[wage$ID %in% empPool$ID,]

  wage$s[is.na(wage$s)] <- 0

  wage <- as.data.frame(wage)

  # SavePoint

  costDB <- getCost(mhDB = mhDB,
                    listR = listR,
                    wage = wage,
                    forecast = forecast)

  tempFolder <- getwd()
  dir.create(path = "./budget")
  setwd(paste(tempFolder, "/budget", sep = ""))

  for (i in costDB[[1]]) {

    xlsx::write.xlsx(x = i[[2]],
                     file = paste(i[[1]], ".xlsx", sep = ""),
                     row.names = FALSE)
  }

  xlsx::write.xlsx(x = costDB[[2]],
                   file = "manhours.xlsx",
                   row.names = FALSE)

  accr.13mp <- costDB[[3]]

  accr.13mp.R <- accr.13mp[accr.13mp$isReg,
                           !colnames(accr.13mp) %in% c("isReg")]

  accr.13mp.S <- accr.13mp[!accr.13mp$isReg,
                           !colnames(accr.13mp) %in% c("isReg")]

  accr.13mp.R <- as.data.frame(accr.13mp.R)
  accr.13mp.S <- as.data.frame(accr.13mp.S)

  xlsx::write.xlsx(x = accr.13mp.R,
                   file = "13mp-reg.xlsx",
                   row.names = FALSE)
  xlsx::write.xlsx(x = accr.13mp.S,
                   file = "13mp-sea.xlsx",
                   row.names = FALSE)

  xlsx::write.xlsx(x = costDB[[4]],
                   file = "bonus.xlsx",
                   row.names = FALSE)

  setwd(tempFolder)

  return(NULL)
}
