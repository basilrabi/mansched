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
#' @importFrom dplyr left_join group_by summarise
#' @importFrom magrittr "%>%"
#' @importFrom tidyr spread
budget <- function(myFile, year, forecast = FALSE) {

  # Define global variables
  equipment      <- NULL
  ID             <- NULL
  mh             <- NULL
  personnelClass <- NULL

  empReq.colnames <- c("activity",
                       "personnelClass",
                       "quantity",
                       "spareFactor",
                       "equipment",
                       "OT",
                       "costCode")

  empReq <- readxl::read_xlsx(path  = myFile,
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

  sched <- readxl::read_xlsx(path      = myFile,
                             sheet     = "Schedule",
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
                        "d.rh",
                        "dcc")

  empPool <- readxl::read_xlsx(path  = myFile,
                               sheet = "Pool")

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

  if (!all(is.na(empPool$dcc)) & class(empPool$dcc) != "character")
    stop("Column dcc in Pool is not character!")

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

  hol <- readxl::read_xlsx(path  = myFile,
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

  empReq$activity <- toupper(empReq$activity)
  sched$activity  <- toupper(sched$activity)

  tempData <- getmhDB(empReq  = empReq,
                      empPool = empPool,
                      sched   = sched,
                      year    = year,
                      hol     = hol)

  mhDB   <- tempData[[1]]
  listR  <- tempData[[3]]
  mhReq  <- tempData[[6]]
  mhPool <- tempData[[7]]

  wage.colnames <- c("ID", "S", "s")

  wage <- readxl::read_xlsx(path  = myFile,
                            sheet = "Wage")

  wage              <- wage[, colnames(wage) %in% wage.colnames]
  colnames(wage)[2] <- "s"

  if (class(wage$ID) != "character")
    stop("Column ID in wage is not character!")

  if (class(wage$s) != "numeric")
    wage$s <- as.numeric(wage$s)

  wage                  <- wage[wage$ID %in% empPool$ID,]
  wage$s[is.na(wage$s)] <- 0
  wage                  <- as.data.frame(wage)

  costDB <- getCost(mhDB     = mhDB,
                    listR    = listR,
                    wage     = wage,
                    forecast = forecast)

  tempFolder <- getwd()

  if (forecast) {

    dir.create(path = "./forecast")
    setwd(paste(tempFolder, "/forecast", sep = ""))

  } else {

    dir.create(path = "./budget")
    setwd(paste(tempFolder, "/budget", sep = ""))

  }

  for (i in costDB[[1]]) {
    xlsx::write.xlsx(x         = i[[2]],
                     file      = "personnelCost.xlsx",
                     row.names = FALSE,
                     sheetName = paste(i[[1]]),
                     append    = TRUE)
  }

  xlsx::write.xlsx(x         = costDB[[2]],
                   file      = "manhours.xlsx",
                   row.names = FALSE)

  accr.13mp   <- costDB[[3]]
  accr.13mp.R <- accr.13mp[accr.13mp$status == "reg",
                           !colnames(accr.13mp) %in% c("status")]
  accr.13mp.P <- accr.13mp[accr.13mp$status == "pro",
                           !colnames(accr.13mp) %in% c("status")]
  accr.13mp.S <- accr.13mp[accr.13mp$status == "sea",
                           !colnames(accr.13mp) %in% c("status")]

  if (nrow(accr.13mp.R) > 0) {
    xlsx::write.xlsx(x         = as.data.frame(accr.13mp.R),
                     file      = "13mp-reg.xlsx",
                     row.names = FALSE)
  }

  if (nrow(accr.13mp.P) > 0) {
    xlsx::write.xlsx(x         = as.data.frame(accr.13mp.P),
                     file      = "13mp-pro.xlsx",
                     row.names = FALSE)
  }

  if (nrow(accr.13mp.S) > 0) {
    xlsx::write.xlsx(x         = as.data.frame(accr.13mp.S),
                     file      = "13mp-sea.xlsx",
                     row.names = FALSE)
  }

  if (nrow(costDB[[4]]) > 0) {
    xlsx::write.xlsx(x         = as.data.frame(costDB[[4]]),
                     file      = "bonus.xlsx",
                     row.names = FALSE)
  }

  if (!is.null(mhPool)) {

    mhPool <- dplyr::left_join(
      x  = mhPool,
      y  = empPool[, colnames(empPool) %in% c("ID",
                                              "personnelClass",
                                              "equipment"),],
      by = "ID"
    )

    mhPool <- mhPool %>%
      dplyr::group_by(ID, month, personnelClass, equipment) %>%
      dplyr::summarise(mh = sum(mh))

    mhPool <- mhPool %>%  tidyr::spread(month, mh, fill = 0)

    xlsx::write.xlsx(x         = as.data.frame(mhPool),
                     file      = "pool.xlsx",
                     row.names = FALSE)
  }

  if (!is.null(mhReq)) {

    mhReq <- mhReq %>% tidyr::spread(month, mh, fill = 0)

    xlsx::write.xlsx(x         = as.data.frame(mhReq),
                     file      = "req.xlsx",
                     row.names = FALSE)
  }

  setwd(tempFolder)

  invisible(NULL)
}
