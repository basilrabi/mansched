#' Compute budget
#'
#' Given a xlsx file containing the employee pool, requirements, wages, and
#'   work schedules, the budget is computed.
#'
#' @param xlsxFile full path of the spreadsheet
#' @param year integer value defining the year to be budgeted
#' @param forecast logical value \cr
#'   Compute cost for forecast?
#' @return NULL
#' @export budget
#' @importFrom readxl read_xlsx
#' @importFrom xlsx write.xlsx
#' @importFrom dplyr left_join group_by summarise "%>%"
#' @importFrom tidyr spread
budget <- function(xlsxFile, year, forecast = FALSE) {

  # Define global variables
  costCode       <- NULL
  equipment      <- NULL
  ID             <- NULL
  manhours       <- NULL
  mh             <- NULL
  mhType         <- NULL
  personnelClass <- NULL
  status         <- NULL

  empReq <- readxl::read_xlsx(path = xlsxFile, sheet = "Requirement")
  empReq <- sanityCheckEmpReq(empReq)

  sched <- readxl::read_xlsx(path = xlsxFile,
                             sheet = "Schedule",
                             col_types = c("text", rep("numeric", times = 12)))
  sched[, !colnames(sched) %in% c("activity")] <-
    lapply(sched[, !colnames(sched) %in% c("activity")], FUN = as.integer)
  sched <- as.data.frame(sched)

  empPool <- readxl::read_xlsx(path = xlsxFile, sheet = "Pool")
  empPool <- sanityCheckEmpPool(empPool)

  hol <- readxl::read_xlsx(path = xlsxFile, sheet = "hol")
  hol <- sanityCheckHol(hol)

  empReq$activity <- toupper(empReq$activity)
  sched$activity <- toupper(sched$activity)

  tempData <- getmhDB(empReq   = empReq,
                      empPool  = empPool,
                      sched    = sched,
                      year     = year,
                      hol      = hol,
                      forecast = forecast)

  mhDB   <- tempData[[1]]
  listR  <- tempData[[3]]
  mhReq  <- tempData[[6]]
  mhPool <- tempData[[7]]

  wage <- readxl::read_xlsx(path = xlsxFile, sheet = "Wage")
  wage <- sanityCheckWage(wage, empPool)

  costDB <- getCost(mhDB     = mhDB,
                    listR    = listR,
                    wage     = wage,
                    forecast = forecast)

  tempFolder <- getwd()

  if (forecast) {
    dir.create(path = "./forecast")
    setwd(paste(tempFolder, "/forecast", sep = ""))
    postFix <- "Forecast.xlsx"
  } else {
    dir.create(path = "./budget")
    setwd(paste(tempFolder, "/budget", sep = ""))
    postFix <- "Budget.xlsx"
  }

  xlsx::write.xlsx(costDB[[1]],
                   file = paste0("personnelCost", postFix),
                   row.names = FALSE)

  manHours <- mhDB %>%
    dplyr::group_by(status, costCode, mhType, month) %>%
    dplyr::summarise(manhours = sum(mh)) %>%
    tidyr::spread(month, manhours, fill = 0) %>%
    as.data.frame(row.names = NULL)
  xlsx::write.xlsx(manHours,
                   file = paste0("manhours", postFix),
                   row.names = FALSE)

  accr.13mp   <- costDB[[3]]
  accr.13mp.R <- accr.13mp[accr.13mp$status == "reg",
                           !colnames(accr.13mp) %in% c("status")]
  accr.13mp.P <- accr.13mp[accr.13mp$status == "pro",
                           !colnames(accr.13mp) %in% c("status")]
  accr.13mp.S <- accr.13mp[accr.13mp$status == "sea",
                           !colnames(accr.13mp) %in% c("status")]

  if (nrow(accr.13mp.R) > 0) {
    xlsx::write.xlsx(x = as.data.frame(accr.13mp.R),
                     file = paste0("13mp-reg", postFix),
                     row.names = FALSE)
  }

  if (nrow(accr.13mp.P) > 0) {
    xlsx::write.xlsx(x = as.data.frame(accr.13mp.P),
                     file = paste0("13mp-pro", postFix),
                     row.names = FALSE)
  }

  if (nrow(accr.13mp.S) > 0) {
    xlsx::write.xlsx(x = as.data.frame(accr.13mp.S),
                     file = paste0("13mp-sea", postFix),
                     row.names = FALSE)
  }

  if (nrow(costDB[[4]]) > 0) {
    xlsx::write.xlsx(x = as.data.frame(costDB[[4]]),
                     file = "bonus.xlsx",
                     row.names = FALSE)
  }

  if (nrow(costDB[[5]]) > 0) {
    xlsx::write.xlsx(x = as.data.frame(costDB[[5]]),
                     file = "seasonalCosts.xlsx",
                     row.names = FALSE)
  }

  if (!is.null(mhPool)) {

    mhPool <- dplyr::left_join(
      x = mhPool,
      y = empPool[, c("ID", "personnelClass", "equipment"),],
      by = "ID"
    )

    mhPool <- mhPool %>%
      dplyr::group_by(ID, month, personnelClass, equipment) %>%
      dplyr::summarise(mh = sum(mh))

    mhPool <- mhPool %>% tidyr::spread(month, mh, fill = 0)

    xlsx::write.xlsx(x = as.data.frame(mhPool),
                     file = paste0("pool", postFix),
                     row.names = FALSE)
  }

  if (!is.null(mhReq)) {
    mhReq <- mhReq %>% tidyr::spread(month, mh, fill = 0)
    xlsx::write.xlsx(x = as.data.frame(mhReq),
                     file = paste0("req", postFix),
                     row.names = FALSE)
  }

  setwd(tempFolder)
  invisible(NULL)
}
