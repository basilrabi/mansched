#' Compute budget
#'
#' Given a xlsx file containing the employee pool, requirements, wages, and
#'   work schedules, the budget is computed.
#'
#' @param myFile full path of the spreadsheet
#' @param year integer value defining the year to be budgeted
#' @return NULL
#' @export budget
#' @importFrom readxl read_xlsx
#' @importFrom xlsx write.xlsx
budget <- function(myFile, year) {

  # setwd("/home/brabi/sampleBudget/")
  # myFile <- "sampleData.xlsx"
  # year <- 2018

  empReq <- readxl::read_xlsx(path = myFile,
                              sheet = "Requirement",
                              col_types = c("text",
                                            "text",
                                            "numeric",
                                            "numeric",
                                            "text",
                                            "numeric",
                                            "text"))

  empReq[, colnames(empReq) %in% c("quantity", "OT")] <- lapply(
    empReq[, colnames(empReq) %in% c("quantity", "OT")], FUN = as.integer
  )
  empReq <- as.data.frame(empReq)

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

  empPool <- readxl::read_xlsx(path = myFile,
                               sheet = "Pool",
                               col_types = c("text",
                                             "text",
                                             "text",
                                             "text",
                                             "numeric",
                                             "text",
                                             "text",
                                             "text",
                                             "text",
                                             "text",
                                             "logical",
                                             "text",
                                             "logical"))
  empPool <- as.data.frame(empPool)

  hol <- readxl::read_xlsx(path = myFile,
                           sheet = "hol",
                           col_types = c("text",
                                         "numeric",
                                         "text",
                                         "text"))
  hol$Day <- as.integer(hol$Day)
  hol <- as.data.frame(hol)


  tempData <- getmhDB(empReq = empReq,
                      empPool = empPool,
                      sched = sched,
                      year = year,
                      hol = hol)

  mhDB <- tempData[[1]]
  listR <- tempData[[3]]

  wage <- readxl::read_xlsx(path = myFile,
                            sheet = "Wage",
                            col_types = c("text",
                                          "numeric"))
  wage <- as.data.frame(wage)

  export <- getCost(mhDB = mhDB, listR = listR, wage = wage)
  tempFolder <- getwd()
  dir.create(path = "./budget")
  setwd(paste(tempFolder, "/budget", sep = ""))

  for (i in export) {

    xlsx::write.xlsx(x = i[[2]],
                     file = paste(i[[1]], ".xlsx", sep = ""),
                     row.names = FALSE)
  }

  setwd(tempFolder)

  return(NULL)
}
