#' Compute budget
#'
#' Given a spreadsheet containing the employee pool, requirements, wages, and
#'   work schedules, the budget is computed.
#'
#' @param myFile full path of the spreadsheet
#' @param year integer value defining the year to be budgeted
#' @return NULL
#' @export budget
#' @importFrom readODS read_ods write_ods
#' @importFrom readr col_character col_integer col_logical col_number cols
budget <- function(myFile, year) {

  # myFile <- "/home/brabi/sampleBudget/sampleData.ods"
  # year <- 2018

  empReq <- readODS::read_ods(
    path = myFile,
    sheet = 1,
    col_types = readr::cols(.default = col_character(),
                            quantity = col_integer(),
                            spareFactor = col_number(),
                            OT = col_integer()))

  sched <- readODS::read_ods(
    path = myFile,
    sheet = 2,
    col_types = readr::cols(.default = col_integer(),
                            activity = col_character()))

  empPool <- readODS::read_ods(
    path = myFile,
    sheet = 3,
    col_types = readr::cols(.default = col_character(),
                            attendance = col_number(),
                            inHouse = col_logical(),
                            isRF = col_logical()))

  hol <- readODS::read_ods(
    path = myFile,
    sheet = 4,
    col_types = readr::cols(Month = col_character(),
                            Day = col_integer(),
                            Type = col_character(),
                            Description = col_character()))

  tempData <- getmhDB(empReq = empReq,
                      empPool = empPool,
                      sched = sched,
                      year = year,
                      hol = hol)

  mhDB <- tempData[[1]]
  listR <- tempData[[3]]

  wage <- readODS::read_ods(
    path = myFile,
    sheet = 5,
    col_types = readr::cols(col_character(), col_number()))

  export <- getCost(mhDB = mhDB, listR = listR, wage = wage)
  tempFolder <- getwd()
  dir.create(path = "./budget")
  setwd(paste(tempFolder, "/budget", sep = ""))
  for (i in export) {
    readODS::write_ods(x = i[[2]],
                       path = paste(i[[1]], ".ods", sep = ""))
  }

  setwd(tempFolder)

  return(NULL)
}

#' Compute budget in windows
#'
#' Given a spreadsheet containing the employee pool, requirements, wages, and
#'   work schedules, the budget is computed.
#'
#' @param myFile full path of the spreadsheet
#' @param year integer value defining the year to be budgeted
#' @return NULL
#' @export budget2
#' @importFrom readxl read_xlsx
#' @importFrom xlsx write.xlsx
budget2 <- function(myFile, year) {

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

    # print(as.data.frame(i[[2]]))
  }

  setwd(tempFolder)

  return(NULL)
}
