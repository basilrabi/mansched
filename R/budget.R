#' Compute budget
#'
#' Given a spreadsheet containing the employee pool, requirements, wages, and
#'   work schedules, the budget is computed.
#'
#' @param myFile full path of the spreadsheet
#' @param year integer value defining the year to be budgeted
#' @return NULL
#' @export budget
#' @importFrom readODS read_ods
#' @importFrom readr col_character col_integer col_logical col_number cols
budget <- function(myFile, year) {
  # library(mansched)
  # library(readODS)
  # library(readr)
  # library(magrittr)

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
                            attendance = col_integer(),
                            inHouse = col_logical(),
                            isRF = col_logical()))

  hol <- readODS::read_ods(path = myFile,
                           sheet = 4)

  tempData <- getmhDB(empReq = empReq,
                      empPool = empPool,
                      sched = sched,
                      year = year,
                      hol = mansched::holidays)

  listR <- initEmpPool(empPool = empPool, hol = hol, year)
  listT <- initEmpReq(empReq = empReq, sched = sched, hol = hol, year = year)

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
