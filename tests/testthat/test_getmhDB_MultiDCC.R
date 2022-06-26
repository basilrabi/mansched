library(mansched)
library(readxl)
library(dplyr)

set.seed(1)

xlsxFile <- system.file("exdata",
                        "sampleDataMultiDCC.xlsx",
                        package = "mansched")
empReq <- readxl::read_xlsx(path = xlsxFile, sheet = "Requirement")
sched <- readxl::read_xlsx(path = xlsxFile,
                           sheet = "Schedule",
                           col_types = c("text", rep("numeric", times = 12)))
empPool <- readxl::read_xlsx(path = xlsxFile, sheet = "Pool")
hol <- readxl::read_xlsx(path = xlsxFile, sheet = "hol")
year <- 2018
forecast <- FALSE

mCols <- as.character(1:12)

empReq  <- as.data.frame(empReq)
empPool <- as.data.frame(empPool)
sched   <- as.data.frame(sched)
hol     <- as.data.frame(hol)

empPool[, c("cBegin", "cEnd")] <- lapply(empPool[, c("cBegin", "cEnd")],
                                         as.character)
empPool[, c("inHouse", "isRF", "field")] <-
  lapply(empPool[, c("inHouse", "isRF", "field")], as.logical)

listT <- initEmpReq(empReq = empReq, sched = sched, hol = hol, year = year)[[1]]
listR <- initEmpPool(empPool = empPool, hol = hol, year = year)[[1]]

tempData <- getmhDB(empReq   = empReq,
                    empPool  = empPool,
                    sched    = sched,
                    year     = year,
                    hol      = hol,
                    forecast = forecast)

mhDBJan <- dplyr::filter(tempData[[1]], month == 1L)
mhDBFeb <- dplyr::filter(tempData[[1]], month == 2L)

test_that("getLaborDayShirt() works", {
  expect_equal(mhDBJan$costCenter, "0-0")
  expect_equal(mhDBFeb$costCenter, c("14000", "AA"))
  expect_equal(mhDBJan$mh, 208L)
  expect_equal(mhDBFeb$mh, c(92L, 92L))
})

rm(list = ls())
