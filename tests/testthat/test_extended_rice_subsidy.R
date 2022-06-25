library(data.table)
library(dplyr)
library(mansched)
library(readxl)
library(stringr)

set.seed(1)

xlsxFile <- system.file(
  "exdata", "riceSubsidySeasonal.xlsx", package = "mansched"
)
empReq <- readxl::read_xlsx(path = xlsxFile, sheet = "Requirement")
sched <- readxl::read_xlsx(path = xlsxFile,
                           sheet = "Schedule",
                           col_types = c("text", rep("numeric", times = 12)))
empPool <- readxl::read_xlsx(path = xlsxFile, sheet = "Pool")
hol <- readxl::read_xlsx(path = xlsxFile, sheet = "hol")
year <- 2018
forecast <- FALSE

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

wage <- readxl::read_xlsx(path = xlsxFile, sheet = "Wage")

tempData <- getCost(mhDB = tempData[[1]],
                    listR = listR,
                    wage = wage,
                    forecast = forecast)

cost <- tempData[[1]] %>%
  dplyr::filter(code == 60300015L)


test_that("off-season rice subsidy", {
  expect_equal(abs(sum(cost$`1`) - 2500) < 0.1, TRUE)
})

rm(list = ls())
