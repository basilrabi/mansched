library(mansched)
library(readxl)
library(dplyr)

set.seed(1)

xlsxFile <- system.file("exdata",
                        "sampleDataPHIC.xlsx",
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

empReq <- as.data.frame(empReq)
empPool <- as.data.frame(empPool)
sched <- as.data.frame(sched)
hol <- as.data.frame(hol)

empPool[, c("cBegin", "cEnd")] <- lapply(empPool[, c("cBegin", "cEnd")],
                                         as.character)
empPool[, c("inHouse", "isRF", "field")] <-
  lapply(empPool[, c("inHouse", "isRF", "field")], as.logical)

listT <- initEmpReq(empReq = empReq, sched = sched, hol = hol, year = year)[[1]]
listR <- initEmpPool(empPool = empPool, hol = hol, year = year)[[1]]

tempData <- getmhDB(empReq = empReq,
                    empPool = empPool,
                    sched = sched,
                    year = year,
                    hol = hol,
                    forecast = forecast)

wage <- readxl::read_xlsx(path = xlsxFile, sheet = "Wage")

tempData <- getCost(mhDB = tempData[[1]],
                    listR = listR,
                    wage = wage,
                    forecast = forecast)

cost <- tempData[[1]]
cost <- cost[cost$code %in% c(521001L, 521002L, 521008L),
             c("costCenter", "row", mCols)]

test_that("Correct PHIC in Budget", {
  expect_equal(437.5, cost$`1`[1])
  expect_equal(437.5, cost$`7`[1])
  expect_equal(cost$`1`[3], 437.5)
  expect_equal(cost$`7`[3], 437.5)
  expect_equal(cost$`1`[5], 125)
  expect_equal(cost$`1`[9], 100)
})


forecast <- TRUE

mCols <- as.character(1:12)

empReq <- as.data.frame(empReq)
empPool <- as.data.frame(empPool)
sched <- as.data.frame(sched)
hol <- as.data.frame(hol)

empPool[, c("cBegin", "cEnd")] <- lapply(empPool[, c("cBegin", "cEnd")],
                                         as.character)
empPool[, c("inHouse", "isRF", "field")] <-
  lapply(empPool[, c("inHouse", "isRF", "field")], as.logical)

listT <- initEmpReq(empReq = empReq, sched = sched, hol = hol, year = year)[[1]]
listR <- initEmpPool(empPool = empPool, hol = hol, year = year)[[1]]

tempData <- getmhDB(empReq = empReq,
                    empPool = empPool,
                    sched = sched,
                    year = year,
                    hol = hol,
                    forecast = forecast)

wage <- readxl::read_xlsx(path = xlsxFile, sheet = "Wage")

tempData <- getCost(mhDB = tempData[[1]],
                    listR = listR,
                    wage = wage,
                    forecast = forecast)

cost <- tempData[[1]]
cost <- cost[cost$code %in% c(521001L, 521002L, 521008L),
             c("costCenter", "row", mCols)]

test_that("Correct PHIC in Budget", {
  expect_equal(cost$`1`[1], 437.5)
  expect_equal(cost$`7`[1], 437.5)
  expect_equal(cost$`1`[3], 437.5)
  expect_equal(cost$`7`[3], 437.5)
  expect_equal(cost$`1`[5], 125)
  expect_equal(cost$`1`[9], 100)
})

rm(list = ls())
