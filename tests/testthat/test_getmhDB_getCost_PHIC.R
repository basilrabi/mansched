library(dplyr)
library(mansched)
library(readxl)
library(stringr)

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
cost <- cost[stringr::str_detect(cost$description, "PHIC"),
             c("costCenter", "description", mCols)]

test_that("Correct PHIC in Budget", {
  expect_equal(cost$`1`[1], 1100)
  expect_equal(cost$`7`[2], 1600)
  expect_equal(cost$`1`[3], round(400 * 313 * 0.02 / 12, 2))
  expect_equal(cost$`1`[4], round(400 * 313 * 0.02 / 12, 2))
  expect_equal(cost$`1`[5], 200)
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
cost <- cost[stringr::str_detect(cost$description, "PHIC"),
             c("costCenter", "description", mCols)]

test_that("Correct PHIC in Forecast", {
  expect_equal(cost$`1`[1], 875)
  expect_equal(cost$`7`[1], 875)
  expect_equal(cost$`1`[2], 1225)
  expect_equal(cost$`7`[2], 1225)
  expect_equal(cost$`1`[3], 182.58)
  expect_equal(cost$`1`[5], 175)
})

rm(list = ls())
