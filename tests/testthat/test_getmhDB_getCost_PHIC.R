library(mansched)
library(readxl)
library(dplyr)

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
                    cores    = 2,
                    forecast = forecast)

wage <- readxl::read_xlsx(path = xlsxFile, sheet = "Wage")

tempData <- getCost(mhDB = tempData[[1]],
                    listR = listR,
                    wage = wage,
                    forecast = forecast)

cost <- tempData[[1]]
cost <- cost[cost$code %in% c(521001L, 521002L, 521008L),
             c("costCode", "row", mCols)]

test_that("Correct PHIC in Budget", {
  expect_equal(cost$`1`[2] * 0.01375, cost$`1`[1])
  expect_equal(cost$`7`[2] * 0.01375, cost$`7`[1])
  expect_equal(cost$`1`[3], 1375)
  expect_equal(cost$`7`[3], 1375)
  expect_equal(cost$`1`[5],
               round(round(400 * (313 / 12), digits = 2) * 0.01375, digits = 2))
  expect_equal(cost$`1`[9], 137.5)
})


forecast <- TRUE

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
                    cores    = 2,
                    forecast = forecast)

wage <- readxl::read_xlsx(path = xlsxFile, sheet = "Wage")

tempData <- getCost(mhDB = tempData[[1]],
                    listR = listR,
                    wage = wage,
                    forecast = forecast)

cost <- tempData[[1]]
cost <- cost[cost$code %in% c(521001L, 521002L, 521008L),
             c("costCode", "row", mCols)]

test_that("Correct PHIC in Budget", {
  expect_equal(cost$`1`[1], 550)
  expect_equal(cost$`7`[1], 550)
  expect_equal(cost$`1`[3], 550)
  expect_equal(cost$`7`[3], 550)
  expect_equal(cost$`1`[5],
               round(round(400 * (313 / 12), digits = 2) * 0.01375, digits = 2))
  expect_equal(cost$`1`[9], 137.5)
})

rm(list = ls())