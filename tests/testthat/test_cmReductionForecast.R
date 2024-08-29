library(dplyr)
library(mansched)
library(readxl)
library(stringr)

set.seed(1)

xlsxFile <- system.file("exdata",
                        "cmReduction.xlsx",
                        package = "mansched")
empReq <- readxl::read_xlsx(path = xlsxFile, sheet = "Requirement")
sched <- readxl::read_xlsx(path = xlsxFile,
                           sheet = "Schedule",
                           col_types = c("text", rep("numeric", times = 12)))
empPool <- readxl::read_xlsx(path = xlsxFile, sheet = "Pool")
hol <- readxl::read_xlsx(path = xlsxFile, sheet = "hol")
year <- 2024

mCols <- as.character(3:12)

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

mhDB <- getmhDB(empReq = empReq,
                empPool = empPool,
                sched = sched,
                year = year,
                hol = hol,
                forecast = TRUE)

wage <- readxl::read_xlsx(path = xlsxFile, sheet = "Wage")

costRun <- getCost(mhDB = mhDB[[1]],
                   listR = listR,
                   wage = wage,
                   forecast = TRUE)

cost <- costRun[[1]]
row.names(cost) <- paste(cost$costCenter, cost$code)
cost <- cost[cost$code < 99999998, mCols]
cost <- sum(as.matrix(cost))

test_that("correct buget", {
  expect_equal(cost, 2442026.04)
})

rm(list = ls())
