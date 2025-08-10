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

mhDBBudget <- getmhDB(empReq = empReq,
                      empPool = empPool,
                      sched = sched,
                      year = year,
                      hol = hol,
                      forecast = FALSE)

wage <- readxl::read_xlsx(path = xlsxFile, sheet = "Wage")

costRunBudget <- getCost(mhDB = mhDBBudget[[1]],
                         listR = listR,
                         wage = wage,
                         forecast = FALSE)

costBudget <- costRunBudget[[1]]
row.names(costBudget) <- paste(costBudget$costCenter, costBudget$code)
costBudget <- costBudget[costBudget$code < 99999998, mCols]
cost <- sum(as.matrix(costBudget))

test_that("correct buget", {
  expect_equal(cost, 2586611.11)
})

rm(list = ls())
