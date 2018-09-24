library(mansched)
library(readxl)

xlsxFile <- system.file("exdata", "sampleData.xlsx", package = "mansched")
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
totTi <- getHoursL(listT)
totRi <- getHoursL(listR)

tempData <- getmhDB(empReq   = empReq,
                    empPool  = empPool,
                    sched    = sched,
                    year     = year,
                    hol      = hol,
                    cores    = 2,
                    forecast = forecast)

totTf <- getHoursL(tempData[[4]])
totRf <- getHoursL(tempData[[5]])

test_that("getmhDB() works", {
  expect_equal(totTi + totRi,
               sum(tempData[[1]]$mh)*2 + totTf + totRf)
  expect_equal(sum(tempData[[1]]$mh),
               totRi - totRf)
  expect_equal(sum(tempData[[1]]$mh),
               totTi - totTf)
})

wage <- readxl::read_xlsx(path = xlsxFile, sheet = "Wage")

tempData <- getCost(mhDB = tempData[[1]],
                    listR = listR,
                    wage = wage,
                    forecast = forecast)

c14000 <- tempData[[1]][tempData[[1]]$costCode == "14000",]
c14100 <- tempData[[1]][tempData[[1]]$costCode == "14100",]

mh14000 <- sum(c14000[c14000$row == "man-hours", c(as.character(1:12))])
mh14100 <- sum(c14100[c14100$row == "man-hours", c(as.character(1:12))])
totMH   <- sum(tempData[[2]][,c(as.character(1:12))])

PI14000 <- sum(c14000[c14000$row == "Prem-HDMF (Pag-ibig)",
                      c(as.character(1:12))])
PI14100 <- sum(c14100[c14100$row == "Prem-HDMF (Pag-ibig)",
                      c(as.character(1:12))])

riceSub <- tempData[[1]]
riceSub <- riceSub[riceSub$code == 521011L,]
riceSub <- riceSub[, as.character(1:12)] %>% as.matrix()

test_that("getCost() works", {
  expect_equal(mh14000 + mh14100, totMH)
  expect_equal(PI14000 + PI14100, 500 * 12)
  expect_equal(sum(riceSub), 5 * 2500 * 12)
})

rm(list = ls())
