library(data.table)
library(dplyr)
library(mansched)
library(readxl)

set.seed(1)

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

mhInitR <- lapply(listR, function(x) {
  data.frame(ID = x@ID, mh_initial = sum(getHours(x)), stringsAsFactors = FALSE)
}) %>% data.table::rbindlist() %>%
  dplyr::arrange(ID)
totRi <- sum(mhInitR$mh_initial)

mhInitT <- lapply(listT, function(x) {
  data.frame(ID = x@ID, mh_initial = sum(getHours(x)), stringsAsFactors = FALSE)
}) %>% data.table::rbindlist() %>%
  dplyr::arrange(ID)
totTi <- sum(mhInitT$mh_initial)

tempData <- getmhDB(empReq   = empReq,
                    empPool  = empPool,
                    sched    = sched,
                    year     = year,
                    hol      = hol,
                    forecast = forecast)

mhFinR <- data.table::rbindlist(list(tempData[[1]], tempData[[8]])) %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(mh_final = sum(mh)) %>%
  dplyr::arrange(ID)
totRf <- sum(tempData[[8]]$mh)

mhFinT <- dplyr::select(tempData[[6]], ID, mh) %>%
  dplyr::bind_rows(dplyr::select(tempData[[1]], ID = reqID, mh)) %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(mh_final = sum(mh)) %>%
  dplyr::arrange(ID)
totTf <- sum(tempData[[6]]$mh)

test_that("getmhDB() works", {
  expect_equal(totTi + totRi,
               sum(tempData[[1]]$mh) * 2 + totTf + totRf)
  expect_equal(sum(tempData[[1]]$mh),
               totRi - totRf)
  expect_equal(sum(tempData[[1]]$mh),
               totTi - totTf)
  expect_equal(mhInitR$mh_initial, mhFinR$mh_final)
  expect_equal(mhInitT$mh_initial, mhFinT$mh_final)
})

wage <- readxl::read_xlsx(path = xlsxFile, sheet = "Wage")

tempData <- getCost(mhDB = tempData[[1]],
                    listR = listR,
                    wage = wage,
                    forecast = forecast)

c14000 <- tempData[[1]][tempData[[1]]$costCenter == "14000",]
c14100 <- tempData[[1]][tempData[[1]]$costCenter == "14100",]

mh14000 <- sum(c14000[c14000$row == "man-hours", c(as.character(1:12))])
mh14100 <- sum(c14100[c14100$row == "man-hours", c(as.character(1:12))])
totMH <- sum(tempData[[2]][,c(as.character(1:12))])

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
