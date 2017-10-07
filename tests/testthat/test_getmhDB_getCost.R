library(mansched)
library(readODS)
library(readr)

myFile <- system.file("exdata", "sampleData.ods", package = "mansched")

empReq <- read_ods(path      = myFile,
                   sheet     = 1,
                   col_types = cols(.default    = col_character(),
                                    quantity    = col_integer(),
                                    spareFactor = col_number(),
                                    OT          = col_integer()))

sched <- read_ods(path      = myFile,
                  sheet     = 2,
                  col_types = cols(.default = col_integer(),
                                   activity = col_character()))

empPool <- read_ods(path      = myFile,
                    sheet     = 3,
                    col_types = cols(.default   = col_character(),
                                     attendance = col_integer(),
                                     inHouse    = col_logical(),
                                     isRF       = col_logical()))
hol <- read_ods(path  = myFile,
                sheet = 4)

year <- 2018

listT <- initEmpReq(empReq = empReq,
                    sched  = sched,
                    hol    = hol,
                    year   = year)[[1]]

listR <- initEmpPool(empPool = empPool,
                     hol     = hol,
                     year    = year)[[1]]

totTi <- getHoursL(listT)
totRi <- getHoursL(listR)

tempData <- getmhDB(empReq  = empReq,
                    empPool = empPool,
                    sched   = sched,
                    year    = year,
                    hol     = mansched::holidays,
                    cores   = 2)

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

wage <- read_ods(path      = myFile,
                 sheet     = 5,
                 col_types = cols(ID = col_character(),
                                  s  = col_number()))

tempData <- getCost(mhDB  = tempData[[1]],
                    listR = listR,
                    wage  = wage)

c14000 <- tempData[[1]][[1]][[2]]
c14100 <- tempData[[1]][[2]][[2]]

mh14000 <- sum(c14000[c14000$row == "man-hours", c(as.character(1:12))])
mh14100 <- sum(c14100[c14100$row == "man-hours", c(as.character(1:12))])
totMH   <- sum(tempData[[2]][,c(as.character(1:12))])

PI14000 <- sum(c14000[c14000$row == "Prem-HDMF (Pag-ibig)",
                      c(as.character(1:12))])
PI14100 <- sum(c14100[c14100$row == "Prem-HDMF (Pag-ibig)",
                      c(as.character(1:12))])

test_that("getCost() works", {
  expect_equal(mh14000 + mh14100, totMH)
  expect_equal(PI14000 + PI14100, 500 * 12)
})

