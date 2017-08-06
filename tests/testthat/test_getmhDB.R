library(mansched2)
library(readODS)
library(readr)

myFile <- system.file("exdata", "sampleData.ods", package = "mansched2")

empReq <- read_ods(path = myFile,
                   sheet = 1,
                   col_types = cols(.default = col_character(),
                                    quantity = col_integer(),
                                    spareFactor = col_number(),
                                    OT = col_integer()))
sched <- read_ods(path = myFile,
                  sheet = 2,
                  col_types = cols(.default = col_integer(),
                                   activity = col_character()))
empPool <- read_ods(path = myFile,
                    sheet = 3,
                    col_types = cols(.default = col_character(),
                                     attendance = col_integer(),
                                     inHouse = col_logical(),
                                     isRF = col_logical()))
hol <- read_ods(path = myFile,
                sheet = 4)
year <- 2018

listT <- initEmpReq(empReq = empReq, sched = sched, hol = hol, year = year)
listR <- initEmpPool(empPool = empPool, hol = hol, year = year)

totTi <- getHoursL(listT)
totRi <- getHoursL(listR)

tempData <- getmhDB(empReq = empReq,
                    empPool = empPool,
                    sched = sched,
                    year = year,
                    hol = mansched2::holidays)

totTf <- getHoursL(tempData[[2]])
totRf <- getHoursL(tempData[[3]])

test_that("getmhDB() works", {
  expect_equal(totTi + totRi,
               sum(tempData[[1]]$mh)*2 + totTf + totRf)
  expect_equal(sum(tempData[[1]]$mh),
               totRi - totRf)
  expect_equal(sum(tempData[[1]]$mh),
               totTi - totTf)
})
