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
hol   <- read_ods(path  = myFile,
                  sheet = 4)

sched <- read_ods(path      = myFile,
                  sheet     = 2,
                  col_types = cols(.default = col_integer(),
                                   activity = col_character()))


manReq <- initEmpReq(empReq = empReq,
                     sched  = sched,
                     hol    = hol,
                     year   = 2018)[[1]]

test_that("initEmpReq() works", {
  expect_equal(length(manReq), 3)
})
