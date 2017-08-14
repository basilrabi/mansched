library(mansched)
library(readODS)
library(readr)

myFile <- system.file("exdata", "sampleData.ods", package = "mansched")
empPool <- read_ods(path = myFile,
                    sheet = 3,
                    col_types = cols(.default = col_character(),
                                     attendance = col_integer(),
                                     inHouse = col_logical(),
                                     isRF = col_logical()))
hol <- read_ods(path = myFile,
                sheet = 4)

manPool <- initEmpPool(empPool = empPool,
                       hol = hol,
                       year = 2018)

test_that("initEmpPool() works", {
  expect_equal(length(manPool), 4)
})
