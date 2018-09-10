library(mansched)

xlsxFile <- system.file("exdata", "sampleData.xlsx", package = "mansched")

empPool <- readxl::read_xlsx(path  = xlsxFile,
                             sheet = "Pool")

empPool[, c("cBegin", "cEnd")] <- lapply(empPool[, c("cBegin", "cEnd")],
                                         as.character)

empPool[, c("inHouse", "isRF")] <- lapply(empPool[, c("inHouse", "isRF")],
                                          as.logical)

hol <- readxl::read_xlsx(path  = xlsxFile,
                         sheet = "hol")

empPool <- as.data.frame(empPool)
hol     <- as.data.frame(hol)

manPool <- initEmpPool(empPool = empPool,
                       hol = hol,
                       year = 2018)[[1]]

test_that("initEmpPool() works", {
  expect_equal(length(manPool), 5)
})

rm(list = ls())
