library(mansched)
library(readxl)

myFile <- system.file("exdata", "sampleData.xlsx", package = "mansched")

empReq <- readxl::read_xlsx(path  = myFile,
                            sheet = "Requirement")

sched <- readxl::read_xlsx(path      = myFile,
                           sheet     = "Schedule",
                           col_types = c("text",
                                         "numeric",
                                         "numeric",
                                         "numeric",
                                         "numeric",
                                         "numeric",
                                         "numeric",
                                         "numeric",
                                         "numeric",
                                         "numeric",
                                         "numeric",
                                         "numeric",
                                         "numeric"))

empPool <- readxl::read_xlsx(path  = myFile,
                             sheet = "Pool")

hol <- readxl::read_xlsx(path  = myFile,
                         sheet = "hol")

manReq <- initEmpReq(empReq = as.data.frame(empReq),
                     sched  = as.data.frame(sched),
                     hol    = as.data.frame(hol),
                     year   = 2018)[[1]]

test_that("initEmpReq() works", {
  expect_equal(length(manReq), 3)
})

rm(list = ls())
