library(mansched)
library(readxl)
library(dplyr)

set.seed(1)

# Regular white hat ------------------------------------------------------------

xlsxFile <- system.file("exdata",
                        "sampleDataRegWhite.xlsx",
                        package = "mansched")
empReq <- readxl::read_xlsx(path = xlsxFile, sheet = "Requirement")
sched <- readxl::read_xlsx(path = xlsxFile,
                           sheet = "Schedule",
                           col_types = c("text", rep("numeric", times = 12)))
empPool <- readxl::read_xlsx(path = xlsxFile, sheet = "Pool")
hol <- readxl::read_xlsx(path = xlsxFile, sheet = "hol")
year <- 2018
forecast <- FALSE

mCols <- as.character(1:12)

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

tempData <- getmhDB(empReq   = empReq,
                    empPool  = empPool,
                    sched    = sched,
                    year     = year,
                    hol      = hol,
                    forecast = forecast)

wage <- readxl::read_xlsx(path = xlsxFile, sheet = "Wage")

tempData <- getCost(mhDB = tempData[[1]],
                    listR = listR,
                    wage = wage,
                    forecast = forecast)

mp13 <- tempData[[1]]
mp13 <- mp13[mp13$code == 521009L, mCols] %>% as.matrix() %>% as.vector()

sal <- c(wage$s[1], wage$i[1])

# Compute 13th  month pay
testMP13 <- rep(round(sal[2] / 12, digits = 2), times = 12)
# Add mid-year bonus
testMP13[5] <- testMP13[5] + sal[2]
# Add year-end bonus
testMP13[12] <- testMP13[12] + sal[2] * 2

test_that("Correct 13th MP for Reg White", {
  expect_equal(mp13, testMP13)
})

rm(list = ls())

# Regular non-white hat --------------------------------------------------------

xlsxFile <- system.file("exdata",
                        "sampleDataRegNonWhite.xlsx",
                        package = "mansched")
empReq <- readxl::read_xlsx(path = xlsxFile, sheet = "Requirement")
sched <- readxl::read_xlsx(path = xlsxFile,
                           sheet = "Schedule",
                           col_types = c("text", rep("numeric", times = 12)))
empPool <- readxl::read_xlsx(path = xlsxFile, sheet = "Pool")
hol <- readxl::read_xlsx(path = xlsxFile, sheet = "hol")
year <- 2018
forecast <- FALSE

mCols <- as.character(1:12)

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

tempData <- getmhDB(empReq   = empReq,
                    empPool  = empPool,
                    sched    = sched,
                    year     = year,
                    hol      = hol,
                    forecast = forecast)

wage <- readxl::read_xlsx(path = xlsxFile, sheet = "Wage")

tempData <- getCost(mhDB = tempData[[1]],
                    listR = listR,
                    wage = wage,
                    forecast = forecast)

mp13 <- tempData[[1]]
mp13 <- mp13[mp13$code == 521009L, mCols] %>% as.matrix() %>% as.vector()

sal <- c(wage$s[1], wage$i[1])

# Compute 13th  month pay
testMP13 <- rep(round(sal[2] * 26 / 12, digits = 2), times = 12)
# Add mid-year bonus
testMP13[5] <- testMP13[5] + (sal[2] * 26)
# Add year-end bonus
testMP13[12] <- testMP13[12] + (sal[2] * 26 * 2)
# Add signing bonus
testMP13[5] <- testMP13[5] + 13000

test_that("Correct 13th MP for Reg non-white", {
  expect_equal(mp13, testMP13)
})

rm(list = ls())

# Seasonal white hat -----------------------------------------------------------

xlsxFile <- system.file("exdata",
                        "sampleDataSeaWhite.xlsx",
                        package = "mansched")
empReq <- readxl::read_xlsx(path = xlsxFile, sheet = "Requirement")
sched <- readxl::read_xlsx(path = xlsxFile,
                           sheet = "Schedule",
                           col_types = c("text", rep("numeric", times = 12)))
empPool <- readxl::read_xlsx(path = xlsxFile, sheet = "Pool")
hol <- readxl::read_xlsx(path = xlsxFile, sheet = "hol")
year <- 2018
forecast <- FALSE

mCols <- as.character(1:12)

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

tempData <- getmhDB(empReq   = empReq,
                    empPool  = empPool,
                    sched    = sched,
                    year     = year,
                    hol      = hol,
                    forecast = forecast)

wage <- readxl::read_xlsx(path = xlsxFile, sheet = "Wage")

tempData <- getCost(mhDB = tempData[[1]],
                    listR = listR,
                    wage = wage,
                    forecast = forecast)

mp13 <- tempData[[1]]
mp13 <- mp13[mp13$code == 521009L, mCols] %>% as.matrix() %>% as.vector()

sal <- c(wage$s[1], wage$i[1])

# Compute 13th  month pay
testMP13 <- rep(round(sal[1] / 12, digits = 2), times = 12)

test_that("Correct 13th MP for Sea White", {
  expect_equal(mp13, testMP13)
})

rm(list = ls())

# Seasonal non-white hat -------------------------------------------------------

xlsxFile <- system.file("exdata",
                        "sampleDataSeaNonWhite.xlsx",
                        package = "mansched")
empReq <- readxl::read_xlsx(path = xlsxFile, sheet = "Requirement")
sched <- readxl::read_xlsx(path = xlsxFile,
                           sheet = "Schedule",
                           col_types = c("text", rep("numeric", times = 12)))
empPool <- readxl::read_xlsx(path = xlsxFile, sheet = "Pool")
hol <- readxl::read_xlsx(path = xlsxFile, sheet = "hol")
year <- 2018
forecast <- FALSE

mCols <- as.character(1:12)

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

tempData <- getmhDB(empReq   = empReq,
                    empPool  = empPool,
                    sched    = sched,
                    year     = year,
                    hol      = hol,
                    forecast = forecast)

wage <- readxl::read_xlsx(path = xlsxFile, sheet = "Wage")

tempData <- getCost(mhDB = tempData[[1]],
                    listR = listR,
                    wage = wage,
                    forecast = forecast)

mp13 <- tempData[[1]]
mp13 <- mp13[mp13$code == 521009L, mCols] %>% as.matrix() %>% as.vector()

sal <- c(wage$s[1], wage$i[1])

# Compute 13th  month pay
testMP13 <- rep(round(sal[1] * 26 / 12, digits = 2), times = 12)

test_that("Correct 13th MP for sea non-white", {
  expect_equal(sum(mp13), sum(testMP13))
})

rm(list = ls())
