library(mansched)

ID <- "S-240"
name <- "Basil Eric Rabi"
designation <- "Materials Engineering Supervisor"
costCenter <- "14000"
status <- "reg"
cBegin <- "2012-10-15"
inHouse <- TRUE
restday <- "Tuesday"
hol <- getHol(hol = holidays, year = 2018)

tempEmp <- createEmp(empClass = "divisionmanager")
tempEmp1 <- initREmployee(theObject = tempEmp,
                          ID = ID,
                          name = name,
                          designation = designation,
                          costCenter = costCenter,
                          status = status,
                          cBegin = cBegin,
                          inHouse = inHouse,
                          restday = restday,
                          hol = hol)

cBegin <- "2018-02-01"
inHouse <- FALSE
tempEmp <- createEmp(empClass = "groupmanager")
tempEmp2 <- initREmployee(theObject = tempEmp,
                          ID = ID,
                          name = name,
                          designation = designation,
                          costCenter = costCenter,
                          status = status,
                          cBegin = cBegin,
                          inHouse = inHouse,
                          restday = restday,
                          hol = hol)

cBegin <- "2018-03-01"
inHouse <- FALSE
tempEmp <- createEmp(empClass = "clerk")
tempEmp3 <- initREmployee(theObject = tempEmp,
                          ID = ID,
                          name = name,
                          designation = designation,
                          costCenter = costCenter,
                          status = status,
                          cBegin = cBegin,
                          inHouse = inHouse,
                          restday = restday,
                          hol = hol,
                          RF = FALSE)

cBegin <- "2018-04-01"
inHouse <- TRUE
status <- "sea"
tempEmp <- createEmp(empClass = "technical")
tempEmp4 <- initREmployee(theObject = tempEmp,
                          ID = ID,
                          name = name,
                          designation = designation,
                          costCenter = costCenter,
                          status = status,
                          cBegin = cBegin,
                          inHouse = inHouse,
                          restday = restday,
                          hol = hol)

inHouse <- TRUE
status <- "sea"
equipment <- "FL"
tempEmp <- createEmp(empClass = "operator")
tempEmp5 <- initREmployee(theObject = tempEmp,
                          ID = ID,
                          name = name,
                          designation = designation,
                          costCenter = costCenter,
                          status = status,
                          cBegin = cBegin,
                          inHouse = inHouse,
                          restday = restday,
                          hol = hol,
                          equipment = equipment)

cEnd <- "2018-10-31"
inHouse <- TRUE
status <- "sea"
tempEmp <- createEmp(empClass = "technical")
tempEmp6 <- initREmployee(theObject = tempEmp,
                          ID = ID,
                          name = name,
                          designation = designation,
                          costCenter = costCenter,
                          status = status,
                          cBegin = cBegin,
                          cEnd = cEnd,
                          inHouse = inHouse,
                          restday = restday,
                          hol = hol)

cEnd <- "2018-10-31"
inHouse <- TRUE
status <- "age"
tempEmp <- createEmp(empClass = "laborer")
tempEmp7 <- initREmployee(theObject = tempEmp,
                          ID = ID,
                          name = name,
                          designation = designation,
                          costCenter = costCenter,
                          status = status,
                          cBegin = cBegin,
                          cEnd = cEnd,
                          inHouse = inHouse,
                          restday = restday,
                          hol = hol)

name <- "BARGE MAINTENANCE 1"
status <- "age"
cBegin <- "2018-01-01"
inHouse <- FALSE
restday <- "Tuesday"
tempEmp <- createEmp(empClass = "laborer")
tempEmp8 <- initREmployee(theObject = tempEmp,
                          ID = ID,
                          name = name,
                          designation = designation,
                          costCenter = costCenter,
                          status = status,
                          cBegin = cBegin,
                          inHouse = inHouse,
                          restday = restday,
                          hol = hol,
                          dependents = c(NA, rep(1L, times = 10), NA))

name <- "ORE BREAKER 1"
status <- "age"
cBegin <- "2018-01-01"
inHouse <- FALSE
restday <- "Tuesday"
tempEmp <- createEmp(empClass = "laborer")
tempEmp9 <- initREmployee(theObject = tempEmp,
                          ID = ID,
                          name = name,
                          designation = designation,
                          costCenter = costCenter,
                          status = status,
                          cBegin = cBegin,
                          inHouse = inHouse,
                          restday = restday,
                          hol = hol,
                          dependents = c(NA, rep(1L, times = 10), NA))

test_that("getRiceSub() works in budget", {
  expect_equal(sum(getRiceSub(tempEmp1)$riceSub), 2500 * 12)
  expect_equal(sum(getRiceSub(tempEmp2)$riceSub), 2500 * 11)
  expect_equal(sum(getRiceSub(tempEmp3)$riceSub), 2500 * 10)
  expect_equal(sum(getRiceSub(tempEmp4)$riceSub), 2500 * 9)
  expect_equal(sum(getRiceSub(tempEmp5)$riceSub), 2500 * 9)
  expect_equal(sum(getRiceSub(tempEmp6)$riceSub), 2500 * 7)
  expect_equal(sum(getRiceSub(tempEmp7)$riceSub), 2500 * 7)
  expect_equal(sum(getRiceSub(tempEmp8)$riceSub), 0)
  expect_equal(sum(getRiceSub(tempEmp9)$riceSub), 2500 * 12 * 0.5)
})

# Test for forecast

ID <- "S-240"
name <- "Basil Eric Rabi"
designation <- "Materials Engineering Supervisor"
costCenter <- "14000"
status <- "reg"
cBegin <- "2012-10-15"
inHouse <- TRUE
restday <- "Tuesday"
hol <- getHol(hol = holidays, year = 2018)

tempEmp <- createEmp(empClass = "divisionmanager")
tempEmp1 <- initREmployee(theObject = tempEmp,
                          ID = ID,
                          name = name,
                          designation = designation,
                          costCenter = costCenter,
                          status = status,
                          cBegin = cBegin,
                          inHouse = inHouse,
                          restday = restday,
                          hol = hol,
                          forecast = TRUE)

cBegin <- "2018-02-01"
inHouse <- FALSE
tempEmp <- createEmp(empClass = "groupmanager")
tempEmp2 <- initREmployee(theObject = tempEmp,
                          ID = ID,
                          name = name,
                          designation = designation,
                          costCenter = costCenter,
                          status = status,
                          cBegin = cBegin,
                          inHouse = inHouse,
                          restday = restday,
                          hol = hol,
                          forecast = TRUE)

cBegin <- "2018-03-01"
inHouse <- FALSE
tempEmp <- createEmp(empClass = "clerk")
tempEmp3 <- initREmployee(theObject = tempEmp,
                          ID = ID,
                          name = name,
                          designation = designation,
                          costCenter = costCenter,
                          status = status,
                          cBegin = cBegin,
                          inHouse = inHouse,
                          restday = restday,
                          hol = hol,
                          RF = FALSE,
                          forecast = TRUE)

cBegin <- "2018-04-01"
inHouse <- TRUE
status <- "sea"
tempEmp <- createEmp(empClass = "technical")
tempEmp4 <- initREmployee(theObject = tempEmp,
                          ID = ID,
                          name = name,
                          designation = designation,
                          costCenter = costCenter,
                          status = status,
                          cBegin = cBegin,
                          inHouse = inHouse,
                          restday = restday,
                          hol = hol,
                          forecast = TRUE)

inHouse <- TRUE
status <- "sea"
equipment <- "FL"
tempEmp <- createEmp(empClass = "operator")
tempEmp5 <- initREmployee(theObject = tempEmp,
                          ID = ID,
                          name = name,
                          designation = designation,
                          costCenter = costCenter,
                          status = status,
                          cBegin = cBegin,
                          inHouse = inHouse,
                          restday = restday,
                          hol = hol,
                          equipment = equipment,
                          forecast = TRUE)

cEnd <- "2018-10-31"
inHouse <- TRUE
status <- "sea"
tempEmp <- createEmp(empClass = "technical")
tempEmp6 <- initREmployee(theObject = tempEmp,
                          ID = ID,
                          name = name,
                          designation = designation,
                          costCenter = costCenter,
                          status = status,
                          cBegin = cBegin,
                          cEnd = cEnd,
                          inHouse = inHouse,
                          restday = restday,
                          hol = hol,
                          forecast = TRUE)

cEnd <- "2018-10-31"
inHouse <- TRUE
status <- "age"
tempEmp <- createEmp(empClass = "laborer")
tempEmp7 <- initREmployee(theObject = tempEmp,
                          ID = ID,
                          name = name,
                          designation = designation,
                          costCenter = costCenter,
                          status = status,
                          cBegin = cBegin,
                          cEnd = cEnd,
                          inHouse = inHouse,
                          restday = restday,
                          hol = hol,
                          forecast = TRUE)

name <- "BARGE MAINTENANCE 1"
status <- "age"
cBegin <- "2018-01-01"
inHouse <- FALSE
restday <- "Tuesday"
tempEmp <- createEmp(empClass = "laborer")
tempEmp8 <- initREmployee(theObject = tempEmp,
                          ID = ID,
                          name = name,
                          designation = designation,
                          costCenter = costCenter,
                          status = status,
                          cBegin = cBegin,
                          inHouse = inHouse,
                          restday = restday,
                          hol = hol,
                          forecast = TRUE,
                          dependents = c(NA, rep(1L, times = 10), NA))

name <- "ORE BREAKER 1"
status <- "age"
cBegin <- "2018-01-01"
inHouse <- FALSE
restday <- "Tuesday"
tempEmp <- createEmp(empClass = "laborer")
tempEmp9 <- initREmployee(theObject = tempEmp,
                          ID = ID,
                          name = name,
                          designation = designation,
                          costCenter = costCenter,
                          status = status,
                          cBegin = cBegin,
                          inHouse = inHouse,
                          restday = restday,
                          hol = hol,
                          forecast = TRUE,
                          dependents = c(NA, rep(1L, times = 10), NA))

test_that("getRiceSub() works in forecast", {
  expect_equal(sum(getRiceSub(tempEmp1)$riceSub), 2300 * 12)
  expect_equal(sum(getRiceSub(tempEmp2)$riceSub), 2300 * 11)
  expect_equal(sum(getRiceSub(tempEmp3)$riceSub), 2300 * 10)
  expect_equal(sum(getRiceSub(tempEmp4)$riceSub), 2300 * 9)
  expect_equal(sum(getRiceSub(tempEmp5)$riceSub), 2300 * 9)
  expect_equal(sum(getRiceSub(tempEmp6)$riceSub), 2300 * 7)
  expect_equal(sum(getRiceSub(tempEmp7)$riceSub), 2300 * 7)
  expect_equal(sum(getRiceSub(tempEmp8)$riceSub), 0)
  expect_equal(sum(getRiceSub(tempEmp9)$riceSub), 2300 * 12 * 0.5)
})

rm(list = ls())
