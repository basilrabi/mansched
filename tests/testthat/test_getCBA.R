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
status <- "pro"
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

test_that("getCBA() works for budget", {
  expect_equal(sum(getCBA(tempEmp1)$benefits), 508.33 * 12)
  expect_equal(sum(getCBA(tempEmp2)$benefits), 508.33 * 11)
  expect_equal(sum(getCBA(tempEmp3)$benefits), 508.33 * 10)
  expect_equal(sum(getCBA(tempEmp4)$benefits), 0)
  expect_equal(sum(getCBA(tempEmp5)$benefits), 0)
  expect_equal(sum(getCBA(tempEmp6)$benefits), 0)
  expect_equal(sum(getCBA(tempEmp7)$benefits), 0)
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

test_that("getCBA() works for forecast", {
  expect_equal(sum(getCBA(tempEmp1)$benefits), 508.33 * 12)
  expect_equal(sum(getCBA(tempEmp2)$benefits), 508.33 * 11)
  expect_equal(sum(getCBA(tempEmp3)$benefits), 508.33 * 10)
  expect_equal(sum(getCBA(tempEmp4)$benefits), 0)
  expect_equal(sum(getCBA(tempEmp5)$benefits), 0)
  expect_equal(sum(getCBA(tempEmp6)$benefits), 0)
  expect_equal(sum(getCBA(tempEmp7)$benefits), 0)
})

rm(list = ls())
