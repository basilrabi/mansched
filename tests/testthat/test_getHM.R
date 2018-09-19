library(mansched)

ID <- "S-240"
name <- "Basil Eric Rabi"
designation <- "Materials Engineering Supervisor"
costCode <- "14000"
status <- "reg"
cBegin <- "2012-10-15"
inHouse <- TRUE
restday <- "Tuesday"
hol <- getHol(hol = holidays, year = 2018)

calDays <- getCalDays(cBegin = cBegin,
                      hol = hol,
                      restday = restday)

tempEmp <- createEmp(empClass = "divisionmanager")
tempEmp1 <- initREmployee(theObject = tempEmp,
                          ID = ID,
                          name = name,
                          designation = designation,
                          costCode = costCode,
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
                          costCode = costCode,
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
                          costCode = costCode,
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
                          costCode = costCode,
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
                          costCode = costCode,
                          status = status,
                          cBegin = cBegin,
                          inHouse = inHouse,
                          restday = restday,
                          hol = hol,
                          equipment = equipment)

status <- "pro"
equipment <- "FL"
tempEmp <- createEmp(empClass = "operator")
tempEmp6 <- initREmployee(theObject = tempEmp,
                          ID = ID,
                          name = name,
                          designation = designation,
                          costCode = costCode,
                          status = status,
                          cBegin = cBegin,
                          inHouse = inHouse,
                          restday = restday,
                          hol = hol,
                          equipment = equipment)

test_that("getHM() works", {
  expect_equal(sum(getHM(tempEmp1)$HM), 1600 * 12)
  expect_equal(sum(getHM(tempEmp2)$HM), 1600 * 11)
  expect_equal(sum(getHM(tempEmp3)$HM), 1600 * 10)
  expect_equal(sum(getHM(tempEmp4)$HM), 600 * 1)
  expect_equal(sum(getHM(tempEmp5)$HM), 600 * 1)
  expect_equal(sum(getHM(tempEmp6)$HM), 1000 * 1)
})

rm(list = ls())
