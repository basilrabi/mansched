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

sal <- c(1000, 2000)
test_that("getHM() works", {
  expect_equal(sum(get13mp(tempEmp1, sal)$mp),
               round(2000 / 12, digits = 2) * 12)
  expect_equal(sum(get13mp(tempEmp2, sal)$mp),
               round(2000 / 12, digits = 2) * 11)
  expect_equal(sum(get13mp(tempEmp3, sal)$mp),
               round(2000 / 12, digits = 2) * 10)
  expect_equal(sum(get13mp(tempEmp4, sal)$mp),
               round(1000 / 12, digits = 2) * 9)
  expect_equal(sum(get13mp(tempEmp5, sal)$mp),
               round(1000 / 12, digits = 2) * 9)
  expect_equal(sum(get13mp(tempEmp6, sal)$mp),
               round(1000 / 12, digits = 2) * 9)
})

rm(list = ls())
