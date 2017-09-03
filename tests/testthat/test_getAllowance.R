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

tempEmp <- createEmp(empClass = "division manager")
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
tempEmp <- createEmp(empClass = "group manager")
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
                          isRF = FALSE)

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


test_that("getAllowance() works", {
  expect_equal(sum(getAllowance(tempEmp1)$allowance), 1100 * 12)
  expect_equal(sum(getAllowance(tempEmp2)$allowance), 4100 * 11)
  expect_equal(sum(getAllowance(tempEmp3)$allowance), 4000 * 10)
  expect_equal(sum(getAllowance(tempEmp4)$allowance), 1800 * 9)
  expect_equal(sum(getAllowance(tempEmp5)$allowance), 0)
})
