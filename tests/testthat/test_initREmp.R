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

# getCalDays(cBegin = cBegin,
#            hol = hol,
#            restday = restday)

tempEmp <- createEmp(empClass = "division manager")
tempEmp <- initREmployee(theObject = tempEmp,
                         ID = ID,
                         name = name,
                         designation = designation,
                         costCode = costCode,
                         status = status,
                         cBegin = cBegin,
                         inHouse = inHouse,
                         restday = restday,
                         hol = hol)

test_that("division_manager() works", {
  expect_equal(class(tempEmp)[1], "Division Manager")
  expect_equivalent(tempEmp@totHours / 8,
                    c(26, 24, 27, 26, 26, 26, 26, 27, 26, 26, 26, 27))
  expect_equivalent(tempEmp@maxReg / 8,
                    c(24, 23, 27, 22, 26, 26, 25, 27, 24, 25, 24, 25))
  expect_equal(tempEmp@holHours,
               tempEmp@totHours - tempEmp@maxReg)
})

tempEmp <- createEmp(empClass = "group manager")
tempEmp <- initREmployee(theObject = tempEmp,
                         ID = ID,
                         name = name,
                         designation = designation,
                         costCode = costCode,
                         status = status,
                         cBegin = cBegin,
                         inHouse = inHouse,
                         restday = restday,
                         hol = hol)

test_that("group_manager() works", {
  expect_equal(class(tempEmp)[1], "Group Manager")
  expect_equivalent(tempEmp@totHours / 8,
                    c(26, 24, 27, 26, 26, 26, 26, 27, 26, 26, 26, 27))
  expect_equivalent(tempEmp@maxReg / 8,
                    c(24, 23, 27, 22, 26, 26, 25, 27, 24, 25, 24, 25))
  expect_equal(tempEmp@holHours,
               tempEmp@totHours - tempEmp@maxReg)
})

tempEmp <- createEmp(empClass = "department manager")
tempEmp <- initREmployee(theObject = tempEmp,
                         ID = ID,
                         name = name,
                         designation = designation,
                         costCode = costCode,
                         status = status,
                         cBegin = cBegin,
                         inHouse = inHouse,
                         restday = restday,
                         hol = hol)

test_that("department_manager() works", {
  expect_equal(class(tempEmp)[1], "Department Manager")
  expect_equivalent(tempEmp@totHours / 8,
                    c(26, 24, 27, 26, 26, 26, 26, 27, 26, 26, 26, 27))
  expect_equivalent(tempEmp@maxReg / 8,
                    c(24, 23, 27, 22, 26, 26, 25, 27, 24, 25, 24, 25))
  expect_equal(tempEmp@holHours,
               tempEmp@totHours - tempEmp@maxReg)
})

tempEmp <- createEmp(empClass = "section head")
tempEmp <- initREmployee(theObject = tempEmp,
                         ID = ID,
                         name = name,
                         designation = designation,
                         costCode = costCode,
                         status = status,
                         cBegin = cBegin,
                         inHouse = inHouse,
                         restday = restday,
                         hol = hol)

test_that("section_head() works", {
  expect_equal(class(tempEmp)[1], "Section Head")
  expect_equivalent(tempEmp@totHours / 8,
                    c(26, 24, 27, 26, 26, 26, 26, 27, 26, 26, 26, 27))
  expect_equivalent(tempEmp@maxReg / 8,
                    c(24, 23, 27, 22, 26, 26, 25, 27, 24, 25, 24, 25))
  expect_equal(tempEmp@holHours,
               tempEmp@totHours - tempEmp@maxReg)
})

tempEmp <- createEmp(empClass = "clerk")
tempEmp <- initREmployee(theObject = tempEmp,
                         ID = ID,
                         name = name,
                         designation = designation,
                         costCode = costCode,
                         status = status,
                         cBegin = cBegin,
                         inHouse = inHouse,
                         restday = restday,
                         hol = hol)

test_that("clerk() works", {
  expect_equal(class(tempEmp)[1], "Clerk")
  expect_equivalent(tempEmp@totHours / 8,
                    c(26, 24, 27, 26, 26, 26, 26, 27, 26, 26, 26, 27))
  expect_equivalent(tempEmp@maxReg / 8,
                    c(24, 23, 27, 22, 26, 26, 25, 27, 24, 25, 24, 25))
  expect_equal(tempEmp@holHours,
               tempEmp@totHours - tempEmp@maxReg)
  expect_equivalent(tempEmp@regOT,
                    (tempEmp@maxReg /8) * 3)
})

tempEmp <- createEmp(empClass = "technical")
tempEmp <- initREmployee(theObject = tempEmp,
                         ID = ID,
                         name = name,
                         designation = designation,
                         costCode = costCode,
                         status = status,
                         cBegin = cBegin,
                         inHouse = inHouse,
                         restday = restday,
                         hol = hol)

test_that("technical() works", {
  expect_equal(class(tempEmp)[1], "Technical")
  expect_equal(tempEmp@isRF, FALSE)
  expect_equal(sum(c(tempEmp@reg,
                     tempEmp@regOT,
                     tempEmp@rd,
                     tempEmp@rdOT,
                     tempEmp@sh,
                     tempEmp@shOT,
                     tempEmp@lh,
                     tempEmp@lhOT,
                     tempEmp@nh,
                     tempEmp@nhOT,
                     tempEmp@rs,
                     tempEmp@rsOT,
                     tempEmp@rl,
                     tempEmp@rlOT,
                     tempEmp@rn,
                     tempEmp@rnOT)),
               365 * 11)
})

tempEmp <- createEmp(empClass = "supervisor")
tempEmp <- initREmployee(theObject = tempEmp,
                         ID = ID,
                         name = name,
                         designation = designation,
                         costCode = costCode,
                         status = status,
                         cBegin = cBegin,
                         inHouse = inHouse,
                         restday = restday,
                         hol = hol)

test_that("supervisor() works", {
  expect_equal(class(tempEmp)[1], "Supervisor")
  expect_equal(tempEmp@isRF, FALSE)
  expect_equal(sum(c(tempEmp@reg,
                     tempEmp@regOT,
                     tempEmp@rd,
                     tempEmp@rdOT,
                     tempEmp@sh,
                     tempEmp@shOT,
                     tempEmp@lh,
                     tempEmp@lhOT,
                     tempEmp@nh,
                     tempEmp@nhOT,
                     tempEmp@rs,
                     tempEmp@rsOT,
                     tempEmp@rl,
                     tempEmp@rlOT,
                     tempEmp@rn,
                     tempEmp@rnOT)),
               365 * 11)
})

status <- "sea"
tempEmp <- createEmp(empClass = "laborer")
tempEmp <- initREmployee(theObject = tempEmp,
                         ID = ID,
                         name = name,
                         designation = designation,
                         costCode = costCode,
                         status = status,
                         cBegin = cBegin,
                         inHouse = inHouse,
                         restday = restday,
                         hol = hol)

test_that("laborer() works", {
  expect_equal(class(tempEmp)[1], "Laborer")
  expect_equal(tempEmp@isRF, TRUE)
  expect_equal(sum(c(tempEmp@reg,
                     tempEmp@regOT,
                     tempEmp@rd,
                     tempEmp@rdOT,
                     tempEmp@sh,
                     tempEmp@shOT,
                     tempEmp@lh,
                     tempEmp@lhOT,
                     tempEmp@nh,
                     tempEmp@nhOT,
                     tempEmp@rs,
                     tempEmp@rsOT,
                     tempEmp@rl,
                     tempEmp@rlOT,
                     tempEmp@rn,
                     tempEmp@rnOT)),
               365 * 11)
})

equipment <- "TX WL DT"
tempEmp <- createEmp(empClass = "operator")
tempEmp <- initREmployee(theObject = tempEmp,
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

test_that("operator() works", {
  expect_equal(class(tempEmp)[1], "Operator")
  expect_equal(tempEmp@isRF, TRUE)
  expect_equal(sum(c(tempEmp@reg,
                     tempEmp@regOT,
                     tempEmp@rd,
                     tempEmp@rdOT,
                     tempEmp@sh,
                     tempEmp@shOT,
                     tempEmp@lh,
                     tempEmp@lhOT,
                     tempEmp@nh,
                     tempEmp@nhOT,
                     tempEmp@rs,
                     tempEmp@rsOT,
                     tempEmp@rl,
                     tempEmp@rlOT,
                     tempEmp@rn,
                     tempEmp@rnOT)),
               365 * 11)
})
