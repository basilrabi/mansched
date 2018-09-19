library(mansched)

ID <- "Materials Handling General Employee"
costCode <- "14000"
spareFactor <- 1
calDays <- getCalDays(cBegin = "2018-01-01",
                      hol = getHol(hol = holidays, year = "2018"),
                      restday = "Sunday")

testDays <- c(25, 24, 27, 22, 25, 23, 25, 25, 24, 26, 24, 24)

tempEmp <- createEmp("divisionmanager")
tempEmp <- initTEmployee(theObject = tempEmp,
                         ID = ID,
                         costCode = costCode,
                         calDays = calDays)

test_that("division_manager() works", {
  expect_equal(class(tempEmp)[1], "DivisionManager")
  expect_equivalent(tempEmp@reg / 8, testDays)
  expect_equal(isRF(tempEmp), FALSE)
})

tempEmp <- createEmp("groupmanager")
tempEmp <- initTEmployee(theObject = tempEmp,
                         ID = ID,
                         costCode = costCode,
                         calDays = calDays)

test_that("group_manager() works", {
  expect_equal(class(tempEmp)[1], "GroupManager")
  expect_equivalent(tempEmp@reg / 8, testDays)
  expect_equal(isRF(tempEmp), FALSE)
})

tempEmp <- createEmp("departmentmanager")
tempEmp <- initTEmployee(theObject = tempEmp,
                         ID = ID,
                         costCode = costCode,
                         calDays = calDays)

test_that("department_manager() works", {
  expect_equal(class(tempEmp)[1], "DepartmentManager")
  expect_equivalent(tempEmp@reg / 8, testDays)
  expect_equal(isRF(tempEmp), FALSE)
})

tempEmp <- createEmp("sectionhead")
tempEmp <- initTEmployee(theObject = tempEmp,
                         ID = ID,
                         costCode = costCode,
                         calDays = calDays)

test_that("section_head() works", {
  expect_equal(class(tempEmp)[1], "SectionHead")
  expect_equivalent(tempEmp@reg / 8, testDays)
  expect_equal(isRF(tempEmp), FALSE)
})

OT <- 2
tempEmp <- createEmp("clerk")
tempEmp <- initTEmployee(theObject = tempEmp,
                         ID = ID,
                         costCode = costCode,
                         calDays = calDays,
                         OT = OT)

test_that("clerk() works", {
  expect_equal(class(tempEmp)[1], "Clerk")
  expect_equivalent(tempEmp@reg / 8, testDays)
  expect_equivalent(tempEmp@regOT / OT, testDays)
  expect_error(isRF(tempEmp))
})

mdtProb <- getMDTProb(getHol(hol = holidays, year = 2018))
tempEmp <- createEmp("technical")
tempEmp <- initTEmployee(theObject = tempEmp,
                         ID = ID,
                         costCode = costCode,
                         mdtProb = mdtProb,
                         OT = OT)

test_that("technical() works", {
  expect_equal(class(tempEmp)[1], "Technical")
  expect_equal(abs((sum(getHours(tempEmp)) / 10) - 365) < 1,
               TRUE)
})

tempEmp <- createEmp("supervisor")
tempEmp <- initTEmployee(theObject = tempEmp,
                         ID = ID,
                         costCode = costCode,
                         mdtProb = mdtProb,
                         OT = OT)

test_that("supervisor() works", {
  expect_equal(class(tempEmp)[1], "Supervisor")
  expect_equal(abs((sum(getHours(tempEmp)) / 10) - 365) < 1,
               TRUE)
})

tempEmp <- createEmp("laborer")
tempEmp <- initTEmployee(theObject = tempEmp,
                         ID = ID,
                         costCode = costCode,
                         mdtProb = mdtProb,
                         OT = OT)

test_that("laborer() works", {
  expect_equal(class(tempEmp)[1], "Laborer")
  expect_equal(abs((sum(getHours(tempEmp)) / 10) - 365) < 1,
               TRUE)
})

equipment <- "DT"

tempEmp <- createEmp("operator")
tempEmp <- initTEmployee(theObject = tempEmp,
                         ID = ID,
                         costCode = costCode,
                         mdtProb = mdtProb,
                         OT = OT,
                         equipment = equipment)

test_that("operator() works", {
  expect_equal(class(tempEmp)[1], "Operator")
  expect_equal(abs((sum(getHours(tempEmp)) / 10) - 365) < 1,
               TRUE)
})

rm(list = ls())
