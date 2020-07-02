library(mansched)

ID <- "S-240"
name <- "Basil Eric Rabi"
designation <- "Materials Engineering Supervisor"
costCenter <- "14000"
status <- "reg"
cBegin <- "2012-10-15"
inHouse <- TRUE
restday <- "Sunday"
hol <- getHol(hol = holidays, year = 2018)
mdtProb <- getMDTProb(hol = hol)
calDays <- getCalDays(cBegin = cBegin, hol = hol, restday = restday)

tempEmp <- createEmp(empClass = "divisionmanager")
empR <- initREmployee(theObject = tempEmp,
                      ID = ID,
                      name = name,
                      designation = designation,
                      costCenter = costCenter,
                      status = status,
                      cBegin = cBegin,
                      inHouse = inHouse,
                      restday = restday,
                      hol = hol)
preHours <- sum(getHours(empR))
empT <- initTEmployee(theObject = tempEmp,
                      ID = ID,
                      costCenter = costCenter,
                      calDays = calDays)
tempData <- assignEmp(empT = empT, empR = empR, selfAssign = FALSE)
test_that("assignment of division manager works", {
  expect_equal(preHours, sum(tempData$mh) + sum(getHours(empR)))
})

tempEmp <- createEmp(empClass = "groupmanager")
empR <- initREmployee(theObject = tempEmp,
                      ID = ID,
                      name = name,
                      designation = designation,
                      costCenter = costCenter,
                      status = status,
                      cBegin = cBegin,
                      inHouse = inHouse,
                      restday = restday,
                      hol = hol)
preHours <- sum(getHours(empR))
empT <- initTEmployee(theObject = tempEmp,
                      ID = ID,
                      costCenter = costCenter,
                      calDays = calDays)
tempData <- assignEmp(empT = empT, empR = empR, selfAssign = FALSE)
test_that("assignment of group manager works", {
  expect_equal(preHours, sum(tempData$mh) + sum(getHours(empR)))
})

tempEmp <- createEmp(empClass = "departmentmanager")
empR <- initREmployee(theObject = tempEmp,
                      ID = ID,
                      name = name,
                      designation = designation,
                      costCenter = costCenter,
                      status = status,
                      cBegin = cBegin,
                      inHouse = inHouse,
                      restday = restday,
                      hol = hol)
preHours <- sum(getHours(empR))
empT <- initTEmployee(theObject = tempEmp,
                      ID = ID,
                      costCenter = costCenter,
                      calDays = calDays)
tempData <- assignEmp(empT = empT, empR = empR, selfAssign = FALSE)
test_that("assignment of department manager works", {
  expect_equal(preHours, sum(tempData$mh) + sum(getHours(empR)))
})

tempEmp <- createEmp(empClass = "sectionhead")
empR <- initREmployee(theObject = tempEmp,
                      ID = ID,
                      name = name,
                      designation = designation,
                      costCenter = costCenter,
                      status = status,
                      cBegin = cBegin,
                      inHouse = inHouse,
                      restday = restday,
                      hol = hol)
preHours <- sum(getHours(empR))
empT <- initTEmployee(theObject = tempEmp,
                      ID = ID,
                      costCenter = costCenter,
                      calDays = calDays)
tempData <- assignEmp(empT = empT, empR = empR, selfAssign = FALSE)
test_that("assignment of section head works", {
  expect_equal(preHours, sum(tempData$mh) + sum(getHours(empR)))
})

OT <- 4
tempEmp <- createEmp(empClass = "clerk")
empR <- initREmployee(theObject = tempEmp,
                      ID = ID,
                      name = name,
                      designation = designation,
                      costCenter = costCenter,
                      status = status,
                      cBegin = cBegin,
                      inHouse = inHouse,
                      restday = restday,
                      hol = hol,
                      OT = OT)
preHours <- sum(getHours(empR))
empT <- initTEmployee(theObject = tempEmp,
                      ID = ID,
                      costCenter = costCenter,
                      OT = OT,
                      mdtProb = mdtProb,
                      calDays = calDays)
tempData <- assignEmp(empT = empT, empR = empR, selfAssign = FALSE)
test_that("assignment of clerk works", {
  expect_equal(preHours, sum(tempData$mh) + sum(getHours(empR)))
})

tempEmp <- createEmp(empClass = "technical")
empR <- initREmployee(theObject = tempEmp,
                      ID = ID,
                      name = name,
                      designation = designation,
                      costCenter = costCenter,
                      status = status,
                      cBegin = cBegin,
                      inHouse = inHouse,
                      restday = restday,
                      hol = hol,
                      OT = OT,
                      d.ho = rep(5L, times = 12))
preHours <- sum(getHours(empR))
empT <- initTEmployee(theObject = tempEmp,
                      ID = ID,
                      costCenter = costCenter,
                      OT = OT,
                      mdtProb = mdtProb)
tempData <- assignEmp(empT = empT, empR = empR, selfAssign = FALSE)
test_that("assignment of technical works", {
  expect_equal(preHours, sum(tempData$mh) + sum(getHours(empR)))
})

tempEmp <- createEmp(empClass = "supervisor")
empR <- initREmployee(theObject = tempEmp,
                      ID = ID,
                      name = name,
                      designation = designation,
                      costCenter = costCenter,
                      status = status,
                      cBegin = cBegin,
                      inHouse = inHouse,
                      restday = restday,
                      hol = hol,
                      OT = OT)
preHours <- sum(getHours(empR))
empT <- initTEmployee(theObject = tempEmp,
                      ID = ID,
                      costCenter = costCenter,
                      OT = OT,
                      mdtProb = mdtProb)
tempData <- assignEmp(empT = empT, empR = empR, selfAssign = FALSE)
test_that("assignment of supervisor works", {
  expect_equal(preHours, sum(tempData$mh) + sum(getHours(empR)))
})

tempEmp <- createEmp(empClass = "laborer")
empR <- initREmployee(theObject = tempEmp,
                      ID = ID,
                      name = name,
                      designation = designation,
                      costCenter = costCenter,
                      status = status,
                      cBegin = cBegin,
                      inHouse = inHouse,
                      restday = restday,
                      hol = hol,
                      OT = OT)
preHours <- sum(getHours(empR))
empT <- initTEmployee(theObject = tempEmp,
                      ID = ID,
                      costCenter = costCenter,
                      OT = OT,
                      mdtProb = mdtProb)
tempData <- assignEmp(empT = empT, empR = empR, selfAssign = FALSE)
test_that("assignment of laborer works", {
  expect_equal(preHours, sum(tempData$mh) + sum(getHours(empR)))
})

tempEmp <- createEmp(empClass = "operator")
empR <- initREmployee(theObject = tempEmp,
                      ID = ID,
                      name = name,
                      designation = designation,
                      costCenter = costCenter,
                      status = status,
                      cBegin = cBegin,
                      inHouse = inHouse,
                      restday = restday,
                      hol = hol,
                      OT = OT,
                      equipment = "TX WX")
preHours <- sum(getHours(empR))
empT <- initTEmployee(theObject = tempEmp,
                      ID = ID,
                      costCenter = costCenter,
                      OT = OT,
                      mdtProb = mdtProb,
                      equipment = "TX")
tempData <- assignEmp(empT = empT, empR = empR, selfAssign = FALSE)
test_that("assignment of operator works", {
  expect_equal(preHours, sum(tempData$mh) + sum(getHours(empR)))
})

rm(list = ls())
