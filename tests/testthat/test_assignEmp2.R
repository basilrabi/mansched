library(mansched)

ID <- "S-240"
name <- "Basil Eric Rabi"
designation <- "Materials Engineering Supervisor"
costCode <- "14000"
status <- "reg"
cBegin <- "2012-10-15"
inHouse <- TRUE
restday <- "Sunday"
hol <- getHol(hol = holidays, year = 2018)
calDays <- getCalDays(cBegin = cBegin, hol = hol, restday = restday)

tempEmp <- createEmp(empClass = "division manager")
empR <- initREmployee(theObject = tempEmp,
                      ID = ID,
                      name = name,
                      designation = designation,
                      costCode = costCode,
                      status = status,
                      cBegin = cBegin,
                      inHouse = inHouse,
                      restday = restday,
                      hol = hol)
preHours <- sum(getHours(empR))
empT <- initTEmployee(theObject = tempEmp,
                      ID = ID,
                      costCode = costCode,
                      calDays = calDays)
tempData <- assignEmp2(empT = empT, empR = empR)
test_that("assignment of division manager works", {
  expect_equal(preHours,
               sum(tempData[[1]]$mh) + sum(getHours(tempData[[3]])))
})

tempEmp <- createEmp(empClass = "group manager")
empR <- initREmployee(theObject = tempEmp,
                      ID = ID,
                      name = name,
                      designation = designation,
                      costCode = costCode,
                      status = status,
                      cBegin = cBegin,
                      inHouse = inHouse,
                      restday = restday,
                      hol = hol)
preHours <- sum(getHours(empR))
empT <- initTEmployee(theObject = tempEmp,
                      ID = ID,
                      costCode = costCode,
                      calDays = calDays)
tempData <- assignEmp2(empT = empT, empR = empR)
test_that("assignment of group manager works", {
  expect_equal(preHours,
               sum(tempData[[1]]$mh) + sum(getHours(tempData[[3]])))
})

tempEmp <- createEmp(empClass = "department manager")
empR <- initREmployee(theObject = tempEmp,
                      ID = ID,
                      name = name,
                      designation = designation,
                      costCode = costCode,
                      status = status,
                      cBegin = cBegin,
                      inHouse = inHouse,
                      restday = restday,
                      hol = hol)
preHours <- sum(getHours(empR))
empT <- initTEmployee(theObject = tempEmp,
                      ID = ID,
                      costCode = costCode,
                      calDays = calDays)
tempData <- assignEmp2(empT = empT, empR = empR)
test_that("assignment of department manager works", {
  expect_equal(preHours,
               sum(tempData[[1]]$mh) + sum(getHours(tempData[[3]])))
})

tempEmp <- createEmp(empClass = "section head")
empR <- initREmployee(theObject = tempEmp,
                      ID = ID,
                      name = name,
                      designation = designation,
                      costCode = costCode,
                      status = status,
                      cBegin = cBegin,
                      inHouse = inHouse,
                      restday = restday,
                      hol = hol)
preHours <- sum(getHours(empR))
empT <- initTEmployee(theObject = tempEmp,
                      ID = ID,
                      costCode = costCode,
                      calDays = calDays)
tempData <- assignEmp2(empT = empT, empR = empR)
test_that("assignment of section head works", {
  expect_equal(preHours,
               sum(tempData[[1]]$mh) + sum(getHours(tempData[[3]])))
})

OT <- 4
tempEmp <- createEmp(empClass = "clerk")
empR <- initREmployee(theObject = tempEmp,
                      ID = ID,
                      name = name,
                      designation = designation,
                      costCode = costCode,
                      status = status,
                      cBegin = cBegin,
                      inHouse = inHouse,
                      restday = restday,
                      hol = hol,
                      OT =OT)
preHours <- sum(getHours(empR))
empT <- initTEmployee(theObject = tempEmp,
                      ID = ID,
                      costCode = costCode,
                      OT = OT,
                      calDays = calDays)
tempData <- assignEmp2(empT = empT, empR = empR)
test_that("assignment of clerk works", {
  expect_equal(preHours,
               sum(tempData[[1]]$mh) + sum(getHours(tempData[[3]])))
})


mdtProb <- getMDTProb(hol = hol)
tempEmp <- createEmp(empClass = "technical")
empR <- initREmployee(theObject = tempEmp,
                      ID = ID,
                      name = name,
                      designation = designation,
                      costCode = costCode,
                      status = status,
                      cBegin = cBegin,
                      inHouse = inHouse,
                      restday = restday,
                      hol = hol,
                      OT =OT,
                      d.ho = 5)
preHours <- sum(getHours(empR))
empT <- initTEmployee(theObject = tempEmp,
                      ID = ID,
                      costCode = costCode,
                      OT = OT,
                      mdtProb = mdtProb)
tempData <- assignEmp2(empT = empT, empR = empR)
test_that("assignment of technical works", {
  expect_equal(preHours,
               sum(tempData[[1]]$mh) + sum(getHours(tempData[[3]])))
})

tempEmp <- createEmp(empClass = "supervisor")
empR <- initREmployee(theObject = tempEmp,
                      ID = ID,
                      name = name,
                      designation = designation,
                      costCode = costCode,
                      status = status,
                      cBegin = cBegin,
                      inHouse = inHouse,
                      restday = restday,
                      hol = hol,
                      OT =OT)
preHours <- sum(getHours(empR))
empT <- initTEmployee(theObject = tempEmp,
                      ID = ID,
                      costCode = costCode,
                      OT = OT,
                      mdtProb = mdtProb)
tempData <- assignEmp2(empT = empT, empR = empR)
test_that("assignment of supervisor works", {
  expect_equal(preHours,
               sum(tempData[[1]]$mh) + sum(getHours(tempData[[3]])))
})

tempEmp <- createEmp(empClass = "laborer")
empR <- initREmployee(theObject = tempEmp,
                      ID = ID,
                      name = name,
                      designation = designation,
                      costCode = costCode,
                      status = status,
                      cBegin = cBegin,
                      inHouse = inHouse,
                      restday = restday,
                      hol = hol,
                      OT =OT)
preHours <- sum(getHours(empR))
empT <- initTEmployee(theObject = tempEmp,
                      ID = ID,
                      costCode = costCode,
                      OT = OT,
                      mdtProb = mdtProb)
tempData <- assignEmp2(empT = empT, empR = empR)
test_that("assignment of laborer works", {
  expect_equal(preHours,
               sum(tempData[[1]]$mh) + sum(getHours(tempData[[3]])))
})

equipment <- "TX"
tempEmp <- createEmp(empClass = "operator")
empR <- initREmployee(theObject = tempEmp,
                      ID = ID,
                      name = name,
                      designation = designation,
                      costCode = costCode,
                      status = status,
                      cBegin = cBegin,
                      inHouse = inHouse,
                      restday = restday,
                      hol = hol,
                      OT =OT,
                      equipment = equipment)
preHours <- sum(getHours(empR))
empT <- initTEmployee(theObject = tempEmp,
                      ID = ID,
                      costCode = costCode,
                      OT = OT,
                      mdtProb = mdtProb,
                      equipment = equipment)
tempData <- assignEmp2(empT = empT, empR = empR)
test_that("assignment of operator works", {
  expect_equal(preHours,
               sum(tempData[[1]]$mh) + sum(getHours(tempData[[3]])))
})
