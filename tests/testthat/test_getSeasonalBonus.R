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

cEnd <- "2018-11-01"
inHouse <- FALSE
tempEmp <- createEmp(empClass = "groupmanager")
tempEmp2 <- initREmployee(theObject = tempEmp,
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

cBegin <- "2018-03-01"
inHouse <- FALSE
status <- "pro"
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

cBegin <- "2018-01-01"
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

test_that("getSigningBonusSea() works", {
  expect_equal(sum(getSigningBonusSea(tempEmp1)$signingBonus), 0)
  expect_equal(sum(getSigningBonusSea(tempEmp2)$signingBonus), 0)
  expect_equal(sum(getSigningBonusSea(tempEmp3)$signingBonus), 0)
  expect_equal(sum(getSigningBonusSea(tempEmp4)$signingBonus), 4186)
  expect_equal(sum(getSigningBonusSea(tempEmp5)$signingBonus), 4186)
})

test_that("getRetentionBonus() works", {
  expect_equal(sum(getRetentionBonus(tempEmp1)$retentionBonus), 0)
  expect_equal(sum(getRetentionBonus(tempEmp2)$retentionBonus), 0)
  expect_equal(sum(getRetentionBonus(tempEmp3)$retentionBonus), 0)
  expect_equal(sum(getRetentionBonus(tempEmp4)$retentionBonus), 16744)
  expect_equal(sum(getRetentionBonus(tempEmp5)$retentionBonus), 16744)
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

cBegin <- "2018-05-01"
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

cBegin <- "2018-06-01"
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

test_that("getSigningBonusSea() works", {
  expect_equal(sum(getSigningBonusSea(tempEmp1)$signingBonus), 0)
  expect_equal(sum(getSigningBonusSea(tempEmp2)$signingBonus), 0)
  expect_equal(sum(getSigningBonusSea(tempEmp3)$signingBonus), 0)
  expect_equal(sum(getSigningBonusSea(tempEmp4)$signingBonus), 4186)
  expect_equal(sum(getSigningBonusSea(tempEmp5)$signingBonus), 4186)
})

test_that("getRetentionBonus() works", {
  expect_equal(sum(getRetentionBonus(tempEmp1)$retentionBonus), 0)
  expect_equal(sum(getRetentionBonus(tempEmp2)$retentionBonus), 0)
  expect_equal(sum(getRetentionBonus(tempEmp3)$retentionBonus), 0)
  expect_equal(sum(getRetentionBonus(tempEmp4)$retentionBonus), 16744)
  expect_equal(sum(getRetentionBonus(tempEmp5)$retentionBonus), 16744)
  expect_equal(sum(getRetentionBonus(tempEmp6)$retentionBonus), 13754)
  expect_equal(sum(getRetentionBonus(tempEmp7)$retentionBonus), 0)
})

rm(list = ls())
