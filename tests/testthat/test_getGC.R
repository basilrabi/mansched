library(mansched)

ID          <- "S-240"
name        <- "Basil Eric Rabi"
designation <- "Materials Engineering Supervisor"
costCode    <- "14000"
status      <- "reg"
cBegin      <- "2012-10-15"
inHouse     <- TRUE
restday     <- "Tuesday"
hol         <- getHol(hol = holidays, year = 2018)

tempEmp  <- createEmp(empClass = "divisionmanager")
tempEmp1 <- initREmployee(theObject   = tempEmp,
                          ID          = ID,
                          name        = name,
                          designation = designation,
                          costCode    = costCode,
                          status      = status,
                          cBegin      = cBegin,
                          inHouse     = inHouse,
                          restday     = restday,
                          hol         = hol)

cEnd   <- "2018-11-01"
inHouse  <- FALSE
tempEmp  <- createEmp(empClass = "groupmanager")
tempEmp2 <- initREmployee(theObject   = tempEmp,
                          ID          = ID,
                          name        = name,
                          designation = designation,
                          costCode    = costCode,
                          status      = status,
                          cBegin      = cBegin,
                          cEnd        = cEnd,
                          inHouse     = inHouse,
                          restday     = restday,
                          hol         = hol)

cBegin   <- "2018-03-01"
inHouse  <- FALSE
status   <- "pro"
tempEmp  <- createEmp(empClass = "clerk")
tempEmp3 <- initREmployee(theObject   = tempEmp,
                          ID          = ID,
                          name        = name,
                          designation = designation,
                          costCode    = costCode,
                          status      = status,
                          cBegin      = cBegin,
                          inHouse     = inHouse,
                          restday     = restday,
                          hol         = hol,
                          RF          = FALSE)

cBegin   <- "2018-04-01"
inHouse  <- TRUE
status   <- "sea"
tempEmp  <- createEmp(empClass = "technical")
tempEmp4 <- initREmployee(theObject   = tempEmp,
                          ID          = ID,
                          name        = name,
                          designation = designation,
                          costCode    = costCode,
                          status      = status,
                          cBegin      = cBegin,
                          inHouse     = inHouse,
                          restday     = restday,
                          hol         = hol)

cBegin   <- "2018-01-01"
inHouse   <- TRUE
status    <- "sea"
equipment <- "FL"
tempEmp   <- createEmp(empClass = "operator")
tempEmp5  <- initREmployee(theObject   = tempEmp,
                           ID          = ID,
                           name        = name,
                           designation = designation,
                           costCode    = costCode,
                           status      = status,
                           cBegin      = cBegin,
                           inHouse     = inHouse,
                           restday     = restday,
                           hol         = hol,
                           equipment   = equipment)

test_that("getGC() works", {
  expect_equal(sum(getGC(tempEmp1)$benefits), 1500)
  expect_equal(getGC(tempEmp1)$benefits[12], 1500)
  expect_equal(sum(getGC(tempEmp2)$benefits), 0)
  expect_equal(sum(getGC(tempEmp3)$benefits), 3000)
  expect_equal(getGC(tempEmp3)$benefits[12], 0)
  expect_equal(sum(getGC(tempEmp4)$benefits), 3000)
  expect_equal(getGC(tempEmp4)$benefits[11], 3000)
  expect_equal(sum(getGC(tempEmp5)$benefits), 3000)
  expect_equal(getGC(tempEmp5)$benefits[11], 3000)
})

# Test for forecast

ID          <- "S-240"
name        <- "Basil Eric Rabi"
designation <- "Materials Engineering Supervisor"
costCode    <- "14000"
status      <- "reg"
cBegin      <- "2012-10-15"
inHouse     <- TRUE
restday     <- "Tuesday"
hol         <- getHol(hol = holidays, year = 2018)

tempEmp  <- createEmp(empClass = "divisionmanager")
tempEmp1 <- initREmployee(theObject   = tempEmp,
                          ID          = ID,
                          name        = name,
                          designation = designation,
                          costCode    = costCode,
                          status      = status,
                          cBegin      = cBegin,
                          inHouse     = inHouse,
                          restday     = restday,
                          hol         = hol,
                          forecast    = TRUE)

cBegin   <- "2018-02-01"
inHouse  <- FALSE
tempEmp  <- createEmp(empClass = "groupmanager")
tempEmp2 <- initREmployee(theObject   = tempEmp,
                          ID          = ID,
                          name        = name,
                          designation = designation,
                          costCode    = costCode,
                          status      = status,
                          cBegin      = cBegin,
                          inHouse     = inHouse,
                          restday     = restday,
                          hol         = hol,
                          forecast    = TRUE)

cBegin   <- "2018-03-01"
inHouse  <- FALSE
tempEmp  <- createEmp(empClass = "clerk")
tempEmp3 <- initREmployee(theObject   = tempEmp,
                          ID          = ID,
                          name        = name,
                          designation = designation,
                          costCode    = costCode,
                          status      = status,
                          cBegin      = cBegin,
                          inHouse     = inHouse,
                          restday     = restday,
                          hol         = hol,
                          RF          = FALSE,
                          forecast    = TRUE)

cBegin   <- "2018-04-01"
inHouse  <- TRUE
status   <- "sea"
tempEmp  <- createEmp(empClass = "technical")
tempEmp4 <- initREmployee(theObject   = tempEmp,
                          ID          = ID,
                          name        = name,
                          designation = designation,
                          costCode    = costCode,
                          status      = status,
                          cBegin      = cBegin,
                          inHouse     = inHouse,
                          restday     = restday,
                          hol         = hol,
                          forecast    = TRUE)

inHouse   <- TRUE
status    <- "sea"
equipment <- "FL"
tempEmp   <- createEmp(empClass = "operator")
tempEmp5  <- initREmployee(theObject   = tempEmp,
                           ID          = ID,
                           name        = name,
                           designation = designation,
                           costCode    = costCode,
                           status      = status,
                           cBegin      = cBegin,
                           inHouse     = inHouse,
                           restday     = restday,
                           hol         = hol,
                           equipment   = equipment,
                           forecast    = TRUE)

cEnd     <- "2018-09-30"
inHouse  <- TRUE
status   <- "sea"
tempEmp  <- createEmp(empClass = "technical")
tempEmp6 <- initREmployee(theObject   = tempEmp,
                          ID          = ID,
                          name        = name,
                          designation = designation,
                          costCode    = costCode,
                          status      = status,
                          cBegin      = cBegin,
                          cEnd        = cEnd,
                          inHouse     = inHouse,
                          restday     = restday,
                          hol         = hol,
                          forecast    = TRUE)

inHouse  <- TRUE
status   <- "age"
tempEmp  <- createEmp(empClass = "laborer")
tempEmp7 <- initREmployee(theObject   = tempEmp,
                          ID          = ID,
                          name        = name,
                          designation = designation,
                          costCode    = costCode,
                          status      = status,
                          cBegin      = cBegin,
                          cEnd        = cEnd,
                          inHouse     = inHouse,
                          restday     = restday,
                          hol         = hol,
                          forecast    = TRUE)

test_that("getGC() works", {
  expect_equal(sum(getGC(tempEmp1)$benefits), 1500)
  expect_equal(sum(getGC(tempEmp2)$benefits), 1500)
  expect_equal(sum(getGC(tempEmp3)$benefits), 1500)
  expect_equal(sum(getGC(tempEmp4)$benefits), 3000)
  expect_equal(sum(getGC(tempEmp5)$benefits), 3000)
  expect_equal(sum(getGC(tempEmp6)$benefits), 2571.43)
  expect_equal(sum(getGC(tempEmp7)$benefits), 0)
})

rm(list = ls())
