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
                          hol         = hol,
                          dependents  = rep(1L, times = 12))

cBegin   <- "2018-02-01"
inHouse  <- FALSE
tempEmp  <- createEmp(empClass = "sectionhead")
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
                          dependents  = rep(2L, times = 12))

cBegin   <- "2018-03-01"
inHouse  <- FALSE
tempEmp  <- createEmp(empClass = "supervisor")
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
                          dependents  = rep(1L, times = 12))

cBegin   <- "2018-04-01"
inHouse  <- TRUE
tempEmp  <- createEmp(empClass = "clerk")
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
                          dependents  = rep(1L, times = 12),
                          RF          = TRUE)

inHouse   <- TRUE
status    <- "pro"
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
                           dependents  = rep(1L, times = 12))

cEnd     <- "2018-10-31"
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
                          dependents  = rep(1L, times = 12))

cEnd     <- "2018-10-31"
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
                          dependents  = rep(1L, times = 12))

test_that("getGroupLife() works", {
  expect_equal(sum(getGroupLife(tempEmp1)$gl),
               round(13629 / 12, digits = 2) * 12)
  expect_equal(sum(getGroupLife(tempEmp2)$gl),
               round(6815 / 12, digits = 2) * 11)
  expect_equal(sum(getGroupLife(tempEmp3)$gl),
               round(4089 / 12, digits = 2) * 10)
  expect_equal(sum(getGroupLife(tempEmp4)$gl),
               round(2044 / 12, digits = 2) * 9)
  expect_equal(sum(getGroupLife(tempEmp5)$gl), 0)
  expect_equal(sum(getGroupLife(tempEmp6)$gl), 0)
  expect_equal(sum(getGroupLife(tempEmp7)$gl), 0)
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
                          forecast    = TRUE,
                          dependents  = rep(1L, times = 12))

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
                          forecast    = TRUE,
                          dependents  = rep(1L, times = 12))

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
                          forecast    = TRUE,
                          dependents  = rep(1L, times = 12))

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
                          forecast    = TRUE,
                          dependents  = rep(1L, times = 12))

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
                           forecast    = TRUE,
                           dependents  = rep(1L, times = 12))

cEnd     <- "2018-10-31"
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
                          forecast    = TRUE,
                          dependents  = rep(1L, times = 12))

cEnd     <- "2018-10-31"
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
                          forecast    = TRUE,
                          dependents  = rep(1L, times = 12))

test_that("getGroupLife() works", {
  expect_equal(sum(getGroupLife(tempEmp1)$gl),
               round(12980 / 12, digits = 2) * 12)
  expect_equal(sum(getGroupLife(tempEmp2)$gl),
               round(12980 / 12, digits = 2) * 11)
  expect_equal(sum(getGroupLife(tempEmp3)$gl),
               round(3894 / 12, digits = 2) * 10)
  expect_equal(sum(getGroupLife(tempEmp4)$gl), 0)
  expect_equal(sum(getGroupLife(tempEmp5)$gl), 0)
  expect_equal(sum(getGroupLife(tempEmp6)$gl), 0)
  expect_equal(sum(getGroupLife(tempEmp7)$gl), 0)
})

rm(list = ls())
