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
calDays     <- getCalDays(cBegin  = cBegin,
                          hol     = hol,
                          restday = restday)

tempEmp  <- createEmp(empClass = "division manager")
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

cBegin   <- "2018-02-01"
inHouse  <- FALSE
tempEmp  <- createEmp(empClass = "group manager")
tempEmp2 <- initREmployee(theObject   = tempEmp,
                          ID          = ID,
                          name        = name,
                          designation = designation,
                          costCode    = costCode,
                          status      = status,
                          cBegin      = cBegin,
                          inHouse     = inHouse,
                          restday     = restday,
                          hol         = hol)

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
                          hol         = hol)


test_that("getRiceSub() works", {
  expect_equal(sum(getRiceSub(tempEmp1)$riceSub), 2200 * 12)
  expect_equal(sum(getRiceSub(tempEmp2)$riceSub), 2200 * 11)
  expect_equal(sum(getRiceSub(tempEmp3)$riceSub), 2200 * 10)
  expect_equal(sum(getRiceSub(tempEmp4)$riceSub), 2200 * 9)
  expect_equal(sum(getRiceSub(tempEmp5)$riceSub), 2200 * 9)
  expect_equal(sum(getRiceSub(tempEmp6)$riceSub), 2200 * 7)
})

rm(list = ls())
