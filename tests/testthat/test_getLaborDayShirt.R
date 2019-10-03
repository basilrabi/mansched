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

cBegin   <- "2018-06-01"
inHouse  <- TRUE
status   <- "reg"
tempEmp  <- createEmp(empClass = "technical")
tempEmp5 <- initREmployee(theObject   = tempEmp,
                          ID          = ID,
                          name        = name,
                          designation = designation,
                          costCode    = costCode,
                          status      = status,
                          cBegin      = cBegin,
                          inHouse     = inHouse,
                          restday     = restday,
                          hol         = hol)

test_that("getLaborDayShirt() works", {
  expect_equal(sum(getLaborDayShirt(tempEmp1)$benefits), 450)
  expect_equal(sum(getLaborDayShirt(tempEmp1)$benefits[5]), 450)
  expect_equal(sum(getLaborDayShirt(tempEmp2)$benefits), 390)
  expect_equal(sum(getLaborDayShirt(tempEmp2)$benefits[5]), 390)
  expect_equal(sum(getLaborDayShirt(tempEmp3)$benefits), 0)
  expect_equal(sum(getLaborDayShirt(tempEmp3)$benefits[5]), 0)
  expect_equal(sum(getLaborDayShirt(tempEmp4)$benefits), 0)
  expect_equal(sum(getLaborDayShirt(tempEmp4)$benefits[5]), 0)
  expect_equal(sum(getLaborDayShirt(tempEmp5)$benefits), 0)
  expect_equal(sum(getLaborDayShirt(tempEmp5)$benefits[5]), 0)
})

