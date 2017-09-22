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
                         d.rd = 5,
                         d.ho = 5,
                         d.rh = 5)

test_that("division_manager() works", {
  expect_equal(class(tempEmp)[1], "Division Manager")
  expect_equivalent(tempEmp@maxReg / 8, calDays[, "reg"])
  expect_equal(isRF(tempEmp), FALSE)
  expect_equal(tempEmp@leaveHours, 256L)
  expect_equal(isReg(tempEmp), TRUE)
  expect_equal(sum(getHours(tempEmp)) / 8, sum(calDays[, "reg"]))
  expect_equal(tempEmp@holHours / 8,
               apply(calDays[, colnames(calDays) %in% c("sh",
                                                        "lh",
                                                        "nh")],
                     MARGIN = 1, FUN = sum))
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
                         hol = hol,
                         d.rd = 5,
                         d.ho = 5,
                         d.rh = 5)

test_that("group_manager() works", {
  expect_equal(class(tempEmp)[1], "Group Manager")
  expect_equivalent(tempEmp@maxReg / 8, calDays[, "reg"])
  expect_equal(isRF(tempEmp), FALSE)
  expect_equal(tempEmp@leaveHours, 256L)
  expect_equal(isReg(tempEmp), TRUE)
  expect_equal(sum(getHours(tempEmp)) / 8, sum(calDays[, "reg"]))
  expect_equal(tempEmp@holHours / 8,
               apply(calDays[, colnames(calDays) %in% c("sh",
                                                        "lh",
                                                        "nh")],
                     MARGIN = 1, FUN = sum))
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
                         hol = hol,
                         d.rd = 5,
                         d.ho = 5,
                         d.rh = 5)

test_that("department_manager() works", {
  expect_equal(class(tempEmp)[1], "Department Manager")
  expect_equivalent(tempEmp@maxReg / 8, calDays[, "reg"])
  expect_equal(isRF(tempEmp), FALSE)
  expect_equal(tempEmp@leaveHours, 256L)
  expect_equal(isReg(tempEmp), TRUE)
  expect_equal(sum(getHours(tempEmp)) / 8, sum(calDays[, "reg"]))
  expect_equal(tempEmp@holHours / 8,
               apply(calDays[, colnames(calDays) %in% c("sh",
                                                        "lh",
                                                        "nh")],
                     MARGIN = 1, FUN = sum))
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
                         hol = hol,
                         d.rd = 5,
                         d.ho = 5,
                         d.rh = 5)

test_that("section_head() works", {
  expect_equal(class(tempEmp)[1], "Section Head")
  expect_equivalent(tempEmp@maxReg / 8, calDays[, "reg"])
  expect_equal(isRF(tempEmp), FALSE)
  expect_equal(tempEmp@leaveHours, 256L)
  expect_equal(isReg(tempEmp), TRUE)
  expect_equal(sum(getHours(tempEmp)) / 8, sum(calDays[, "reg"]))
  expect_equal(tempEmp@holHours / 8,
               apply(calDays[, colnames(calDays) %in% c("sh",
                                                        "lh",
                                                        "nh")],
                     MARGIN = 1, FUN = sum))
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
                         hol = hol,
                         d.rd = 5,
                         d.ho = 5,
                         d.rh = 5)

test_that("clerk() works for regular non-RF", {
  expect_equal(class(tempEmp)[1], "Clerk")
  expect_equivalent(tempEmp@maxReg / 8, calDays[, "reg"])
  expect_equivalent(tempEmp@regOT, (tempEmp@maxReg / 8) * 3)
  expect_equal(isRF(tempEmp), FALSE)
  expect_equal(tempEmp@leaveHours, 256L)
  expect_equal(isReg(tempEmp), TRUE)
  expect_equal(sum(getHours(tempEmp)) / 11, sum(calDays[, "reg"]))
  expect_equal(tempEmp@holHours / 8,
               apply(calDays[, colnames(calDays) %in% c("sh",
                                                        "lh",
                                                        "nh")],
                     MARGIN = 1, FUN = sum))
})

tempEmp <- createEmp(empClass = "clerk")
tempEmp <- initREmployee(theObject = tempEmp,
                         ID = ID,
                         name = name,
                         designation = designation,
                         costCode = costCode,
                         status = "sea",
                         cBegin = cBegin,
                         inHouse = inHouse,
                         restday = restday,
                         hol = hol,
                         d.rd = 5,
                         d.ho = 5,
                         d.rh = 5)

test_that("clerk() works for seasonal non-RF", {
  expect_equal(class(tempEmp)[1], "Clerk")
  expect_equivalent(tempEmp@maxReg / 8,
                    apply(calDays[, colnames(calDays) %in% c("reg", "nh")],
                          MARGIN = 1,
                          FUN = sum))
  expect_equivalent(tempEmp@regOT, (tempEmp@maxReg / 8) * 3)
  expect_equal(isRF(tempEmp), FALSE)
  expect_equal(tempEmp@leaveHours, 40L)
  expect_equal(isReg(tempEmp), FALSE)
  expect_equal(sum(getHours(tempEmp)) / 11,
               sum(calDays[, colnames(calDays) %in% c("reg",
                                                      "nh")]))
  expect_equal(tempEmp@holHours / 8,
               apply(calDays[, colnames(calDays) %in% c("lh","sh")],
                     MARGIN = 1,
                     FUN = sum))
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
                         hol = hol,
                         RF = TRUE,
                         d.rd = 5,
                         d.ho = 5,
                         d.rh = 5)

test_that("clerk() works for regular RF", {
  expect_equal(class(tempEmp)[1], "Clerk")
  expect_equivalent(tempEmp@maxReg / 8, calDays[, "reg"])
  expect_equivalent(tempEmp@regOT, (tempEmp@maxReg / 8) * 3)
  expect_equal(isRF(tempEmp), TRUE)
  expect_equal(tempEmp@leaveHours, 256L)
  expect_equal(isReg(tempEmp), TRUE)
  expect_equal(sum(getHours(tempEmp)) / 11, sum(calDays[, "reg"]))
  expect_equal(tempEmp@holHours / 8,
               apply(calDays[, colnames(calDays) %in% c("sh",
                                                        "lh",
                                                        "nh",
                                                        "rs",
                                                        "rl",
                                                        "rn")],
                     MARGIN = 1, FUN = sum))
})

tempEmp <- createEmp(empClass = "clerk")
tempEmp <- initREmployee(theObject = tempEmp,
                         ID = ID,
                         name = name,
                         designation = designation,
                         costCode = costCode,
                         status = "sea",
                         cBegin = cBegin,
                         inHouse = inHouse,
                         restday = restday,
                         hol = hol,
                         RF = TRUE,
                         d.rd = 5,
                         d.ho = 5,
                         d.rh = 5)

test_that("clerk() works for seasonal RF", {
  expect_equal(class(tempEmp)[1], "Clerk")
  expect_equivalent(tempEmp@maxReg / 8,
                    apply(calDays[, colnames(calDays) %in% c("reg", "nh")],
                          MARGIN = 1,
                          FUN = sum))
  expect_equivalent(tempEmp@regOT, (tempEmp@maxReg / 8) * 3)
  expect_equal(isRF(tempEmp), TRUE)
  expect_equal(tempEmp@leaveHours, 40L)
  expect_equal(isReg(tempEmp), FALSE)
  expect_equal(sum(getHours(tempEmp)) / 11,
               sum(calDays[, colnames(calDays) %in% c("reg",
                                                      "nh")]))
  expect_equal(tempEmp@holHours / 8,
               apply(calDays[, colnames(calDays) %in% c("lh",
                                                        "rl")],
                     MARGIN = 1, FUN = sum))
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
                         hol = hol,
                         d.rd = 5,
                         d.ho = 5,
                         d.rh = 5)

test_that("technical() works", {
  expect_equal(class(tempEmp)[1], "Technical")
  expect_equivalent(tempEmp@maxReg / 8, calDays[, "reg"])
  expect_equal(isRF(tempEmp), FALSE)
  expect_equal(tempEmp@leaveHours, 256L)
  expect_equal(isReg(tempEmp), TRUE)
  expect_equal(sum(getHours(tempEmp)) / 11, sum(calDays))
  expect_equal(tempEmp@holHours / 8,
               apply(calDays[, colnames(calDays) %in% c("sh",
                                                        "lh",
                                                        "nh")],
                     MARGIN = 1, FUN = sum))
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
                         hol = hol,
                         d.rd = 5,
                         d.ho = 5,
                         d.rh = 5)

test_that("supervisor() works for regular", {
  expect_equal(class(tempEmp)[1], "Supervisor")
  expect_equivalent(tempEmp@maxReg / 8, calDays[, "reg"])
  expect_equal(isRF(tempEmp), FALSE)
  expect_equal(tempEmp@leaveHours, 256L)
  expect_equal(isReg(tempEmp), TRUE)
  expect_equal(sum(getHours(tempEmp)) / 11, sum(calDays))
  expect_equal(tempEmp@holHours / 8,
               apply(calDays[, colnames(calDays) %in% c("sh",
                                                        "lh",
                                                        "nh")],
                     MARGIN = 1, FUN = sum))
})

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
                         hol = hol,
                         d.rd = 5,
                         d.ho = 5,
                         d.rh = 5)

test_that("laborer() works for regular", {
  expect_equal(class(tempEmp)[1], "Laborer")
  expect_equivalent(tempEmp@maxReg / 8, calDays[, "reg"])
  expect_equal(isRF(tempEmp), TRUE)
  expect_equal(tempEmp@leaveHours, 256L)
  expect_equal(isReg(tempEmp), TRUE)
  expect_equal(sum(getHours(tempEmp)) / 11, sum(calDays))
  expect_equal(tempEmp@holHours / 8,
               apply(calDays[, colnames(calDays) %in% c("sh",
                                                        "lh",
                                                        "nh",
                                                        "rs",
                                                        "rl",
                                                        "rn")],
                     MARGIN = 1, FUN = sum))
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
                         equipment = equipment,
                         d.rd = 5,
                         d.ho = 5,
                         d.rh = 5)

test_that("operator() works for regular", {
  expect_equal(class(tempEmp)[1], "Operator")
  expect_equivalent(tempEmp@maxReg / 8, calDays[, "reg"])
  expect_equal(isRF(tempEmp), TRUE)
  expect_equal(tempEmp@leaveHours, 256L)
  expect_equal(isReg(tempEmp), TRUE)
  expect_equal(sum(getHours(tempEmp)) / 11, sum(calDays))
  expect_equal(tempEmp@holHours / 8,
               apply(calDays[, colnames(calDays) %in% c("sh",
                                                        "lh",
                                                        "nh",
                                                        "rs",
                                                        "rl",
                                                        "rn")],
                     MARGIN = 1, FUN = sum))
})

status <- "sea"

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
                         hol = hol,
                         d.rd = 5,
                         d.ho = 5,
                         d.rh = 5)

test_that("supervisor() works for seasonal", {
  expect_equal(class(tempEmp)[1], "Supervisor")
  expect_equivalent(tempEmp@maxReg / 8,
                    apply(calDays[, colnames(calDays) %in% c("reg", "nh")],
                          MARGIN = 1,
                          FUN = sum))
  expect_equal(isRF(tempEmp), FALSE)
  expect_equal(tempEmp@leaveHours, 40L)
  expect_equal(isReg(tempEmp), FALSE)
  expect_equal(sum(getHours(tempEmp)) / 11, sum(calDays))
  expect_equal(tempEmp@holHours / 8,
               apply(calDays[, colnames(calDays) %in% c("lh","sh")],
                     MARGIN = 1,
                     FUN = sum))
})

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
                         hol = hol,
                         d.rd = 5,
                         d.ho = 5,
                         d.rh = 5)

test_that("laborer() works for seasonal", {
  expect_equal(class(tempEmp)[1], "Laborer")
  expect_equivalent(tempEmp@maxReg / 8,
                    apply(calDays[, colnames(calDays) %in% c("reg", "nh")],
                          MARGIN = 1,
                          FUN = sum))
  expect_equal(isRF(tempEmp), TRUE)
  expect_equal(tempEmp@leaveHours, 40L)
  expect_equal(isReg(tempEmp), FALSE)
  expect_equal(sum(getHours(tempEmp)) / 11, sum(calDays))
  expect_equal(tempEmp@holHours / 8,
               apply(calDays[, colnames(calDays) %in% c("lh",
                                                        "rl")],
                     MARGIN = 1, FUN = sum))
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
                         equipment = equipment,
                         d.rd = 5,
                         d.ho = 5,
                         d.rh = 5)

test_that("operator() works for seasonal", {
  expect_equal(class(tempEmp)[1], "Operator")
  expect_equivalent(tempEmp@maxReg / 8,
                    apply(calDays[, colnames(calDays) %in% c("reg", "nh")],
                          MARGIN = 1,
                          FUN = sum))
  expect_equal(isRF(tempEmp), TRUE)
  expect_equal(tempEmp@leaveHours, 40L)
  expect_equal(isReg(tempEmp), FALSE)
  expect_equal(sum(getHours(tempEmp)) / 11, sum(calDays))
  expect_equal(tempEmp@holHours / 8,
               apply(calDays[, colnames(calDays) %in% c("lh",
                                                        "rl")],
                     MARGIN = 1, FUN = sum))
})
