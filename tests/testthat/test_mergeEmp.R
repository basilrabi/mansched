library(mansched)

spareFactor <- 1
calDays <- getCalDays(cBegin = "2018-01-01",
                      hol = getHol(hol = holidays, year = "2018"),
                      restday = "Sunday")

mdtProb <- getMDTProb(hol = getHol(hol = holidays, year = 2018))

testDays <- c(25, 24, 27, 22, 25, 23, 25, 25, 24, 26, 24, 24)

empListA <- list()
empListB <- list()
empListC <- list()
empListD <- list()
empListE <- list()
empListF <- list()
empListG <- list()
empListH <- list()
empListI <- list()

empListA[[1]] <- createEmp("divisionmanager")
empListA[[2]] <- createEmp("departmentmanager")

empListB[[1]] <- createEmp("sectionhead")
empListB[[2]] <- createEmp("sectionhead")
empListB[[3]] <- createEmp("sectionhead")

empListB[[1]] <- initTEmployee(theObject = empListB[[1]],
                               ID = "E1",
                               costCode = "C01",
                               calDays = calDays)
empListB[[2]] <- initTEmployee(theObject = empListB[[2]],
                               ID = "E1",
                               costCode = "C01",
                               calDays = calDays)
empListB[[3]] <- initTEmployee(theObject = empListB[[3]],
                               ID = "E1",
                               costCode = "C02",
                               calDays = calDays)

empListC[[1]] <- createEmp("sectionhead")
empListC[[2]] <- createEmp("sectionhead")
empListC[[3]] <- createEmp("sectionhead")

empListC[[1]] <- initTEmployee(theObject = empListC[[1]],
                               ID = "E1",
                               costCode = "C01",
                               calDays = calDays)
empListC[[2]] <- initTEmployee(theObject = empListC[[2]],
                               ID = "E1",
                               costCode = "C01",
                               calDays = calDays)
empListC[[3]] <- initTEmployee(theObject = empListC[[3]],
                               ID = "E1",
                               costCode = "C01",
                               calDays = calDays)

empMergedC <- mergeEmp(empListC)

empListD[[1]] <- createEmp("supervisor")
empListD[[2]] <- createEmp("supervisor")
empListD[[3]] <- createEmp("supervisor")

empListD[[1]] <- initTEmployee(theObject = empListD[[1]],
                               ID = "E1",
                               costCode = "C01",
                               mdtProb = mdtProb)
empListD[[2]] <- initTEmployee(theObject = empListD[[2]],
                               ID = "E1",
                               costCode = "C01",
                               mdtProb = mdtProb)
empListD[[3]] <- initTEmployee(theObject = empListD[[3]],
                               ID = "E1",
                               costCode = "C02",
                               mdtProb = mdtProb)

empListD[[1]] <- createEmp("supervisor")
empListD[[2]] <- createEmp("supervisor")
empListD[[3]] <- createEmp("supervisor")
empListE[[1]] <- createEmp("supervisor")
empListE[[2]] <- createEmp("supervisor")
empListE[[3]] <- createEmp("supervisor")


empListD[[1]] <- initTEmployee(theObject = empListD[[1]],
                               ID = "E1",
                               costCode = "C01",
                               mdtProb = mdtProb)
empListD[[2]] <- initTEmployee(theObject = empListD[[2]],
                               ID = "E1",
                               costCode = "C01",
                               mdtProb = mdtProb)
empListD[[3]] <- initTEmployee(theObject = empListD[[3]],
                               ID = "E1",
                               costCode = "C02",
                               mdtProb = mdtProb)

empListE[[1]] <- initTEmployee(theObject = empListE[[1]],
                               ID = "E1",
                               costCode = "C01",
                               mdtProb = mdtProb)
empListE[[2]] <- initTEmployee(theObject = empListE[[2]],
                               ID = "E1",
                               costCode = "C01",
                               mdtProb = mdtProb)
empListE[[3]] <- initTEmployee(theObject = empListE[[3]],
                               ID = "E1",
                               costCode = "C01",
                               mdtProb = mdtProb)

empListF[[1]] <- createEmp("operator")
empListF[[2]] <- createEmp("operator")
empListF[[3]] <- createEmp("operator")
empListG[[1]] <- createEmp("operator")
empListG[[2]] <- createEmp("operator")
empListG[[3]] <- createEmp("operator")
empListH[[1]] <- createEmp("operator")
empListH[[2]] <- createEmp("operator")
empListH[[3]] <- createEmp("operator")
empListI[[1]] <- createEmp("operator")
empListI[[2]] <- createEmp("operator")
empListI[[3]] <- createEmp("operator")

empListF[[1]] <- initTEmployee(theObject = empListF[[1]],
                               ID = "E1",
                               costCode = "C01",
                               equipment = "DT",
                               mdtProb = mdtProb)
empListF[[2]] <- initTEmployee(theObject = empListF[[2]],
                               ID = "E1",
                               equipment = "DT",
                               costCode = "C01",
                               mdtProb = mdtProb)
empListF[[3]] <- initTEmployee(theObject = empListF[[3]],
                               ID = "E1",
                               costCode = "C02",
                               equipment = "DT",
                               mdtProb = mdtProb)

empListG[[1]] <- initTEmployee(theObject = empListG[[1]],
                               ID = "E1",
                               costCode = "C01",
                               equipment = "DT",
                               mdtProb = mdtProb)
empListG[[2]] <- initTEmployee(theObject = empListG[[2]],
                               ID = "E1",
                               equipment = "DT",
                               costCode = "C01",
                               mdtProb = mdtProb)
empListG[[3]] <- initTEmployee(theObject = empListG[[3]],
                               ID = "E1",
                               costCode = "C01",
                               equipment = "TX",
                               mdtProb = mdtProb)

empListH[[1]] <- initTEmployee(theObject = empListH[[1]],
                               ID = "E1",
                               costCode = "C01",
                               equipment = "DT",
                               mdtProb = mdtProb)
empListH[[2]] <- initTEmployee(theObject = empListH[[2]],
                               ID = "E1",
                               equipment = "DT",
                               costCode = "C01",
                               mdtProb = mdtProb)
empListH[[3]] <- initTEmployee(theObject = empListH[[3]],
                               ID = "E1",
                               costCode = "C02",
                               equipment = "TX",
                               mdtProb = mdtProb)

empListI[[1]] <- initTEmployee(theObject = empListI[[1]],
                               ID = "E1",
                               costCode = "C01",
                               equipment = "DT",
                               mdtProb = mdtProb)
empListI[[2]] <- initTEmployee(theObject = empListI[[2]],
                               ID = "E1",
                               equipment = "DT",
                               costCode = "C01",
                               mdtProb = mdtProb)
empListI[[3]] <- initTEmployee(theObject = empListI[[3]],
                               ID = "E1",
                               costCode = "C01",
                               equipment = "DT",
                               mdtProb = mdtProb)

test_that("mergeEmp() works", {
  expect_error(mergeEmp(empListA))
  expect_error(mergeEmp(empListB))
  expect_equal(sum(getHours(empMergedC)),
               sum(calDays$reg * 8) * length(empListC))
  expect_equal(empMergedC@ID, "SectionHead-C01")
  expect_error(mergeEmp(empListD))
  expect_equal(sum(getHours(mergeEmp(empListE))), getHoursL(empListE))
  expect_equal(mergeEmp(empListE)@ID, "Supervisor-C01")
  expect_equal(mergeEmp(empListE)@costCode, "C01")
  expect_error(mergeEmp(empListF))
  expect_error(mergeEmp(empListG))
  expect_error(mergeEmp(empListH))
  expect_equal(sum(getHours(mergeEmp(empListI))), getHoursL(empListI))
  expect_equal(mergeEmp(empListI)@ID, "Operator-C01-DT")
  expect_equal(mergeEmp(empListI)@costCode, "C01")
})

rm(list = ls())
