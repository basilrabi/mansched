library(mansched2)

hol <- getHol(hol = holidays, year = 2018)

mdtProb <- getMDTProb(hol = hol)

test_that("getMDTProb() returns the correct total prob", {
  expect_equal(sum(mdtProb[,c(1:8)]),
               12)
})
