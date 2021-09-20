library(mansched)

z <- c("01400", "1400", "C00", "CCC", "14E")
y <- rmLead0(z)

test_that("rmLead0 works", {
  expect_equal(y[1], "1400")
  expect_equal(y[2], "1400")
  expect_equal(y[3], "C00")
  expect_equal(y[4], "CCC")
  expect_equal(y[5], "14E")
})
