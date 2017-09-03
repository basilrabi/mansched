library(mansched)

test_that("getLeaveHours() works", {
  expect_equal(getLeaveHours(cBegin = "2017-01-01",
                             year = 2017,
                             status = "reg"),
               240L)
  expect_equal(getLeaveHours(cBegin = "2017-02-01",
                             year = 2017,
                             status = "reg"),
               220L)
  expect_equal(getLeaveHours(cBegin = "2017-02-01",
                             year = 2017,
                             status = "pro"),
               40L)
  expect_equal(getLeaveHours(cBegin = "2017-03-01",
                             year = 2017,
                             status = "sea"),
               40L)
  expect_equal(getLeaveHours(cBegin = "1987-02-01",
                             year = 2017,
                             status = "reg"),
               352L)
})
