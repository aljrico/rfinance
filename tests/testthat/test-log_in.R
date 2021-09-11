testthat::test_that("log_in creates a connection instance", {
  rfinance::log_in(NULL, NULL)
  testthat::expect_true(exists("rfinanceConnection"))
})

testthat::test_that("log_out removes a connection instance", {
  rfinance::log_in(NULL, NULL)
  rfinance::log_out()
  testthat::expect_false(exists("rfinanceConnection"))
})
